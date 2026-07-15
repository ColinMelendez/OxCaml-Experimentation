open! Core
open! Async_kernel
open! Import
module Id = Unique_id.Int ()

module Control_event = struct
  type t = Set_level of Level.t [@@deriving globalize, sexp_of]
end

(* Mutable state that is only accessed within the Async scheduler. Portable callers use
   [Raw_log.t], but do not inspect or mutate this state directly. *)
module Async_state = struct
  type t =
    { on_error : On_error.t ref
    ; output : Mutable_outputs.t
    ; mutable time_source : Synchronous_time_source.t
    ; transforms : (Message_event.t -> Message_event.t option) Doubly_linked.t
    ; control_events : (Control_event.t @ local -> unit) Bus.Read_write.t
    }
end

type t =
  { id : Id.t
  ; state : Async_state.t aliased Capsule.Initial.Data.t
  ; execution_context : Execution_context.t Capsule.Initial.Data.t
  ; (* These atomics are independent of each other and don't require synchronization for
       updating. *)
    is_closed : bool Atomic.t
  ; level : Level.t Atomic.t
  ; has_output_or_transform : bool Atomic.t
  }

let state t = (Capsule.Initial.Data.unwrap t.state).aliased
let control_events t = Bus.read_only (state t).control_events
let is_closed t = Atomic.get t.is_closed

let assert_open t tag =
  if is_closed t then failwithf "Log: can't %s because this log has been closed" tag ()
;;

(* Publishes a worker-readable summary of whether any output or transform can observe a
   message. Call this after mutating outputs or transforms; the portable [would_log]
   fast-path reads only this atomic summary and does not inspect Async-owner state. *)
let update_has_output_or_transform t =
  let state = state t in
  Atomic.set
    t.has_output_or_transform
    ((not (Mutable_outputs.is_empty state.output))
     || not (Doubly_linked.is_empty state.transforms))
;;

let flushed t =
  (* 2025-10 - We don't [assert_open] here. In a way, it's not necessary, but we also
     found that if you have a log A with an output that writes and flushes to another log
     B, and then stop using both, [flush_and_close] can be called on both logs, but in a
     nondeterministic order. If B is closed before A, then A tries to flush, an
     [assert_open] here would raise. *)
  Mutable_outputs.flushed (state t).output
;;

let flush_and_close t =
  let state = state t in
  if not (is_closed t)
  then (
    let finished = flushed t in
    Atomic.set t.is_closed true;
    upon finished (fun () -> Bus.close state.control_events);
    finished)
  else return ()
;;

let close = flush_and_close

let live_logs =
  lazy
    (Live_entry_registry.create
       (module struct
         type nonrec t = t

         let equal t1 t2 = Id.equal t1.id t2.id
         let hash t = Id.hash t.id
         let flushed = flushed
         let is_closed = is_closed
         let flush_and_close = flush_and_close
       end))
;;

let create ~level ~default_outputs ~named_outputs ~on_error ~time_source ~transforms =
  let time_source =
    match Option.map time_source ~f:Synchronous_time_source.read_only with
    | Some time_source -> time_source
    | None ->
      if Ppx_inline_test_lib.am_running
      then Synchronous_time_source.(read_only (create ~now:Time_ns.epoch ()))
      else Synchronous_time_source.wall_clock ()
  in
  let on_error = ref on_error in
  let output =
    Mutable_outputs.create
      ~default_outputs
      ~named_outputs
      ~on_background_output_error:(fun exn -> On_error.handle_error !on_error exn)
  in
  let id = Id.create () in
  let control_events =
    Bus.create_exn
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:(ignore : Error.t -> unit)
      ()
  in
  let transforms = Doubly_linked.of_list transforms in
  let state = { Async_state.on_error; output; time_source; transforms; control_events } in
  let has_output_or_transform =
    (not (Mutable_outputs.is_empty output)) || not (Doubly_linked.is_empty transforms)
  in
  let t =
    { id
    ; state = { aliased = state } |> Capsule.Initial.Data.wrap
    ; execution_context =
        Async_kernel_scheduler.current_execution_context () |> Capsule.Initial.Data.wrap
    ; is_closed = Atomic.make false
    ; level = Atomic.make level
    ; has_output_or_transform = Atomic.make has_output_or_transform
    }
  in
  Live_entry_registry.register (force live_logs) t;
  t
;;

let set_output t new_outputs =
  assert_open t "set output";
  Mutable_outputs.update_default_outputs (state t).output new_outputs;
  update_has_output_or_transform t
;;

let get_output t = Mutable_outputs.current_default_outputs (state t).output
let get_named_outputs t = Mutable_outputs.current_named_outputs (state t).output
let get_on_error t = !((state t).on_error)
let set_on_error t handler = (state t).on_error := handler
let level t = Atomic.get t.level

let set_level t level =
  let state = state t in
  match Level.equal level (Atomic.get t.level) with
  | true -> ()
  | false ->
    Atomic.set t.level level;
    let local_ control_event = Control_event.Set_level level in
    Bus.write_local state.control_events control_event [@nontail]
;;

let get_time_source t = (state t).time_source

let set_time_source t time_source =
  (state t).time_source <- Synchronous_time_source.read_only time_source
;;

let has_transform t = not (Doubly_linked.is_empty (state t).transforms)

module Transform = struct
  type t = (Message_event.t -> Message_event.t option) Doubly_linked.Elt.t

  let add log f before_or_after =
    let state = state log in
    let transform =
      match before_or_after with
      | `Before -> Doubly_linked.insert_first state.transforms f
      | `After -> Doubly_linked.insert_last state.transforms f
    in
    update_has_output_or_transform log;
    transform
  ;;

  let remove_exn log t =
    (* [Doubly_linked.remove] can raise if the transform is not a part of the log's
       transforms. *)
    Doubly_linked.remove (state log).transforms t;
    update_has_output_or_transform log
  ;;
end

let clear_transforms t =
  Doubly_linked.clear (state t).transforms;
  update_has_output_or_transform t
;;

let transform_message (state : Async_state.t) msg =
  Doubly_linked.fold_until
    state.transforms
    ~init:msg
    ~f:(fun msg transform ->
      match transform msg with
      | Some msg -> Continue msg
      | None -> Stop None)
    ~finish:Option.return
;;

let transform t msg = transform_message (state t) msg

let get_transform t =
  (* This doesn’t use [transform] function above as [transforms] is mutable and this takes
     a snapshot of it *)
  match Doubly_linked.to_list (state t).transforms with
  | [] -> None
  | [ f ] -> Some f
  | fs ->
    Some
      (fun msg ->
        let rec loop fs msg =
          match fs with
          | [] -> Some msg
          | f :: fs ->
            (match f msg with
             | None -> None
             | Some msg -> loop fs msg)
        in
        loop fs msg)
;;

let set_transform t f =
  clear_transforms t;
  match f with
  | None -> ()
  | Some f ->
    let (_ : Transform.t) = Transform.add t f `Before in
    ()
;;

let copy t =
  create
    ~level:(level t)
    ~default_outputs:(get_output t)
    ~named_outputs:(get_named_outputs t)
    ~on_error:(get_on_error t)
    ~time_source:(Some (get_time_source t))
    ~transforms:(Doubly_linked.to_list (state t).transforms)
;;

(* would_log is broken out and tested separately for every sending function to avoid the
   overhead of message allocation when we are just going to drop the message. *)
let would_log t msg_level =
  Atomic.get t.has_output_or_transform
  && Level.as_or_more_verbose_than ~log_level:(Atomic.get t.level) ~msg_level
;;

let write_message_event ~is_closed state msg =
  (* We want to call [transform], even if we don't end up pushing the message to an
     output. This allows for someone to listen to all messages that would theoretically be
     logged by this log (respecting level), and then maybe log them somewhere else. *)
  match transform_message state msg with
  | Some msg ->
    if not (Mutable_outputs.is_empty state.output)
    then (
      if Atomic.get is_closed
      then failwith "Log: can't write message because this log has been closed";
      Mutable_outputs.write state.output msg)
  | None -> ()
;;

let push_message_event t msg = write_message_event ~is_closed:t.is_closed (state t) msg
let write_message_event_wrapped = Capsule.Initial.Data.wrap write_message_event

let enqueue_message_request t data source ~level ~time ~legacy_tags =
  Async_kernel_scheduler.portable_enqueue_job
    t.execution_context
    (Capsule.Data.create
       (fun () #(access, ({ aliased = state } : Async_state.t aliased)) ->
          let write_message_event =
            Capsule.Data.unwrap ~access write_message_event_wrapped
          in
          let time =
            match time with
            | Some time -> time
            | None ->
              (* Default timestamps are filled in the async scheduler. Portable callers
                 may pass an explicit time but cannot read the time source directly. *)
              Synchronous_time_source.now state.time_source
              |> Time_ns.to_time_float_round_nearest
          in
          Message_event.create data ~source ?level ~time ~legacy_tags
          |> write_message_event ~is_closed:t.is_closed state))
    t.state
;;

let all_live_logs_flushed () =
  match Lazy.peek live_logs with
  | Some live_logs -> Live_entry_registry.live_entries_flushed live_logs
  | None -> Deferred.unit
;;

module Private = struct
  let set_named_output t name output =
    assert_open t "set named output";
    Mutable_outputs.set_named_output (state t).output name output;
    update_has_output_or_transform t
  ;;

  let get_named_output t name =
    assert_open t "get named output";
    Mutable_outputs.current_named_outputs (state t).output |> Fn.flip Map.find name
  ;;

  let remove_named_output t name =
    assert_open t "remove named output";
    Mutable_outputs.remove_named_output (state t).output name;
    update_has_output_or_transform t
  ;;

  let with_temporary_outputs t outputs ~f =
    assert_open t "with temporary outputs";
    let original_outputs = get_output t in
    let original_named_outputs = get_named_outputs t in
    Mutable_outputs.update_named_outputs (state t).output Output_name.Map.empty;
    set_output t outputs;
    f ();
    set_output t original_outputs;
    Mutable_outputs.update_named_outputs (state t).output original_named_outputs
  ;;

  module For_testing = struct
    let get_named_outputs = get_named_outputs

    let update_named_outputs t named_outputs =
      assert_open t "update named outputs";
      Mutable_outputs.update_named_outputs (state t).output named_outputs;
      update_has_output_or_transform t
    ;;
  end
end

module For_testing = struct
  let transform = transform
end
