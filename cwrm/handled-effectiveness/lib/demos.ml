open! Core
open Handled_effect
open Types

(** Instrumented [handled_effect] demos. Each demo runs to completion under a tracing
    handler and returns a scrubbable [Demo.t] for the TUI. *)

module Trace_builder : sig
  type t

  val create : unit -> t
  val console : t -> string
  val ready : t -> string list
  val set_ready : t -> string list -> unit
  val set_running : t -> string option -> unit
  val set_state_panel : t -> string -> unit

  val record
    :  t
    -> event:Event.t
    -> explanation:string
    -> unit

  val frames : t -> Frame.t list
end = struct
  type t =
    { mutable step : int
    ; mutable console : string
    ; mutable ready : string list
    ; mutable running : string option
    ; mutable state_panel : string
    ; mutable frames : Frame.t list
    }

  let create () =
    { step = 0
    ; console = ""
    ; ready = []
    ; running = None
    ; state_panel = ""
    ; frames = []
    }
  ;;

  let console t = t.console
  let ready t = t.ready
  let set_ready t ready = t.ready <- ready
  let set_running t running = t.running <- running
  let set_state_panel t s = t.state_panel <- s

  let record t ~event ~explanation =
    (match event with
     | Event.Say { text; _ } -> t.console <- t.console ^ text
     | Event.Yield_char { char } -> t.console <- t.console ^ Char.to_string char
     | _ -> ());
    let frame =
      { Frame.step = t.step
      ; event
      ; explanation
      ; ready = t.ready
      ; running = t.running
      ; console = t.console
      ; state_panel = t.state_panel
      }
    in
    t.frames <- frame :: t.frames;
    t.step <- t.step + 1
  ;;

  let frames t = List.rev t.frames
end

(* -------------------------------------------------------------------------- *)
(* Demo 1: shallow mutable state via Get / Set                                 *)
(* -------------------------------------------------------------------------- *)

module State_demo = struct
  type 'a op =
    | Get : int op
    | Set : int -> unit op

  module Eff = Handled_effect.Make (struct
      type 'a t = 'a op
    end)

  let handle_state (trace : Trace_builder.t) init f =
    let rec handle (state : int) = function
      | Eff.Value result ->
        Trace_builder.set_state_panel trace (sprintf "state = %d  (done)" state);
        Trace_builder.set_running trace None;
        Trace_builder.record
          trace
          ~event:Finished
          ~explanation:(sprintf "computation returned; final state %d" state);
        result, state
      | Eff.Exception e -> raise e
      | Eff.Operation (Get, k) ->
        Trace_builder.set_state_panel trace (sprintf "state = %d" state);
        Trace_builder.record
          trace
          ~event:(Get { value = state })
          ~explanation:(sprintf "handler answers Get with %d" state);
        handle state (continue k state [])
      | Eff.Operation (Set new_state, k) ->
        Trace_builder.set_state_panel trace (sprintf "state = %d" new_state);
        Trace_builder.record
          trace
          ~event:(Set { old_value = state; new_value = new_state })
          ~explanation:(sprintf "handler updates state %d -> %d" state new_state);
        handle new_state (continue k () [])
    in
    Trace_builder.set_state_panel trace (sprintf "state = %d" init);
    Trace_builder.set_running trace (Some "comp");
    Trace_builder.record
      trace
      ~event:(Resume { fiber = "comp" })
      ~explanation:(sprintf "install state handler with initial state %d" init);
    handle init (Eff.run f)
  ;;

  let computation h =
    let x = Eff.perform h Get in
    Eff.perform h (Set (x + 10));
    let y = Eff.perform h Get in
    Eff.perform h (Set (y * 2));
    ignore (Eff.perform h Get : int)
  ;;

  let demo () =
    let trace = Trace_builder.create () in
    let (), _final = handle_state trace 7 computation in
    { Demo.id = "state"
    ; title = "Shallow state (Get / Set)"
    ; blurb =
        "A single handler interprets Get/Set operations, threading an int through \
         continue. The computation never sees the state cell - only the handler does."
    ; frames = Trace_builder.frames trace
    }
  ;;
end

(* -------------------------------------------------------------------------- *)
(* Demo 2: Send / Recv session protocol                                        *)
(* -------------------------------------------------------------------------- *)

module Protocol_demo = struct
  type 'a op =
    | Send : int -> unit op
    | Recv : int op

  module Eff = Handled_effect.Make (struct
      type 'a t = 'a op
    end)

  let run (trace : Trace_builder.t) comp =
    let set_phase phase buffered =
      Trace_builder.set_state_panel
        trace
        (match buffered with
         | None -> sprintf "phase = %s" phase
         | Some n -> sprintf "phase = %s\nbuffered = %d" phase n)
    in
    let rec handle_send = function
      | Eff.Value x ->
        set_phase "done" None;
        Trace_builder.set_running trace None;
        Trace_builder.record
          trace
          ~event:Finished
          ~explanation:"protocol completed successfully";
        x
      | Eff.Exception e -> raise e
      | Eff.Operation (Send n, k) ->
        set_phase "recv" (Some n);
        Trace_builder.record
          trace
          ~event:(Send { value = n })
          ~explanation:
            (sprintf "Send %d - handler buffers the value and waits for Recv" n);
        handle_recv n (continue k () [])
      | Eff.Operation (Recv, _) -> failwith "protocol violation: Recv while expecting Send"
    and handle_recv n = function
      | Eff.Value x ->
        set_phase "done" None;
        Trace_builder.set_running trace None;
        Trace_builder.record
          trace
          ~event:Finished
          ~explanation:"protocol completed successfully";
        x
      | Eff.Exception e -> raise e
      | Eff.Operation (Recv, k) ->
        set_phase "send" None;
        Trace_builder.record
          trace
          ~event:(Recv { value = n })
          ~explanation:(sprintf "Recv - handler delivers buffered %d" n);
        handle_send (continue k n [])
      | Eff.Operation (Send _, _) ->
        failwith "protocol violation: Send while expecting Recv"
    in
    Trace_builder.set_running trace (Some "session");
    set_phase "send" None;
    Trace_builder.record
      trace
      ~event:(Resume { fiber = "session" })
      ~explanation:"start in Send phase (client must Send before Recv)";
    handle_send (Eff.run comp)
  ;;

  let computation h =
    Eff.perform h (Send 42);
    let a = Eff.perform h Recv in
    Eff.perform h (Send (a + 1));
    ignore (Eff.perform h Recv : int)
  ;;

  let demo () =
    let trace = Trace_builder.create () in
    let () = run trace computation in
    { Demo.id = "protocol"
    ; title = "Send / Recv protocol"
    ; blurb =
        "Two mutually recursive handlers enforce a session type: Send then Recv, \
         repeatedly. A wrong operation is a protocol violation."
    ; frames = Trace_builder.frames trace
    }
  ;;
end

(* -------------------------------------------------------------------------- *)
(* Demo 3: iterator inversion (generator via Yield)                            *)
(* -------------------------------------------------------------------------- *)

module Generator_demo = struct
  type ('a, 'p) op = Yield : 'p -> (unit, 'p) op

  module Eff = Handled_effect.Make1 (struct
      type ('a, 'p) t = ('a, 'p) op
    end)

  let invert (trace : Trace_builder.t) ~(iter : local_ (char -> unit) -> unit) =
    let rec handle = function
      | Eff.Value () ->
        Trace_builder.set_running trace None;
        Trace_builder.set_state_panel trace "generator exhausted";
        Trace_builder.record
          trace
          ~event:Finished
          ~explanation:"iterator finished - generator is done"
      | Eff.Exception e -> raise e
      | Eff.Operation (Yield c, k) ->
        Trace_builder.set_state_panel trace (sprintf "yielded '%c'" c);
        Trace_builder.record
          trace
          ~event:(Yield_char { char = c })
          ~explanation:
            (sprintf "Yield '%c' - suspend iterator, produce next generator element" c);
        handle (continue k () [])
    in
    Trace_builder.set_running trace (Some "iter");
    Trace_builder.set_state_panel trace "inverting iterator -> generator";
    Trace_builder.record
      trace
      ~event:(Resume { fiber = "iter" })
      ~explanation:"run the iterator under a Yield handler (classic invert pattern)";
    handle (Eff.run (fun h -> iter (fun c -> Eff.perform h (Yield c)) [@nontail]))
  ;;

  let demo () =
    let trace = Trace_builder.create () in
    let () =
      invert trace ~iter:(fun yield ->
        String.iter "OxCaml" ~f:yield)
    in
    { Demo.id = "generator"
    ; title = "Iterator -> generator"
    ; blurb =
        "Effect Yield turns a push-style iterator into a pull-style generator. Each \
         Yield suspends the iterator and delivers one character."
    ; frames = Trace_builder.frames trace
    }
  ;;
end

(* -------------------------------------------------------------------------- *)
(* Demo 4: cooperative round-robin scheduler                                   *)
(* -------------------------------------------------------------------------- *)

module Scheduler_demo = struct
  module Uniqueue : sig
    type 'a t

    val create : unit -> 'a t
    val push : 'a @ once unique -> 'a t -> unit
    val pop : 'a t -> 'a @ once unique
    val is_empty : 'a t -> bool
  end = struct
    type 'a t = 'a Unique.Once.t Queue.t

    let create () = Queue.create ()
    let push v t = Queue.enqueue t (Unique.Once.make v)
    let pop t = Unique.Once.take_exn (Queue.dequeue_exn t)
    let is_empty t = Queue.is_empty t
  end

  type ('a, 'e) op =
    | Yield : (unit, 'e) op
    | Fork : (local_ 'e Handled_effect.Handler.t -> unit) * string -> (unit, 'e) op
    | Say : string -> (unit, 'e) op

  module Eff = Handled_effect.Make_rec (struct
      type ('a, 'e) t = ('a, 'e) op
    end)

  let yield h = Eff.perform h Yield
  let fork h ~name f = Eff.perform h (Fork (f, name))
  let say h text = Eff.perform h (Say text)

  let run (trace : Trace_builder.t) main =
    let run_q = Uniqueue.create () in
    let name_q = Queue.create () in
    let current = ref "main" in
    let sync_ready () =
      Trace_builder.set_ready trace (Queue.to_list name_q);
      Trace_builder.set_running trace (Some !current);
      Trace_builder.set_state_panel
        trace
        (sprintf
           "scheduler\nrunning: %s\nready:   [%s]"
           !current
           (String.concat ~sep:", " (Queue.to_list name_q)))
    in
    let rec dequeue () =
      if Uniqueue.is_empty run_q
      then (
        Trace_builder.set_ready trace [];
        Trace_builder.set_running trace None;
        Trace_builder.set_state_panel trace "scheduler\nidle - all fibers done";
        Trace_builder.record
          trace
          ~event:Finished
          ~explanation:"ready queue empty; scheduler terminates")
      else (
        let name = Queue.dequeue_exn name_q in
        current := name;
        sync_ready ();
        Trace_builder.record
          trace
          ~event:(Resume { fiber = name })
          ~explanation:(sprintf "dequeue and resume fiber %s" name);
        let task = Uniqueue.pop run_q in
        task ())
    and enqueue
      : type a. string -> (a, _, _) Eff.Continuation.t @ once unique -> a @ unique -> unit
      =
      fun name k v ->
      Queue.enqueue name_q name;
      let task () = handle (continue k v []) in
      Uniqueue.push task run_q
    and spawn ~name f =
      current := name;
      sync_ready ();
      Trace_builder.record
        trace
        ~event:(Resume { fiber = name })
        ~explanation:(sprintf "spawn fiber %s" name);
      handle (Eff.run f)
    and handle = function
      | Eff.Value () ->
        Trace_builder.record
          trace
          ~event:(Complete { fiber = !current })
          ~explanation:(sprintf "fiber %s returned" !current);
        dequeue ()
      | Eff.Exception e ->
        Trace_builder.record
          trace
          ~event:(Complete { fiber = !current })
          ~explanation:(sprintf "fiber %s raised: %s" !current (Exn.to_string e));
        dequeue ()
      | Eff.Operation (Yield, k) ->
        let name = !current in
        Trace_builder.record
          trace
          ~event:(Yield { fiber = name })
          ~explanation:(sprintf "%s yields; continuation enqueued" name);
        enqueue name k ();
        sync_ready ();
        dequeue ()
      | Eff.Operation (Fork (f, child), k) ->
        let parent = !current in
        Trace_builder.record
          trace
          ~event:(Fork { parent; child })
          ~explanation:(sprintf "%s forks %s; parent enqueued, child runs next" parent child);
        enqueue parent k ();
        sync_ready ();
        spawn ~name:child f
      | Eff.Operation (Say text, k) ->
        Trace_builder.record
          trace
          ~event:(Say { fiber = !current; text })
          ~explanation:(sprintf "%s prints %S" !current text);
        handle (continue k () [])
    in
    spawn ~name:"main" main
  ;;

  let computation h =
    fork h ~name:"A" (fun h ->
      say h "A";
      yield h;
      say h "a";
      yield h;
      say h "A3");
    fork h ~name:"B" (fun h ->
      say h "B";
      yield h;
      say h "b");
    say h "M"
  ;;

  let demo () =
    let trace = Trace_builder.create () in
    let () = run trace computation in
    { Demo.id = "scheduler"
    ; title = "Round-robin scheduler"
    ; blurb =
        "Fork / Yield / Say under a cooperative scheduler. Parent continuations are \
         uniquely enqueued (Unique.Once); each Yield rotates the ready queue."
    ; frames = Trace_builder.frames trace
    }
  ;;
end

let all () =
  [ State_demo.demo (); Protocol_demo.demo (); Generator_demo.demo (); Scheduler_demo.demo () ]
;;

let by_index demos i =
  let i = Int.clamp_exn i ~min:0 ~max:(List.length demos - 1) in
  List.nth_exn demos i
;;
