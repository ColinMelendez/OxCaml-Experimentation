open! Core
open Bonsai_term
open Bonsai.Let_syntax
open Types
open Ui_state

module Incoming = struct
  type t =
    | Set_snapshot of Types.Snapshot.t
    | Ui_action of Types.Ui_state.action
  [@@deriving sexp]
end

module Model = struct
  type t =
    { snapshot : Types.Snapshot.t
    ; ui : Types.Ui_state.t
    }
  [@@deriving sexp, equal]
end

type result =
  { view : View.t
  ; handler : Event.t -> unit Effect.t
  ; model : Model.t
  ; inject : Incoming.t -> unit Effect.t
  }

let empty_snapshot : Types.Snapshot.t =
  { hostname = "localhost"
  ; ncpu = 1
  ; loadavg = { one = 0.; five = 0.; fifteen = 0. }
  ; memory = { total_bytes = 0L; used_bytes = 0L }
  ; cpu_pct = 0.
  ; cpu_history = []
  ; processes = []
  }
;;

let visible_rows ~dimensions =
  let { Dimensions.height; _ } = dimensions in
  let { Render.process_total; _ } = Render.layout_heights ~total_height:height in
  (* process_total includes border (2) + column header (1). *)
  Int.max 1 (process_total - 3)
;;

let apply_incoming (model : Model.t) (incoming : Incoming.t) ~dimensions =
  match incoming with
  | Set_snapshot snapshot ->
    let visible_rows = visible_rows ~dimensions in
    let ui =
      Types.Ui_state.apply
        model.ui
        (Clamp_to { num_processes = List.length snapshot.processes; visible_rows })
        ~num_processes:(List.length snapshot.processes)
        ~visible_rows
    in
    { Model.snapshot; ui }
  | Ui_action action ->
    let visible_rows = visible_rows ~dimensions in
    let procs = Render.sorted_processes model.snapshot ~sort_by:model.ui.sort_by in
    let ui =
      Types.Ui_state.apply
        model.ui
        action
        ~num_processes:(List.length procs)
        ~visible_rows
    in
    { model with ui }
;;

let event_to_action (event : Event.t) : Types.Ui_state.action option =
  match event with
  | Key_press { key = ASCII ('j' | 'J'); mods = [] }
  | Key_press { key = Arrow `Down; mods = [] } -> Some Select_next
  | Key_press { key = ASCII ('k' | 'K'); mods = [] }
  | Key_press { key = Arrow `Up; mods = [] } -> Some Select_prev
  | Key_press { key = Page `Down; mods = [] } -> Some Page_down
  | Key_press { key = Page `Up; mods = [] } -> Some Page_up
  | Key_press { key = ASCII 'g'; mods = [] } -> Some Goto_top
  | Key_press { key = ASCII 'G'; mods = [] } -> Some Goto_bottom
  | Key_press { key = ASCII ('c' | 'C'); mods = [] } -> Some (Set_sort Cpu)
  | Key_press { key = ASCII ('m' | 'M'); mods = [] } -> Some (Set_sort Mem)
  | Key_press { key = ASCII ('p' | 'P'); mods = [] } -> Some (Set_sort Pid)
  | Key_press { key = ASCII ('n' | 'N'); mods = [] } -> Some (Set_sort Name)
  | Key_press { key = ASCII ('s' | 'S'); mods = [] } -> Some Cycle_sort
  | _ -> None
;;

let make_component ~poll ~initial_snapshot ~(exit : unit -> unit Effect.t) ~dimensions
  (local_ graph)
  =
  let model, inject =
    Bonsai.state_machine
      ~default_model:{ Model.snapshot = initial_snapshot; ui = Types.Ui_state.initial }
      ~apply_action:(fun _ctx model (incoming, dimensions) ->
        apply_incoming model incoming ~dimensions)
      graph
  in
  let inject =
    let%arr inject and dimensions in
    fun (incoming : Incoming.t) -> inject (incoming, dimensions)
  in
  let () =
    match poll with
    | None -> ()
    | Some poll_every ->
      Bonsai.Clock.every
        ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
        ~trigger_on_activate:true
        (Bonsai.return poll_every)
        (let%arr inject and model in
         let%bind.Effect snapshot =
           Effect.of_deferred_thunk (fun () ->
             Sysinfo.collect ~prev_history:model.snapshot.cpu_history ())
         in
         inject (Incoming.Set_snapshot snapshot))
        graph
  in
  let view =
    let%arr model and dimensions in
    Render.view ~snapshot:model.snapshot ~ui:model.ui ~dimensions
  in
  let handler =
    let%arr inject in
    fun (event : Event.t) ->
      match event with
      | Key_press { key = ASCII ('q' | 'Q'); mods = [] }
      | Key_press { key = ASCII 'c'; mods = [ Ctrl ] } -> exit ()
      | event ->
        (match event_to_action event with
         | None -> Effect.Ignore
         | Some action -> inject (Incoming.Ui_action action))
  in
  let%arr view and handler and model and inject in
  { view; handler; model; inject }
;;

let app ~exit ~dimensions (local_ graph) =
  let result =
    make_component
      ~poll:(Some (Time_ns.Span.of_sec 1.0))
      ~initial_snapshot:empty_snapshot
      ~exit
      ~dimensions
      graph
  in
  let view =
    let%arr result in
    result.view
  in
  let handler =
    let%arr result in
    result.handler
  in
  ~view, ~handler
;;

let test_component ~initial_snapshot ~dimensions (local_ graph) =
  make_component
    ~poll:None
    ~initial_snapshot
    ~exit:(fun () -> Effect.Ignore)
    ~dimensions
    graph
;;
