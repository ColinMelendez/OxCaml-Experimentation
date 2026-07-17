open! Core
open Bonsai_term
open Bonsai.Let_syntax
open Types

module Incoming = struct
  type t = Ui_action of Ui_state.action [@@deriving sexp]
end

module Model = struct
  type t =
    { demos : Demo.t list
    ; ui : Ui_state.t
    }
  [@@deriving sexp, equal]
end

type result =
  { view : View.t
  ; handler : Bonsai_term.Event.t -> unit Effect.t
  ; model : Model.t
  ; inject : Incoming.t -> unit Effect.t
  }

let apply_incoming (model : Model.t) (incoming : Incoming.t) =
  match incoming with
  | Ui_action action ->
    let demo = Demos.by_index model.demos model.ui.demo_index in
    let ui =
      Ui_state.apply
        model.ui
        action
        ~num_demos:(List.length model.demos)
        ~num_steps:(Demo.num_steps demo)
    in
    { model with ui }
;;

let event_to_action (event : Bonsai_term.Event.t) : Ui_state.action option =
  match event with
  | Key_press { key = ASCII ' '; mods = [] }
  | Key_press { key = ASCII ('l' | 'L'); mods = [] }
  | Key_press { key = Arrow `Right; mods = [] } -> Some Next_step
  | Key_press { key = ASCII ('h' | 'H'); mods = [] }
  | Key_press { key = Arrow `Left; mods = [] } -> Some Prev_step
  | Key_press { key = ASCII ('r' | 'R'); mods = [] } -> Some Reset
  | Key_press { key = ASCII ('p' | 'P'); mods = [] } -> Some Toggle_play
  | Key_press { key = ASCII ']'; mods = [] }
  | Key_press { key = ASCII ('n' | 'N'); mods = [] } -> Some Next_demo
  | Key_press { key = ASCII '['; mods = [] } -> Some Prev_demo
  | Key_press { key = ASCII '1'; mods = [] } -> Some (Set_demo 0)
  | Key_press { key = ASCII '2'; mods = [] } -> Some (Set_demo 1)
  | Key_press { key = ASCII '3'; mods = [] } -> Some (Set_demo 2)
  | Key_press { key = ASCII '4'; mods = [] } -> Some (Set_demo 3)
  | _ -> None
;;

let make_component
  ~(demos : Demo.t list)
  ~(exit : unit -> unit Effect.t)
  ~dimensions
  ?(autoplay : bool = false)
  (local_ graph)
  =
  let model, inject =
    Bonsai.state_machine
      ~default_model:{ Model.demos; ui = { Ui_state.initial with playing = autoplay } }
      ~apply_action:(fun _ctx model incoming -> apply_incoming model incoming)
      graph
  in
  let () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
      ~trigger_on_activate:false
      (Bonsai.return (Time_ns.Span.of_ms 450.))
      (let%arr inject and model in
       if model.ui.playing
       then inject (Incoming.Ui_action Tick)
       else Effect.Ignore)
      graph
  in
  let view =
    let%arr model and dimensions in
    Render.view ~demos:model.demos ~ui:model.ui ~dimensions
  in
  let is_ctrl_c (event : Bonsai_term.Event.t) =
    match event with
    | Key_press { key = ASCII ('C' | 'c'); mods = [ Ctrl ] } -> true
    | Key_press { key = Uchar uchar; mods = [ Ctrl ] } ->
      Uchar.equal (Uchar.of_char 'C') uchar || Uchar.equal (Uchar.of_char 'c') uchar
    | _ -> false
  in
  let handler =
    let%arr inject in
    fun (event : Bonsai_term.Event.t) ->
      match event with
      | Key_press { key = ASCII ('q' | 'Q'); mods = [] } -> exit ()
      | event when is_ctrl_c event -> exit ()
      | event ->
        (match event_to_action event with
         | None -> Effect.Ignore
         | Some action -> inject (Incoming.Ui_action action))
  in
  let%arr view and handler and model and inject in
  { view; handler; model; inject }
;;

let app ~exit ~dimensions (local_ graph) =
  let demos = Demos.all () in
  let result = make_component ~demos ~exit ~dimensions graph in
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

let test_component ~demos ~dimensions (local_ graph) =
  make_component
    ~demos
    ~exit:(fun () -> Effect.Ignore)
    ~dimensions
    ~autoplay:false
    graph
;;
