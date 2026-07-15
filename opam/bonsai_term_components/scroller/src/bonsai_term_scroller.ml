open! Core
open Bonsai
open Bonsai.Let_syntax
open Bonsai_term

module Action = struct
  type t =
    | Scroll_to of
        { bottom : int
        ; top : int
        }
    | Up
    | Down
    | Top
    | Bottom
    | Up_half_screen
    | Down_half_screen
    | Stick_to_bottom
  [@@deriving sexp_of]

  type inner_action =
    | Public_action of t
    | Set_offset of int
    | Single_g
    | Not_g
  [@@deriving sexp_of]
end

module Input = struct
  type t =
    { dimensions : Dimensions.t
    ; content_height : int
    }
  [@@deriving sexp_of]
end

module Model = struct
  type t =
    { offset : int
    ; last_time_g_was_pressed : Time_ns.t option
    ; stuck_to_bottom : bool
    }
  [@@deriving sexp_of]
end

module Scroll_position = struct
  type t =
    | Top
    | Bottom
    | Percentage of Percent.t
    | All_visible
  [@@deriving sexp_of]
end

type t =
  { view : View.t
  ; inject : Action.t -> unit Effect.t
  ; less_keybindings_handler : Event.t -> Captured_or_ignored.t Effect.t
  ; stuck_to_bottom : bool
  ; scroll_position : Scroll_position.t
  }

let recv
  ctx
  input
  ({ Model.offset; last_time_g_was_pressed; stuck_to_bottom } as model)
  action
  : Model.t * Captured_or_ignored.t
  =
  let time_source = Bonsai.Apply_action_context.time_source ctx in
  let captured x = x, Captured_or_ignored.captured () in
  let ignored x = x, Captured_or_ignored.ignored in
  match input with
  | Bonsai.Computation_status.Inactive -> ignored model
  | Active { Input.dimensions = { Dimensions.height; width = _ }; content_height } ->
    (match stuck_to_bottom, action with
     | true, Action.Public_action Stick_to_bottom -> captured model
     | _ ->
       let time_interval = Time_ns.Span.of_sec 0.3 in
       let now = Bonsai.Time_source.now time_source in
       let max_bounded_offset = Int.max 0 (content_height - height) in
       let prev_offset = offset in
       let stuck_to_bottom =
         match action with
         | Action.Public_action Stick_to_bottom -> true
         | _ -> stuck_to_bottom
       in
       let offset, main_action_captured_or_ignored =
         match action with
         | Action.Public_action (Scroll_to { bottom; top }) ->
           (* NOTE: I am unsure if defaulting to always scrolling to the top when the
              element is bigger than the viewport is totally correct... *)
           let min_visible = offset in
           let max_visible = offset + height - 1 in
           captured
           @@
           if bottom <= max_visible && top >= min_visible
           then offset
           else if bottom < max_visible
           then top
           else offset + (bottom - max_visible)
         | Public_action Up -> captured (Int.max 0 (offset - 1))
         | Public_action Up_half_screen -> captured (Int.max 0 (offset - (height / 2)))
         | Public_action Down -> captured (Int.min max_bounded_offset (offset + 1))
         | Public_action Down_half_screen ->
           captured
             (Int.min
                max_bounded_offset
                (Int.min (content_height - 1) (offset + (height / 2))))
         | Public_action Top -> captured 0
         | Public_action Bottom -> captured max_bounded_offset
         | Public_action Stick_to_bottom -> captured max_bounded_offset
         | Set_offset offset -> captured offset
         | Not_g -> ignored offset
         | Single_g ->
           captured
           @@
             (match last_time_g_was_pressed with
             | None -> offset
             | Some last ->
               (match Time_ns.Span.O.(Time_ns.diff now last < time_interval) with
                | true -> 0
                | false -> offset))
       in
       let stuck_to_bottom =
         match action with
         | Action.Set_offset _ ->
           (* don't re-compute [stuck_to_bottom] if the offset was set directly *)
           stuck_to_bottom
         | _ -> stuck_to_bottom && offset >= prev_offset
       in
       let last_time_g_was_pressed, g_press_captured =
         match action with
         | Single_g ->
           captured
           @@
             (match last_time_g_was_pressed with
             | None -> Some now
             | Some last ->
               (match Time_ns.Span.O.(Time_ns.diff now last < time_interval) with
                | true -> None
                | false -> Some now))
         | Not_g -> captured None
         | _ -> ignored last_time_g_was_pressed
       in
       let captured_or_ignored =
         Captured_or_ignored.any [ main_action_captured_or_ignored; g_press_captured ]
       in
       { offset; last_time_g_was_pressed; stuck_to_bottom }, captured_or_ignored)
;;

let use_less_keybindings
  (event : Event.t)
  (inject : Action.inner_action -> Captured_or_ignored.t Effect.t)
  : Captured_or_ignored.t Effect.t
  =
  let%bind.Effect cancelled_g_press_capture_or_not =
    match event with
    | Key_press { key = ASCII 'g'; mods = [] } -> Captured_or_ignored.ignore
    | _ -> inject Not_g
  in
  let%bind.Effect captured_or_not =
    match event with
    | Key_press { key = ASCII 'j'; mods = [] }
    | Key_press { key = Arrow `Down; mods = [] }
    | Key_press { key = ASCII ('e' | 'E'); mods = [ Ctrl ] } ->
      inject (Public_action Down)
    | Key_press { key = ASCII 'd'; mods = [ Ctrl ] | [] }
    | Key_press { key = ASCII 'D'; mods = [ Ctrl ] }
    | Key_press { key = Page `Down; mods = [] } -> inject (Public_action Down_half_screen)
    | Key_press { key = ASCII 'u'; mods = [ Ctrl ] | [] }
    | Key_press { key = ASCII 'U'; mods = [ Ctrl ] }
    | Key_press { key = Page `Up; mods = [] } -> inject (Public_action Up_half_screen)
    | Key_press { key = ASCII 'k'; mods = [] }
    | Key_press { key = Arrow `Up; mods = [] }
    | Key_press { key = ASCII ('y' | 'Y'); mods = [ Ctrl ] } -> inject (Public_action Up)
    | Key_press { key = ASCII 'g'; mods = [] } -> inject Single_g
    | Key_press { key = ASCII 'G'; mods = [] } -> inject (Public_action Bottom)
    | Mouse { kind = Scroll `Down; position = _; mods = [] } ->
      let%map.Effect () =
        Effect.all_unit
          (List.create
             ~len:5
             (let%bind.Effect _ : Captured_or_ignored.t = inject (Public_action Down) in
              Effect.return ()))
      in
      Captured_or_ignored.captured ()
    | Mouse { kind = Scroll `Up; position = _; mods = [] } ->
      let%map.Effect () =
        Effect.all_unit
          (List.create
             ~len:5
             (let%bind.Effect _ : Captured_or_ignored.t = inject (Public_action Up) in
              Effect.return ()))
      in
      Captured_or_ignored.captured ()
    | _ -> Captured_or_ignored.ignore
  in
  Effect.return
    (Captured_or_ignored.any [ cancelled_g_press_capture_or_not; captured_or_not ])
;;

let component
  ?(default_stuck_to_bottom = false)
  ~crop_width_if_too_big
  ~dimensions
  view
  (local_ graph)
  =
  let content_height =
    let%arr view in
    View.height view
  in
  let%sub { offset = offset_state; last_time_g_was_pressed = _; stuck_to_bottom }, inject =
    let input =
      let%arr content_height and dimensions in
      { Input.content_height; dimensions }
    in
    let state, inject =
      Bonsai.actor_with_input
        ~default_model:
          { Model.offset = 0
          ; last_time_g_was_pressed = None
          ; stuck_to_bottom = default_stuck_to_bottom
          }
        ~recv
        input
        graph
    in
    let%arr state and inject in
    state, inject
  in
  let offset =
    let%arr offset_state and stuck_to_bottom and content_height and dimensions in
    let max_offset = Int.max 0 (content_height - dimensions.Dimensions.height) in
    if stuck_to_bottom then max_offset else Int.min offset_state max_offset
  in
  (* Keep the internal offset up to date so that scrolling _out_ of a sticky state still
     works. *)
  Bonsai.Edge.on_change
    ~trigger:`After_display
    ~equal:[%equal: int * int]
    (Bonsai.both offset_state offset)
    graph
    ~callback:
      (let%arr inject and stuck_to_bottom in
       fun (offset_state, offset) ->
         if stuck_to_bottom && not (Int.equal offset_state offset)
         then (
           let%bind.Effect _ : Captured_or_ignored.t = inject (Set_offset offset) in
           Effect.return ())
         else Effect.Ignore);
  let view =
    let%arr view and offset and content_height and dimensions in
    let view = View.crop ~t:offset view in
    let b_crop = Int.max 0 (content_height - dimensions.Dimensions.height - offset) in
    let view = View.crop ~b:b_crop view in
    match crop_width_if_too_big with
    | `No -> view
    | `Yes ->
      let r_crop = Int.max 0 (View.width view - dimensions.Dimensions.width) in
      View.crop ~r:r_crop view
  in
  let less_keybindings_handler =
    let%arr inject in
    fun event -> use_less_keybindings event inject
  in
  let inject =
    let%arr inject in
    fun action -> inject (Public_action action)
  in
  let inject =
    let%arr inject in
    fun action ->
      let%bind.Effect _ : Captured_or_ignored.t = inject action in
      Effect.return ()
  in
  let scroll_position =
    let%arr offset and content_height and dimensions in
    let max_bounded_offset = content_height - dimensions.Dimensions.height in
    if max_bounded_offset <= 0
    then Scroll_position.All_visible
    else if offset <= 0
    then Scroll_position.Top
    else if offset >= max_bounded_offset
    then Scroll_position.Bottom
    else
      Scroll_position.Percentage
        (Percent.of_mult (Float.of_int offset /. Float.of_int max_bounded_offset))
  in
  let%arr view
  and inject
  and less_keybindings_handler
  and stuck_to_bottom
  and scroll_position in
  { view; inject; less_keybindings_handler; stuck_to_bottom; scroll_position }
;;

module Scrollbar = struct
  module Style = struct
    let default_track_attrs = [ Attr.fg (Attr.Color.rgb ~r:80 ~g:80 ~b:80) ]

    let default_thumb_attrs =
      [ Attr.fg (Attr.Color.rgb ~r:200 ~g:200 ~b:200)
      ; Attr.bg (Attr.Color.rgb ~r:100 ~g:100 ~b:100)
      ]
    ;;

    let vertical_bar ?track_attrs ?thumb_attrs ~scroll_position ~height () =
      if height <= 0
      then View.none
      else (
        let thumb_position =
          match (scroll_position : Scroll_position.t) with
          | All_visible -> None
          | Top -> Some 0
          | Bottom -> Some (height - 1)
          | Percentage pct ->
            Some
              (Float.iround_nearest_exn
                 (Percent.to_mult pct *. Float.of_int (height - 1)))
        in
        match thumb_position with
        | None -> View.vcat (List.init height ~f:(fun _ -> View.text " "))
        | Some thumb_position ->
          let track_attrs = Option.value track_attrs ~default:default_track_attrs in
          let thumb_attrs = Option.value thumb_attrs ~default:default_thumb_attrs in
          View.vcat
            (List.init height ~f:(fun i ->
               if i = thumb_position
               then View.text ~attrs:thumb_attrs "┃"
               else View.text ~attrs:track_attrs "│")))
    ;;

    let vim_status ?attrs ~scroll_position () =
      let text =
        match (scroll_position : Scroll_position.t) with
        | Top -> "Top"
        | Bottom -> "Bot"
        | All_visible -> "All"
        | Percentage pct ->
          [%string "%{Percent.to_percentage pct |> Float.iround_nearest_exn#Int}%"]
      in
      match attrs with
      | None -> View.text text
      | Some attrs -> View.text ~attrs text
    ;;
  end
end
