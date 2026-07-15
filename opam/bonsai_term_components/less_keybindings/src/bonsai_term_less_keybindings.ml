open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Action = struct
  type t =
    | Up
    | Down
    | Top
    | Bottom
    | Up_half_screen
    | Down_half_screen
  [@@deriving sexp_of]
end

let component handler (local_ _graph)
  : (Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t
  =
  (* NOTE: This component takes a graph today for the sake of potentially keeping track of
     state in the future to handle multi-key keybindings. *)
  let handler =
    let%arr handler in
    fun (event : Event.t) ->
      let handler event = Captured_or_ignored.capture (handler event) in
      match event with
      | Key_press { key = ASCII 'j'; mods = [] }
      | Key_press { key = Arrow `Down; mods = [] }
      | Key_press { key = ASCII ('e' | 'E'); mods = [ Ctrl ] }
      | Mouse { kind = Scroll `Down; position = _; mods = [] } -> handler Action.Down
      | Key_press { key = ASCII 'd'; mods = [ Ctrl ] | [] }
      | Key_press { key = ASCII 'D'; mods = [ Ctrl ] }
      | Key_press { key = Page `Down; mods = [] } -> handler Down_half_screen
      | Key_press { key = ASCII 'u'; mods = [ Ctrl ] | [] }
      | Key_press { key = ASCII 'U'; mods = [ Ctrl ] }
      | Key_press { key = Page `Up; mods = [] } -> handler Up_half_screen
      | Key_press { key = ASCII 'k'; mods = [] }
      | Key_press { key = Arrow `Up; mods = [] }
      | Key_press { key = ASCII ('y' | 'Y'); mods = [ Ctrl ] }
      | Mouse { kind = Scroll `Up; position = _; mods = [] } -> handler Up
      | Key_press { key = ASCII 'g'; mods = [] } -> handler Top
      | Key_press { key = ASCII 'G'; mods = [] } -> handler Bottom
      | _ -> Captured_or_ignored.ignore
  in
  handler
;;
