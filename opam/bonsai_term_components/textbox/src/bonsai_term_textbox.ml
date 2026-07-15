open! Core
open! Bonsai
open! Bonsai_term
open Bonsai.Let_syntax

type t =
  { view : View.t
  ; string : string
  ; handler : Event.t -> unit Effect.t
  ; set : string -> unit Effect.t
  }

module Model = struct
  type t =
    { text : string
    ; cursor : int
    }
  [@@deriving sexp_of, equal]
end

module Action = struct
  type t =
    | Insert_char of char
    | Insert_uchar of Uchar.t
    | Backspace
    | Delete
    | Clear
    | Set of string
    | Move_left
    | Move_right
    | Home
    | End
  [@@deriving sexp_of]
end

let apply_action
  (_ : _ Bonsai.Apply_action_context.t)
  (model : Model.t)
  (action : Action.t)
  =
  match action with
  | Clear ->
    let after = String.drop_prefix model.text model.cursor in
    { Model.text = after; cursor = 0 }
  | Set s -> { text = s; cursor = String.length s }
  | Insert_char char ->
    let before = String.prefix model.text model.cursor in
    let after = String.drop_prefix model.text model.cursor in
    { text = before ^ Char.to_string char ^ after; cursor = model.cursor + 1 }
  | Insert_uchar uchar ->
    let before = String.prefix model.text model.cursor in
    let after = String.drop_prefix model.text model.cursor in
    let s = Uchar.Utf8.to_string uchar in
    { text = before ^ s ^ after; cursor = model.cursor + String.length s }
  | Backspace ->
    if model.cursor = 0
    then model
    else (
      let before = String.prefix model.text (model.cursor - 1) in
      let after = String.drop_prefix model.text model.cursor in
      { text = before ^ after; cursor = model.cursor - 1 })
  | Delete ->
    if model.cursor >= String.length model.text
    then model
    else (
      let before = String.prefix model.text model.cursor in
      let after = String.drop_prefix model.text (model.cursor + 1) in
      { text = before ^ after; cursor = model.cursor })
  | Move_left -> { model with cursor = Int.max 0 (model.cursor - 1) }
  | Move_right ->
    { model with cursor = Int.min (String.length model.text) (model.cursor + 1) }
  | Home -> { model with cursor = 0 }
  | End -> { model with cursor = String.length model.text }
;;

let component
  ?(cursor_attrs = Bonsai.return [])
  ?(text_attrs = Bonsai.return [])
  ?(default_model = "")
  ~is_focused
  (local_ graph)
  =
  let model, inject =
    Bonsai.state_machine
      ~default_model:{ Model.text = default_model; cursor = String.length default_model }
      ~apply_action
      graph
  in
  let set =
    let%arr inject in
    fun value -> inject (Set value)
  in
  let handler =
    let%arr inject in
    fun (event : Event.t) ->
      match event with
      | Mouse _ | Paste _ -> Effect.Ignore
      | Key_press { key = ASCII char; mods = [] } -> inject (Insert_char char)
      | Key_press { key = Uchar uchar; mods = [] } -> inject (Insert_uchar uchar)
      | Key_press { key = ASCII ('U' | 'u'); mods = [ Ctrl ] } -> inject Clear
      | Key_press { key = Backspace; mods = [] } -> inject Backspace
      | Key_press { key = Delete; mods = [] } -> inject Delete
      | Key_press { key = Arrow `Left; mods = [] } -> inject Move_left
      | Key_press { key = Arrow `Right; mods = [] } -> inject Move_right
      | Key_press { key = Home; mods = [] } -> inject Home
      | Key_press { key = End; mods = [] } -> inject End
      | _ -> Effect.Ignore
  in
  let view =
    let%arr model and is_focused and cursor_attrs and text_attrs in
    if not is_focused
    then View.text ~attrs:text_attrs model.text
    else (
      let before = String.prefix model.text model.cursor in
      let at_cursor =
        if model.cursor < String.length model.text
        then String.make 1 (String.get model.text model.cursor)
        else " "
      in
      let after =
        if model.cursor < String.length model.text
        then String.drop_prefix model.text (model.cursor + 1)
        else ""
      in
      View.hcat
        [ View.text ~attrs:text_attrs before
        ; View.text ~attrs:(cursor_attrs @ [ Attr.invert; Attr.blink ]) at_cursor
        ; View.text ~attrs:text_attrs after
        ])
  in
  let%arr model and view and handler and set in
  { view; string = model.text; handler; set }
;;
