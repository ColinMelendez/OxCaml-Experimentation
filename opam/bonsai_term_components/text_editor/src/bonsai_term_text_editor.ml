open! Core
open Bonsai_term
open Bonsai.Let_syntax
open Zed
module Vim_text_object_command = Vim_text_object_command
open Vim_text_object_movements

let cursor_tag : (Bonsai.Path.t, Region.t) View.Tag.t =
  View.Tag.create
    (module Bonsai.Path)
    ~reduce:(fun _ t -> t)
    ~transform_regions:(fun region f -> f region)
;;

module Action = struct
  type t =
    | Insert of string
    | Newline
    | Next_char
    | Prev_char
    | Next_line
    | Prev_line
    | Goto_bol
    | Goto_first_non_whitespace_character_in_line
    | Goto_eol
    | Goto_bot
    | Goto_eot
    | Delete_next_char
    | Delete_prev_char
    | Delete_next_line
    | Delete_prev_line
    | Kill_next_char
    | Kill_next_line
    | Kill_prev_line
    | Next_word
    | Prev_word
    | Delete_next_word
    | Delete_prev_word
    | Kill_next_word
    | Kill_prev_word
    | Yank
    | Undo
    | Clear
    | Replace_char of string
    | Prev_char_in_line
    | Next_word_vim
    | Prev_word_vim
    | Next_WORD
    | Prev_WORD
    | End_word
    | End_WORD
    | Find_char_forward of char
    | Find_char_backward of char
    | Till_char_forward of char
    | Till_char_backward of char
    | Vim_text_object_command of Vim_text_object_command.t
  [@@deriving sexp_of]

  let to_zed_action
    :  t
    -> [ `Forward_to_zed of Zed_edit.action
       | `Custom of
         [ `prev_line
         | `next_line
         | `goto_first_non_whitespace_character_in_line
         | `replace_char of string
         | `kill_next_char
         | `next_word_emacs
         | `prev_word_emacs
         | `next_word_vim
         | `prev_word_vim
         | `next_WORD
         | `prev_WORD
         | `end_word
         | `end_WORD
         | `clear
         | `find_char_forward of char
         | `find_char_backward of char
         | `till_char_forward of char
         | `till_char_backward of char
         | `execute_vim_text_object_command of Vim_text_object_command.t
         | `prev_char_in_line
         ]
       ]
    = function
    | Insert s -> `Forward_to_zed (`insert s)
    | Newline -> `Forward_to_zed `newline
    | Next_char -> `Forward_to_zed `next_char
    | Prev_char -> `Forward_to_zed `prev_char
    | Next_line -> `Custom `next_line
    | Prev_line -> `Custom `prev_line
    | Goto_bol -> `Forward_to_zed `goto_bol
    | Goto_first_non_whitespace_character_in_line ->
      `Custom `goto_first_non_whitespace_character_in_line
    | Goto_eol -> `Forward_to_zed `goto_eol
    | Goto_bot -> `Forward_to_zed `goto_bot
    | Goto_eot -> `Forward_to_zed `goto_eot
    | Delete_next_char -> `Forward_to_zed `delete_next_char
    | Delete_prev_char -> `Forward_to_zed `delete_prev_char
    | Delete_next_line -> `Forward_to_zed `delete_next_line
    | Delete_prev_line -> `Forward_to_zed `delete_prev_line
    | Kill_next_char -> `Custom `kill_next_char
    | Kill_next_line -> `Forward_to_zed `kill_next_line
    | Kill_prev_line -> `Forward_to_zed `kill_prev_line
    | Next_word -> `Custom `next_word_emacs
    | Prev_word -> `Custom `prev_word_emacs
    | Next_word_vim -> `Custom `next_word_vim
    | Prev_word_vim -> `Custom `prev_word_vim
    | Next_WORD -> `Custom `next_WORD
    | Prev_WORD -> `Custom `prev_WORD
    | Delete_next_word -> `Forward_to_zed `delete_next_word
    | Delete_prev_word -> `Forward_to_zed `delete_prev_word
    | Kill_next_word -> `Forward_to_zed `kill_next_word
    | Kill_prev_word -> `Forward_to_zed `kill_prev_word
    | Yank -> `Forward_to_zed `yank
    | Undo -> `Forward_to_zed `undo
    | Clear -> `Custom `clear
    | Replace_char s -> `Custom (`replace_char s)
    | End_word -> `Custom `end_word
    | End_WORD -> `Custom `end_WORD
    | Find_char_forward c -> `Custom (`find_char_forward c)
    | Find_char_backward c -> `Custom (`find_char_backward c)
    | Till_char_forward c -> `Custom (`till_char_forward c)
    | Till_char_backward c -> `Custom (`till_char_backward c)
    | Vim_text_object_command command ->
      `Custom (`execute_vim_text_object_command command)
    | Prev_char_in_line -> `Custom `prev_char_in_line
  ;;
end

module Cursor = struct
  type t =
    { logical_line : int
    ; logical_column : int
    ; visual_column : int (* Some characters (e.g. some emojis) will be twice as wide! *)
    ; visual_line : int (* Visual line number considering wrapped lines *)
    ; position : int
    }
  [@@deriving sexp_of]
end

let maximum_character_size = 5

let wrap_text ~text ~max_width : ([ `Not_wrapped | `Wrapped ] * string) list =
  let lines = String.split ~on:'\n' text in
  let max_width =
    (* NOTE: Some characters are bigger than 1 char visually, so [width = 1] could result
       in a forever loop. Setting it to a minimum [max_width] of 5 makes it so that every
       character is guaranteed to visually fit. *)
    Int.max max_width maximum_character_size
  in
  let wrap_line line =
    if max_width <= 0
    then [ line ]
    else if String.is_empty line
    then [ "" ] (* Preserve empty lines *)
    else (
      let utf8_chars = String.Utf8.to_list (String.Utf8.of_string line) in
      let rec wrap_chars ~chars ~current_line ~current_width ~acc =
        match chars with
        | [] ->
          (* Always include the current line, even if empty (though it shouldn't be empty
             here) *)
          List.rev (current_line :: acc)
        | char :: rest ->
          let char_width = Int.min maximum_character_size (View.uchar_tty_width char) in
          let char_str = String.Utf8.of_list [ char ] |> String.Utf8.to_string in
          if current_width + char_width <= max_width
          then
            wrap_chars
              ~chars:rest
              ~current_line:(current_line ^ char_str)
              ~current_width:(current_width + char_width)
              ~acc
          else if String.is_empty current_line
          then
            (* Character is too wide for the line, but we must include it to avoid
               infinite loop *)
            wrap_chars
              ~chars:rest
              ~current_line:char_str
              ~current_width:char_width
              ~acc:(current_line :: acc)
          else
            wrap_chars ~chars ~current_line:"" ~current_width:0 ~acc:(current_line :: acc)
      in
      wrap_chars ~chars:utf8_chars ~current_line:"" ~current_width:0 ~acc:[])
  in
  List.concat_map lines ~f:(fun line ->
    let sub_lines = wrap_line line in
    match sub_lines with
    | [] -> []
    | first :: rem -> (`Not_wrapped, first) :: List.map rem ~f:(fun x -> `Wrapped, x))
;;

let calculate_visual_position ~text ~logical_line ~logical_column ~max_width =
  let max_width = Int.max maximum_character_size max_width in
  let lines = String.split ~on:'\n' text in
  if logical_line >= List.length lines
  then
    ( ~visual_line:logical_line
    , ~visual_column:logical_column (* Fallback to logical position *) )
  else (
    (* First, count the visual lines from all previous logical lines *)
    let visual_lines_before =
      List.take lines logical_line
      |> List.sum (module Int) ~f:(fun line_text ->
        List.length (wrap_text ~text:line_text ~max_width))
    in
    (* Now find the position within the current logical line *)
    let line_text = List.nth_exn lines logical_line in
    let utf8_chars = String.Utf8.to_list (String.Utf8.of_string line_text) in
    let chars_before_cursor = List.take utf8_chars logical_column in
    let rec find_visual_line ~chars_remaining ~current_line_chars ~visual_line_offset =
      match chars_remaining with
      | [] ->
        (* Cursor is at the end of this logical line *)
        let visual_column =
          List.sum (module Int) ~f:View.uchar_tty_width current_line_chars
        in
        let visual_line = visual_lines_before + visual_line_offset in
        if visual_column >= max_width
        then ~visual_line:(visual_line + 1), ~visual_column:0
        else ~visual_line, ~visual_column
      | char :: rest ->
        let char_width = View.uchar_tty_width char in
        let current_width =
          List.sum (module Int) ~f:View.uchar_tty_width current_line_chars
        in
        if current_width + char_width <= max_width
        then
          find_visual_line
            ~chars_remaining:rest
            ~current_line_chars:(current_line_chars @ [ char ])
            ~visual_line_offset
        else
          (* Move to next visual line *)
          find_visual_line
            ~chars_remaining
            ~current_line_chars:[]
            ~visual_line_offset:(visual_line_offset + 1)
    in
    find_visual_line
      ~chars_remaining:chars_before_cursor
      ~current_line_chars:[]
      ~visual_line_offset:0)
;;

type t =
  { text : string Bonsai.t
  ; send_actions : (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
  ; view : View.t Bonsai.t
  ; cursor : Cursor.t Bonsai.t
  ; set_text : (string -> unit Effect.t) Bonsai.t
  ; rope : Zed.Zed_rope.t Bonsai.t
  ; get_cursor_position : (View.t -> Position.t option) Bonsai.t
  }

module Highlight = struct
  type t =
    { start_offset : int
    ; end_offset : int
    ; attrs : (Attr.t list[@sexp.opaque])
    }
  [@@deriving sexp_of]
end

(** Render a single wrapped line, splitting it into differently-styled segments wherever a
    highlight range overlaps the line. [line_start_offset] is the byte offset of the first
    character of [line] within the full text. [highlights] must be sorted by
    [start_offset] and non-overlapping. *)
let render_line_with_highlights ~line ~line_start_offset ~text_attrs ~highlights =
  let line_end_offset = line_start_offset + String.length line in
  let overlapping =
    List.filter_map highlights ~f:(fun { Highlight.start_offset; end_offset; attrs } ->
      let start_offset = Int.max start_offset line_start_offset in
      let end_offset = Int.min end_offset line_end_offset in
      if start_offset < end_offset then Some (start_offset, end_offset, attrs) else None)
  in
  match overlapping with
  | [] -> View.text ~attrs:text_attrs line
  | overlapping ->
    let slice ~from ~until =
      String.sub line ~pos:(from - line_start_offset) ~len:(until - from)
    in
    let segments, last_offset =
      List.fold
        overlapping
        ~init:([], line_start_offset)
        ~f:(fun (segments, previous_end) (start_offset, end_offset, attrs) ->
          (* Defensively clamp in case the caller supplied overlapping ranges. *)
          let start_offset = Int.max start_offset previous_end in
          let end_offset = Int.max end_offset start_offset in
          let plain = slice ~from:previous_end ~until:start_offset in
          let highlighted = slice ~from:start_offset ~until:end_offset in
          let segments =
            (highlighted, text_attrs @ attrs) :: (plain, text_attrs) :: segments
          in
          segments, end_offset)
    in
    let segments =
      (slice ~from:last_offset ~until:line_end_offset, text_attrs) :: segments |> List.rev
    in
    (match
       List.filter segments ~f:(fun (segment, _) -> not (String.is_empty segment))
     with
     | [] -> View.text ~attrs:text_attrs line
     | segments ->
       View.hcat (List.map segments ~f:(fun (segment, attrs) -> View.text ~attrs segment)))
;;

(** Render the wrapped lines of the editor, applying [highlights] (byte ranges over the
    unwrapped text). The byte offset of each wrapped line is reconstructed from the
    wrapping itself: a [`Not_wrapped] line (other than the first) is preceded by a newline
    byte, while a [`Wrapped] line continues the previous line directly. *)
let render_wrapped_lines ~wrapped_lines ~text_attrs ~highlights =
  let highlights =
    List.filter highlights ~f:(fun { Highlight.start_offset; end_offset; attrs = _ } ->
      start_offset < end_offset)
    |> List.sort
         ~compare:
           (Comparable.lift [%compare: int] ~f:(fun { Highlight.start_offset; _ } ->
              start_offset))
  in
  let views, (_ : int), (_ : bool) =
    List.fold
      wrapped_lines
      ~init:([], 0, true)
      ~f:(fun (views, offset, is_first) (kind, line) ->
        let offset =
          match kind, is_first with
          | `Not_wrapped, false -> offset + 1 (* The newline before this logical line. *)
          | (`Not_wrapped | `Wrapped), _ -> offset
        in
        let view =
          render_line_with_highlights
            ~line
            ~line_start_offset:offset
            ~text_attrs
            ~highlights
        in
        view :: views, offset + String.length line, false)
  in
  View.vcat (List.rev views)
;;

let compute_text ~zed_context =
  let rope = Zed_edit.text (Zed_edit.edit zed_context) in
  let text = Zed_rope.to_string rope in
  ~text, ~rope
;;

let compute_cursor ~zed_context ~text ~width =
  let cursor = Zed_edit.cursor zed_context in
  let position = Zed_edit.position zed_context in
  let logical_line = Zed_cursor.get_line cursor
  and logical_column = Zed_cursor.get_column cursor in
  let ~visual_line, ~visual_column =
    calculate_visual_position ~text ~logical_line ~logical_column ~max_width:width
  in
  let cursor =
    { Cursor.logical_line; logical_column; visual_column; visual_line; position }
  in
  cursor
;;

let utf8_chars_of_string s = String.Utf8.to_list (String.Utf8.of_string s)

let is_leading_whitespace_uchar uchar =
  let whitespace_uchars = List.map [ ' '; '\t'; '\r' ] ~f:Uchar.of_char in
  List.mem whitespace_uchars uchar ~equal:Uchar.equal
;;

let ensure_cursor_visible ~visual_line ~scroll_offset ~max_height =
  if visual_line < scroll_offset
  then (* Cursor is above visible area, scroll up *)
    Int.max 0 visual_line
  else if visual_line >= scroll_offset + max_height
  then
    (* Cursor is below visible area, scroll down *)
    Int.max 0 (visual_line - max_height + 1)
  else scroll_offset
;;

let count_chars_before_width ~width:visual_width utf_chars =
  List.fold_until
    utf_chars
    ~finish:(fun (~width:_, ~count) -> count)
    ~init:(~width:0, ~count:0)
    ~f:(fun (~width, ~count) a ->
      match width >= visual_width with
      | true -> Stop count
      | false ->
        let width = width + View.uchar_tty_width a in
        Continue (~width, ~count:(count + 1)))
;;

let prev_line ~ideal_visual_column ~zed_context ~width =
  (* NOTE: We are repeatedly computing [compute_text] - consider de-duplicating this
     compoutation. This is also very much not optimized at all. The priority here is a
     nice UX. *)
  let ~text, .. = compute_text ~zed_context in
  let%tydi { visual_line; visual_column; logical_column; logical_line = _; _ } =
    compute_cursor ~zed_context ~text ~width
  in
  let is_at_first_line = visual_line = 0 in
  match is_at_first_line with
  | true -> Zed_edit.prev_line zed_context
  | false ->
    let wrapped_lines = wrap_text ~text ~max_width:width in
    (match List.nth wrapped_lines visual_line with
     | None ->
       if visual_column = 0
       then (
         (* NOTE: We are in the "fake" last virtual line right after a wrapped line. *)
         match List.nth wrapped_lines (visual_line - 1) with
         | None -> Zed_edit.prev_line zed_context
         | Some (_, prev_line) ->
           Zed_edit.move
             zed_context
             ~set_wanted_column:false
             (-List.length (utf8_chars_of_string prev_line)))
       else Zed_edit.prev_line zed_context
     | Some (_, current_line) ->
       (* NOTE: We have a 0 check previously, so this should always be true. *)
       assert (visual_line >= 1);
       let _, prev_line = List.nth_exn wrapped_lines (visual_line - 1) in
       let prev_line_chars = utf8_chars_of_string prev_line in
       let current_line_chars_before_cursor =
         count_chars_before_width ~width:visual_column (utf8_chars_of_string current_line)
       in
       let num_chars_on_prev_line_before_target =
         count_chars_before_width ~width:ideal_visual_column prev_line_chars
       in
       let is_wrapped = logical_column <> visual_column in
       let delta =
         List.length prev_line_chars
         - num_chars_on_prev_line_before_target
         + current_line_chars_before_cursor
         + if is_wrapped then 0 else 1
       in
       let delta = -delta in
       Zed_edit.move zed_context ~set_wanted_column:false delta)
;;

let next_line ~ideal_visual_column ~zed_context ~width =
  (* NOTE: We are repeatedly computing [compute_text] - consider de-duplicating this
     compoutation. This is also very much not optimized at all. The priority here is a
     nice UX. *)
  let ~text, .. = compute_text ~zed_context in
  let%tydi { visual_line; visual_column; logical_column = _; logical_line = _; _ } =
    compute_cursor ~zed_context ~text ~width
  in
  let wrapped_lines = wrap_text ~text ~max_width:width in
  match List.nth wrapped_lines visual_line with
  | None -> Zed_edit.next_line zed_context
  | Some (_, current_line) ->
    let current_line_chars = utf8_chars_of_string current_line in
    (match List.nth wrapped_lines (visual_line + 1) with
     | None -> Zed_edit.next_line zed_context
     | Some (next_line_is_wrapped, next_line) ->
       let next_line_chars = utf8_chars_of_string next_line in
       let current_line_chars_before_cursor =
         count_chars_before_width ~width:visual_column current_line_chars
       in
       let num_chars_on_next_line_before_target =
         count_chars_before_width ~width:ideal_visual_column next_line_chars
       in
       let next_line_is_wrapped =
         match next_line_is_wrapped with
         | `Wrapped -> true
         | `Not_wrapped -> false
       in
       let delta =
         List.length current_line_chars
         - current_line_chars_before_cursor
         + num_chars_on_next_line_before_target
         + if next_line_is_wrapped then 0 else 1
       in
       Zed_edit.move zed_context ~set_wanted_column:false delta)
;;

let goto_first_non_whitespace_character_in_line ~zed_context =
  let current_line =
    let logical_line = Zed_cursor.get_line (Zed_edit.cursor zed_context) in
    let ~text, .. = compute_text ~zed_context in
    List.nth (String.split ~on:'\n' text) logical_line
  in
  let num_leading_whitespace_chars =
    match current_line with
    | None -> 0
    | Some current_line ->
      List.length
        (List.take_while
           (utf8_chars_of_string current_line)
           ~f:is_leading_whitespace_uchar)
  in
  (Zed.Zed_edit.get_action `goto_bol) zed_context;
  Zed_edit.move zed_context ~set_wanted_column:false num_leading_whitespace_chars
;;

let move_to_position ~zed_context ~position ~new_position =
  match position = new_position with
  | true -> ()
  | false ->
    let delta = new_position - position in
    Zed_edit.move zed_context ~set_wanted_column:false delta
;;

module Forward_or_backward = struct
  type t =
    | Forward
    | Backward

  let flip = function
    | Forward -> Backward
    | Backward -> Forward
  ;;

  let update = function
    | Forward -> succ
    | Backward -> pred
  ;;
end

let move_to_end_like_vim ~zed_context ~position ~find_end_from =
  match find_end_from position with
  | None -> ()
  | Some new_position when new_position > position ->
    move_to_position ~zed_context ~position ~new_position
  | Some _ (* already at end of group; move to next group *) ->
    (match find_end_from (position + 1) with
     | None -> ()
     | Some new_position -> move_to_position ~zed_context ~position ~new_position)
;;

let end_word_generic ~(word_kind : WORD_or_word.t) ~zed_context =
  (* This implements the VIM [e] and [E] commands. *)
  let rope = Zed_edit.text (Zed_edit.edit zed_context) in
  let position = Zed_edit.position zed_context in
  let len = Zed_rope.length rope in
  if position >= len
  then ()
  else (
    let find_end_from start =
      if start >= len
      then None
      else (
        let start = skip_forward_while ~rope ~position:start ~f:is_whitespace in
        if start >= len
        then None
        else (
          let in_group =
            match word_kind with
            | WORD -> fun c -> not (is_whitespace c)
            | Word ->
              let start_char = Zed_rope.get rope start in
              (match is_word_char start_char with
               | true -> is_word_char
               | false -> fun c -> (not (is_whitespace c)) && not (is_word_char c))
          in
          Some (group_end_forward_inclusive ~rope ~position:start ~in_group)))
    in
    move_to_end_like_vim ~zed_context ~position ~find_end_from)
;;

let next_or_prev_word_generic
  ~(forward_or_backward : Forward_or_backward.t)
  ~(word_kind : WORD_or_word.t)
  ~zed_context
  =
  let rope = Zed_edit.text (Zed_edit.edit zed_context) in
  let position = Zed_edit.position zed_context in
  let len = Zed_rope.length rope in
  let in_group_at pos =
    match word_kind with
    | WORD -> fun c -> not (is_whitespace c)
    | Word ->
      let ch = Zed_rope.get rope pos in
      (match is_word_char ch with
       | true -> is_word_char
       | false -> fun c -> (not (is_whitespace c)) && not (is_word_char c))
  in
  let new_position =
    match forward_or_backward with
    | Forward ->
      if position >= len
      then position
      else (
        let in_group = in_group_at position in
        let position = skip_forward_while ~rope ~position ~f:in_group in
        skip_forward_while ~rope ~position ~f:is_whitespace)
    | Backward ->
      if position <= 0
      then position
      else (
        let position =
          skip_backward_while ~rope ~position:(position - 1) ~f:is_whitespace
        in
        let in_group = in_group_at position in
        group_start_backward ~rope ~position ~in_group)
  in
  move_to_position ~zed_context ~position ~new_position
;;

let next_WORD ~zed_context =
  next_or_prev_word_generic ~forward_or_backward:Forward ~word_kind:WORD ~zed_context
;;

let prev_WORD ~zed_context =
  next_or_prev_word_generic ~forward_or_backward:Backward ~word_kind:WORD ~zed_context
;;

let next_word ~zed_context =
  next_or_prev_word_generic ~forward_or_backward:Forward ~word_kind:Word ~zed_context
;;

let prev_word ~zed_context =
  next_or_prev_word_generic ~forward_or_backward:Backward ~word_kind:Word ~zed_context
;;

let end_word ~zed_context = end_word_generic ~word_kind:Word ~zed_context
let end_WORD ~zed_context = end_word_generic ~word_kind:WORD ~zed_context

let next_or_prev_word_emacs ~(forward_or_backward : Forward_or_backward.t) ~zed_context =
  let position_before = Zed_edit.position zed_context in
  (* NOTE: We don't exactly re-use the vim [next_word] / prev word initially because the
     emacs and vim nextword have slightly different semantics.
  *)
  let zed_action =
    match forward_or_backward with
    | Forward -> `next_word
    | Backward -> `prev_word
  in
  (Zed.Zed_edit.get_action zed_action) zed_context;
  let position_after = Zed_edit.position zed_context in
  if position_after = position_before
  then (
    let rope = Zed_edit.text (Zed_edit.edit zed_context) in
    match forward_or_backward with
    | Forward ->
      if position_before < Zed_rope.length rope
      then (
        let current_char = Zed_rope.get rope position_before in
        if (not (is_whitespace current_char)) && not (is_word_char current_char)
        then next_word ~zed_context)
    | Backward ->
      if position_before > 0
      then (
        let previous_char = Zed_rope.get rope (position_before - 1) in
        if (not (is_whitespace previous_char)) && not (is_word_char previous_char)
        then prev_word ~zed_context))
;;

let next_word_emacs ~zed_context =
  next_or_prev_word_emacs ~forward_or_backward:Forward ~zed_context
;;

let prev_word_emacs ~zed_context =
  next_or_prev_word_emacs ~forward_or_backward:Backward ~zed_context
;;

let zed_char_equal a b = Int.equal (Zed_char.compare a b) 0

module Till_or_find = struct
  (* [Till] is for [t] commands and [Find] if for vim [f] commands. *)
  type t =
    | Till
    | Find
end

let find_character_generic
  ~(till_or_find : Till_or_find.t)
  ~(forward_or_backward : Forward_or_backward.t)
  ~zed_context
  ~char
  =
  let rope = Zed_edit.text (Zed_edit.edit zed_context) in
  let position = Zed_edit.position zed_context in
  let len = Zed_rope.length rope in
  let target = Zed_char.of_char char in
  let update = Forward_or_backward.update forward_or_backward in
  let rec loop i =
    match i >= len || i < 0 with
    | true -> None
    | false ->
      let current_char = Zed_rope.get rope i in
      (match zed_char_equal current_char target with
       | true -> Some i
       | false ->
         (match zed_char_equal current_char (Zed_char.of_char '\n') with
          | true -> None
          | false -> loop (update i)))
  in
  let initialize_search_position i =
    match till_or_find with
    | Till ->
      (* NOTE: In [t] we advance twice when we begin search so that repeatedly pressing tx
         will jump to the next characters. *)
      update (update i)
    | Find -> update i
  in
  match loop (initialize_search_position position) with
  | None -> ()
  | Some new_position ->
    let new_position =
      match till_or_find with
      | Find -> new_position
      | Till ->
        Forward_or_backward.update
          (Forward_or_backward.flip forward_or_backward)
          new_position
    in
    move_to_position ~zed_context ~position ~new_position
;;

let find_char_forward ~zed_context ~char =
  find_character_generic
    ~till_or_find:Find
    ~forward_or_backward:Forward
    ~zed_context
    ~char
;;

let find_char_backward ~zed_context ~char =
  find_character_generic
    ~till_or_find:Find
    ~forward_or_backward:Backward
    ~zed_context
    ~char
;;

let till_char_forward ~zed_context ~char =
  find_character_generic
    ~till_or_find:Till
    ~forward_or_backward:Forward
    ~zed_context
    ~char
;;

let till_char_backward ~zed_context ~char =
  find_character_generic
    ~till_or_find:Till
    ~forward_or_backward:Backward
    ~zed_context
    ~char
;;

let clear ~zed_context =
  let rope = Zed_edit.text (Zed_edit.edit zed_context) in
  match Zed_rope.length rope with
  | 0 -> ()
  | len ->
    let position = Zed_edit.position zed_context in
    move_to_position ~zed_context ~position ~new_position:0;
    Zed_edit.remove_next zed_context len
;;

let delete_text_object ~zed_context ({ Text_object.start; stop } : Text_object.t) =
  if start < stop
  then (
    let position = Zed_edit.position zed_context in
    move_to_position ~zed_context ~position ~new_position:start;
    (Zed.Zed_edit.get_action `set_mark) zed_context;
    move_to_position ~zed_context ~position:start ~new_position:stop;
    (Zed.Zed_edit.get_action `kill) zed_context)
;;

let execute_vim_text_object_command ~zed_context (command : Vim_text_object_command.t) =
  Option.iter
    (text_object_of_vim_command ~zed_context command)
    ~f:(delete_text_object ~zed_context)
;;

module State_manager = struct
  module Model = struct
    type t =
      { update_count : int
      ; ideal_visual_column : int
      ; scroll_offset : int
      }
  end

  module Input = struct
    type t =
      { zed_context : unit Zed_edit.context
      ; width : int
      ; max_height : int
      }
  end

  module Editor_action = Action

  module Action = struct
    type t =
      | Send_actions of Action.t Nonempty_list.t
      | Set_text of string
      | Dimensions_changed
  end

  let apply_action
    _
    (input : Input.t Bonsai.Computation_status.t)
    (model : Model.t)
    (action : Action.t)
    =
    let update_count = succ model.update_count in
    let get_visual_lines () =
      match input with
      | Inactive -> None
      | Active { zed_context; width; _ } ->
        let ~text, .. = compute_text ~zed_context in
        Some (wrap_text ~text ~max_width:width)
    in
    let lines_before = get_visual_lines () in
    let () =
      match action with
      | Send_actions actions ->
        let apply_action action =
          match input with
          | Inactive -> ()
          | Active { zed_context; width; max_height = _ } ->
            (match Editor_action.to_zed_action action with
             | `Forward_to_zed action -> (Zed.Zed_edit.get_action action) zed_context
             | `Custom `prev_line ->
               prev_line
                 ~ideal_visual_column:model.ideal_visual_column
                 ~width
                 ~zed_context
             | `Custom `next_line ->
               next_line
                 ~ideal_visual_column:model.ideal_visual_column
                 ~width
                 ~zed_context
             | `Custom `goto_first_non_whitespace_character_in_line ->
               goto_first_non_whitespace_character_in_line ~zed_context
             | `Custom `kill_next_char ->
               (Zed.Zed_edit.get_action `set_mark) zed_context;
               (Zed.Zed_edit.get_action `next_char) zed_context;
               (Zed.Zed_edit.get_action `kill) zed_context
             | `Custom (`replace_char s) ->
               (Zed.Zed_edit.get_action `delete_next_char) zed_context;
               (Zed.Zed_edit.get_action (`insert s)) zed_context;
               (Zed.Zed_edit.get_action `prev_char) zed_context
             | `Custom `clear -> clear ~zed_context
             | `Custom `next_word_emacs -> next_word_emacs ~zed_context
             | `Custom `prev_word_emacs -> prev_word_emacs ~zed_context
             | `Custom `next_word_vim -> next_word ~zed_context
             | `Custom `prev_word_vim -> prev_word ~zed_context
             | `Custom `next_WORD -> next_WORD ~zed_context
             | `Custom `prev_WORD -> prev_WORD ~zed_context
             | `Custom `end_word -> end_word ~zed_context
             | `Custom `end_WORD -> end_WORD ~zed_context
             | `Custom (`find_char_forward c) -> find_char_forward ~zed_context ~char:c
             | `Custom (`find_char_backward c) -> find_char_backward ~zed_context ~char:c
             | `Custom (`till_char_forward c) -> till_char_forward ~zed_context ~char:c
             | `Custom (`till_char_backward c) -> till_char_backward ~zed_context ~char:c
             | `Custom (`execute_vim_text_object_command command) ->
               execute_vim_text_object_command ~zed_context command
             | `Custom `prev_char_in_line ->
               if Zed_edit.at_bol zed_context
               then ()
               else (Zed.Zed_edit.get_action `prev_char) zed_context)
        in
        Nonempty_list.iter actions ~f:apply_action
      | Set_text text ->
        (match input with
         | Inactive -> ()
         | Active { zed_context; width = _; max_height = _ } ->
           let rope = Zed.Zed_rope.of_string text in
           Zed.Zed_edit.set_text_and_forget_history zed_context rope)
      | Dimensions_changed ->
        (* NOTE: When dimensions change, we don't update the text editor state, we solely
           let the other parts of the apply action function re-update. *)
        ()
    in
    let ideal_visual_column =
      let should_update_ideal_visual_column =
        (* We do not want to update the visual column if for [prev_line/next_line] events. *)
        match action with
        | Set_text _ | Dimensions_changed -> `Update_ideal_column
        | Send_actions actions ->
          (match
             Nonempty_list.for_all actions ~f:(function
               | Prev_line | Next_line -> true
               | _ -> false)
           with
           | true -> `Keep_ideal_column
           | false -> `Update_ideal_column)
      in
      match should_update_ideal_visual_column with
      | `Keep_ideal_column -> model.ideal_visual_column
      | `Update_ideal_column ->
        (match input with
         | Inactive -> model.ideal_visual_column
         | Active { zed_context; width; max_height = _ } ->
           let%tydi { visual_column; _ } =
             let ~text, .. = compute_text ~zed_context in
             compute_cursor ~zed_context ~text ~width
           in
           visual_column)
    in
    let scroll_offset =
      match input with
      | Inactive -> model.scroll_offset
      | Active { zed_context; width; max_height } ->
        let ~text, .. = compute_text ~zed_context in
        let%tydi { visual_line; _ } = compute_cursor ~zed_context ~text ~width in
        let current_offset = model.scroll_offset in
        let scroll_offset =
          ensure_cursor_visible ~visual_line ~scroll_offset:current_offset ~max_height
        in
        if scroll_offset <> current_offset
        then scroll_offset
        else (
          (* Cursor is still in view, adjust for content shrinkage *)
          match current_offset with
          | 0 -> current_offset
          | _ ->
            (match
               let%map.Option lines_after = get_visual_lines ()
               and lines_before
               and max_height =
                 match input with
                 | Inactive -> None
                 | Active { max_height; _ } -> Some max_height
               in
               let num_lines_before = List.length lines_before
               and num_lines_after = List.length lines_after in
               let diff =
                 match num_lines_after > max_height with
                 | false -> 0
                 | true -> Int.max 0 (num_lines_before - num_lines_after)
               in
               diff
             with
             | None -> current_offset
             | Some diff -> current_offset - diff))
    in
    { Model.update_count; ideal_visual_column; scroll_offset }
  ;;
end

let component
  ?match_word
  ?undo_size
  ?(highlights = Bonsai.return (fun (_ : string) -> []))
  ~text_attrs
  ~width
  ~max_height
  (local_ graph)
  =
  let path_id = Bonsai.path graph in
  let width =
    let%arr width in
    Int.max width maximum_character_size
  in
  let max_height =
    let%arr max_height in
    Int.max 1 max_height
  in
  let zed_context =
    Bonsai.Expert.thunk
      ~f:(fun () ->
        let edit = Zed_edit.create ?match_word ?undo_size () in
        let cursor = Zed_edit.new_cursor edit in
        Zed_edit.context edit cursor)
      graph
  in
  let model, inject =
    Bonsai.state_machine_with_input
      ~default_model:
        { State_manager.Model.update_count = 0
        ; ideal_visual_column = 0
        ; scroll_offset = 0
        }
      ~apply_action:State_manager.apply_action
      (let%arr zed_context and width and max_height in
       { State_manager.Input.zed_context; width; max_height })
      graph
  in
  (* When dimensions change, dispatch a no-op action so the state machine recomputes
     [scroll_offset] with the new [width] and [max_height]. *)
  Bonsai.Edge.on_change
    (let%arr width and max_height in
     width, max_height)
    ~equal:[%equal: int * int]
    ~callback:
      (let%arr inject in
       fun (_ : int * int) -> inject Dimensions_changed)
    graph;
  let%sub { update_count = count; ideal_visual_column = _; scroll_offset } = model in
  let send_actions =
    let%arr inject in
    fun actions -> inject (Send_actions actions)
  in
  let set_text =
    let%arr inject in
    fun text -> inject (Set_text text)
  in
  let%sub ~text, ~rope =
    let%arr zed_context and count in
    let _ = count in
    compute_text ~zed_context
  in
  let cursor =
    let%arr zed_context and count and text and width in
    let _ = count in
    let cursor = compute_cursor ~zed_context ~text ~width in
    { cursor with visual_line = Int.max 0 cursor.visual_line }
  in
  let full_view =
    let full_view =
      let%arr text and text_attrs and width and highlights in
      let wrapped_lines = wrap_text ~text ~max_width:width in
      render_wrapped_lines ~wrapped_lines ~text_attrs ~highlights:(highlights text)
    in
    let%arr full_view
    and { visual_column; visual_line; _ } = cursor
    and path_id in
    let cursor = View.transparent_rectangle ~width:1 ~height:1 in
    let cursor = View.Tag.mark cursor ~id:cursor_tag ~key:path_id ~f:Fn.id in
    let view = View.zcat [ View.pad ~l:visual_column ~t:visual_line cursor; full_view ] in
    view
  in
  let view =
    let%arr scroll_offset
    and full_view
    and max_height
    and { visual_line; _ } = cursor in
    (* The [scroll_offset] from the state machine may be stale if dimensions changed
       without an editor action being dispatched (e.g. during a frame gap). We correct it
       here at render time to ensure the cursor is always visible and the view is not
       over-cropped during the frame gap frame. *)
    let total_lines = View.height full_view in
    let scroll_offset =
      let scroll_offset =
        (* Clamp: don't scroll past the content *)
        if total_lines <= max_height
        then 0
        else Int.min scroll_offset (total_lines - max_height)
      in
      ensure_cursor_visible ~visual_line ~scroll_offset ~max_height
    in
    let view = View.crop ~t:scroll_offset full_view in
    let too_big_by = Int.max 0 (View.height view - max_height) in
    View.crop ~b:too_big_by view
  in
  let get_cursor_position =
    let%arr path_id in
    fun view ->
      let%map.Option { x; y; _ } = View.Tag.find view ~id:cursor_tag path_id in
      { Position.x; y }
  in
  { text; send_actions; view; cursor; set_text; rope; get_cursor_position }
;;

let default_keybindings_handler
  :  (Action.t Nonempty_list.t -> unit Effect.t) -> Event.t
  -> Captured_or_ignored.t Effect.t
  =
  fun send_actions ->
  let send_action ~(here : [%call_pos]) action =
    Captured_or_ignored.capture ~here (send_actions [ action ])
  in
  let handler (event : Event.t) =
    match event with
    | Key_press { key = ASCII c; mods = [] } -> send_action (Insert (Char.to_string c))
    | Key_press { key = Uchar c; mods = [] } ->
      send_action (Insert (Uchar.Utf8.to_string c))
    | Key_press { key = Backspace; mods = [] } -> send_action Delete_prev_char
    | Key_press { key = Backspace; mods = [ Meta ] }
    | Key_press { key = Backspace; mods = [ Ctrl ] }
    | Key_press
        { key = ASCII 'W' (* NOTE: In VSCode Ctrl+Backspace is Ctrl+W. *)
        ; mods = [ Ctrl ]
        } -> send_action Delete_prev_word
    | Key_press { key = Delete; mods = [ Ctrl ] }
    | Key_press { key = ASCII 'd'; mods = [ Meta ] } -> send_action Delete_next_word
    | Key_press { key = Arrow `Left; mods = [ Ctrl ] } -> send_action Prev_word
    | Key_press { key = Arrow `Right; mods = [ Ctrl ] } -> send_action Next_word
    | Key_press { key = ASCII 'b'; mods = [ Meta ] } -> send_action Prev_word
    | Key_press { key = ASCII 'f'; mods = [ Meta ] } -> send_action Next_word
    | Key_press { key = Arrow `Left; mods = [] } -> send_action Prev_char
    | Key_press { key = Arrow `Right; mods = [] } -> send_action Next_char
    | Key_press { key = Arrow `Up; mods = [] } -> send_action Prev_line
    | Key_press { key = Arrow `Down; mods = [] } -> send_action Next_line
    | Key_press { key = Enter; mods = [] } -> send_action Newline
    | Key_press { key = ASCII ('A' | 'a'); mods = [ Ctrl ] } -> send_action Goto_bol
    | Key_press { key = ASCII ('E' | 'e'); mods = [ Ctrl ] } -> send_action Goto_eol
    | Key_press { key = ASCII 'U'; mods = [ Ctrl ] } -> send_action Delete_prev_line
    | Key_press { key = ASCII 'Z'; mods = [ Ctrl ] } -> send_action Undo
    | Key_press { key = Home; mods = [] } -> send_action Goto_bol
    | Key_press { key = End; mods = [] } -> send_action Goto_eol
    | Key_press { key = Delete; mods = [] } -> send_action Delete_next_char
    | _ -> Captured_or_ignored.ignore
  in
  handler
;;

module Vim = struct
  module Mode = struct
    type t =
      | Normal
      | Insert
    [@@deriving sexp_of]
  end

  module Command_state = struct
    module Pending_text_object = struct
      type t =
        { verb : Vim_text_object_command.Verb.t
        ; adverb : Vim_text_object_command.Adverb.t
        }
      [@@deriving sexp_of]
    end

    module Waiting_for_second_char = struct
      type t =
        | Delete
        | Change
        | Yank
        | Goto_bot
        | Replace_char
        | Find_char_forward
        | Find_char_backward
        | Till_char_forward
        | Till_char_backward
      [@@deriving sexp_of]
    end

    type t =
      | None
      | Waiting_for_second_char of Waiting_for_second_char.t
      | Waiting_for_text_object_target of Pending_text_object.t
    [@@deriving sexp_of]
  end

  module Yank_type = struct
    type t =
      | Line
      | Char
    [@@deriving sexp_of]
  end

  type t =
    { handler : (Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t
    ; mode : Mode.t Bonsai.t
    }

  module Model = struct
    type t =
      { mode : Mode.t
      ; command_state : Command_state.t
      ; last_yank_type : Yank_type.t
      }

    module Input = struct
      type t = { send_action : Action.t -> unit Effect.t }
    end

    let mode_after_vim_command (command : Vim_text_object_command.t) : Mode.t =
      match command.verb with
      | Change -> Insert
      | Delete -> Normal
    ;;

    let command_state_after_vim_command
      (command : Vim_text_object_command.t)
      last_yank_type
      =
      { mode = mode_after_vim_command command; command_state = None; last_yank_type }
    ;;

    let vim_text_object_command pending target =
      { Vim_text_object_command.verb = pending.Command_state.Pending_text_object.verb
      ; adverb = pending.adverb
      ; target
      }
    ;;

    let recv
      context
      (input : Input.t Bonsai.Computation_status.t)
      (model : t)
      (event : Event.t)
      : t * Captured_or_ignored.t
      =
      let send_action action =
        Bonsai.Apply_action_context.schedule_event
          context
          (match input with
           | Inactive -> Effect.Ignore
           | Active { send_action } -> send_action action)
      in
      let captured model = model, Captured_or_ignored.captured () in
      let ignored model = model, Captured_or_ignored.ignored in
      let%tydi { mode; command_state; last_yank_type } = model in
      match mode, event with
      (* Insert mode handling *)
      | Insert, Key_press { key = Escape; mods = [] } ->
        (* NOTE: This is not intuitive, but repeatedly pressing [esc,i,esc,i,esc,i] will
           gradually move your cursor backwards because pressing [esc] will put you into
           normal mode, but _also_ move your cursor backwards! *)
        send_action Prev_char_in_line;
        captured { mode = Normal; command_state = None; last_yank_type }
      | Insert, Key_press { key = ASCII c; mods = [] } ->
        send_action (Insert (Char.to_string c));
        captured model
      | Insert, Key_press { key = Uchar c; mods = [] } ->
        send_action (Insert (Uchar.Utf8.to_string c));
        captured model
      | Insert, Key_press { key = Backspace; mods = [] } ->
        send_action Delete_prev_char;
        captured model
      | Insert, Key_press { key = Enter; mods = [] } ->
        send_action Newline;
        captured model
      | Insert, Key_press { key = ASCII 'U'; mods = [ Ctrl ] } ->
        send_action Delete_prev_line;
        captured model
      | Insert, Key_press { key = ASCII 'W'; mods = [ Ctrl ] } ->
        send_action Delete_prev_word;
        captured model
      | Insert, Key_press { key = Backspace; mods = [ Ctrl ] } ->
        send_action Delete_prev_word;
        captured model
      | Insert, Key_press { key = Delete; mods = [ Ctrl ] } ->
        send_action Delete_next_word;
        captured model
      (* Normal mode handling *)
      | Normal, Key_press { key = ASCII c; mods = [] } ->
        (match command_state, c with
         (* Movement commands *)
         | None, 'h' ->
           send_action Prev_char;
           captured model
         | None, 'j' ->
           send_action Next_line;
           captured model
         | None, 'k' ->
           send_action Prev_line;
           captured model
         | None, 'l' ->
           send_action Next_char;
           captured model
         | None, 'w' ->
           (* NOTE: The zed Next_word, Prev_words behave slightly different from the vim
              w, b keybindings. We isntead implement our own [Next_word_vim] and
              [Prev_word_vim] commands. *)
           send_action Next_word_vim;
           captured model
         | None, 'b' ->
           send_action Prev_word_vim;
           captured model
         | None, 'e' ->
           send_action End_word;
           captured model
         | None, 'W' ->
           send_action Next_WORD;
           captured model
         | None, 'B' ->
           send_action Prev_WORD;
           captured model
         | None, 'E' ->
           send_action End_WORD;
           captured model
         | None, 'f' ->
           captured
             { model with command_state = Waiting_for_second_char Find_char_forward }
         | None, 'F' ->
           captured
             { model with command_state = Waiting_for_second_char Find_char_backward }
         | None, 't' ->
           captured
             { model with command_state = Waiting_for_second_char Till_char_forward }
         | None, 'T' ->
           captured
             { model with command_state = Waiting_for_second_char Till_char_backward }
         | None, '0' ->
           send_action Goto_bol;
           captured model
         | None, '^' ->
           send_action Goto_first_non_whitespace_character_in_line;
           captured model
         | None, '$' ->
           send_action Goto_eol;
           captured model
         | None, 'G' ->
           send_action Goto_eot;
           captured model
         (* Mode switching *)
         | None, 'i' -> captured { model with mode = Insert }
         | None, 'I' ->
           send_action Goto_bol;
           captured { model with mode = Insert }
         | None, 'a' ->
           send_action Next_char;
           captured { model with mode = Insert }
         | None, 'A' ->
           send_action Goto_eol;
           captured { model with mode = Insert }
         | None, 'o' ->
           send_action Goto_eol;
           send_action Newline;
           captured { model with mode = Insert }
         | None, 'S' ->
           send_action Goto_bol;
           send_action Kill_next_line;
           captured { mode = Insert; command_state = None; last_yank_type = Line }
         | None, 'D' ->
           send_action Kill_next_line;
           captured { model with last_yank_type = Char }
         | None, 'C' ->
           send_action Kill_next_line;
           captured { mode = Insert; command_state = None; last_yank_type = Char }
         | None, 'O' ->
           send_action Goto_bol;
           send_action (Insert "\n");
           send_action Prev_line;
           captured { model with mode = Insert }
         (* Single character commands *)
         | None, 'x' ->
           send_action Kill_next_char;
           captured { model with last_yank_type = Char }
         | None, 'X' ->
           send_action Delete_prev_char;
           captured model
         | None, 's' ->
           send_action Delete_next_char;
           captured { model with mode = Insert }
         | None, 'u' ->
           send_action Undo;
           captured model
         | None, 'p' ->
           (match last_yank_type with
            | Line ->
              (* line-wise paste below current line *)
              send_action Goto_eol;
              send_action Newline;
              send_action Yank
            | Char ->
              (* character-wise paste after cursor, cursor on last char of pasted text *)
              send_action Next_char;
              send_action Yank;
              send_action Prev_char);
           captured model
         | None, 'P' ->
           (match last_yank_type with
            | Line ->
              (* line-wise paste above current line *)
              send_action Goto_bol;
              send_action Yank
            | Char ->
              (* character-wise paste at cursor, cursor on last char of pasted text *)
              send_action Yank;
              send_action Prev_char);
           captured model
         (* Multi-character commands - first char *)
         | None, 'd' ->
           captured { model with command_state = Waiting_for_second_char Delete }
         | None, 'c' ->
           captured { model with command_state = Waiting_for_second_char Change }
         | None, 'y' ->
           captured { model with command_state = Waiting_for_second_char Yank }
         | None, 'g' ->
           captured { model with command_state = Waiting_for_second_char Goto_bot }
         | None, 'r' ->
           captured { model with command_state = Waiting_for_second_char Replace_char }
         (* Multi-character commands - second char *)
         | Waiting_for_second_char Delete, 'd' ->
           send_action Goto_bol;
           send_action Kill_next_line;
           captured { mode = Normal; command_state = None; last_yank_type = Line }
         | Waiting_for_second_char Delete, 'b' ->
           send_action Kill_prev_word;
           captured { mode = Normal; command_state = None; last_yank_type = Char }
         | Waiting_for_second_char Delete, '0' ->
           send_action Kill_prev_line;
           captured { mode = Normal; command_state = None; last_yank_type = Char }
         | Waiting_for_second_char Delete, '$' ->
           send_action Kill_next_line;
           captured { mode = Normal; command_state = None; last_yank_type = Char }
         | Waiting_for_second_char Delete, 'w' ->
           send_action Kill_next_word;
           captured { model with command_state = None }
         | Waiting_for_second_char Delete, 'a' ->
           let pending : Command_state.Pending_text_object.t =
             { verb = Delete; adverb = Around }
           in
           captured { model with command_state = Waiting_for_text_object_target pending }
         | Waiting_for_second_char Delete, 'i' ->
           let pending : Command_state.Pending_text_object.t =
             { verb = Delete; adverb = Inside }
           in
           captured { model with command_state = Waiting_for_text_object_target pending }
         | Waiting_for_second_char Change, 'e' ->
           send_action Kill_next_word;
           captured { mode = Insert; command_state = None; last_yank_type = Line }
         | Waiting_for_second_char Change, 'w' ->
           send_action Kill_next_word;
           captured { mode = Insert; command_state = None; last_yank_type = Char }
         | Waiting_for_second_char Change, 'b' ->
           send_action Kill_prev_word;
           captured { mode = Insert; command_state = None; last_yank_type = Char }
         | Waiting_for_second_char Change, 'c' ->
           send_action Goto_bol;
           send_action Kill_next_line;
           captured { mode = Insert; command_state = None; last_yank_type = Line }
         | Waiting_for_second_char Change, 'a' ->
           let pending : Command_state.Pending_text_object.t =
             { verb = Change; adverb = Around }
           in
           captured { model with command_state = Waiting_for_text_object_target pending }
         | Waiting_for_second_char Change, 'i' ->
           let pending : Command_state.Pending_text_object.t =
             { verb = Change; adverb = Inside }
           in
           captured { model with command_state = Waiting_for_text_object_target pending }
         | Waiting_for_second_char Goto_bot, 'g' ->
           send_action Goto_bot;
           captured { model with command_state = None }
         | Waiting_for_second_char Replace_char, c ->
           send_action (Replace_char (Char.to_string c));
           captured { model with command_state = None }
         | Waiting_for_second_char Find_char_forward, c ->
           send_action (Find_char_forward c);
           captured { model with command_state = None }
         | Waiting_for_second_char Find_char_backward, c ->
           send_action (Find_char_backward c);
           captured { model with command_state = None }
         | Waiting_for_second_char Till_char_forward, c ->
           send_action (Till_char_forward c);
           captured { model with command_state = None }
         | Waiting_for_second_char Till_char_backward, c ->
           send_action (Till_char_backward c);
           captured { model with command_state = None }
         | Waiting_for_text_object_target pending, target_char ->
           (match Vim_text_object_command.Target.of_char target_char with
            | Some target ->
              let command = vim_text_object_command pending target in
              send_action (Vim_text_object_command command);
              captured (command_state_after_vim_command command Char)
            | None -> captured { model with command_state = None })
         (* Cancel multi-character command on any other key *)
         | Waiting_for_second_char _, _ -> captured { model with command_state = None }
         | None, _ -> ignored model)
      (* Handle arrow keys in both modes *)
      | Insert, Key_press { key = Arrow `Left; mods = [] } ->
        send_action Prev_char;
        captured model
      | Insert, Key_press { key = Arrow `Right; mods = [] } ->
        send_action Next_char;
        captured model
      | Insert, Key_press { key = Arrow `Up; mods = [] } ->
        send_action Prev_line;
        captured model
      | Insert, Key_press { key = Arrow `Down; mods = [] } ->
        send_action Next_line;
        captured model
      (* Ctrl+Arrow for word movement in insert mode *)
      | Insert, Key_press { key = Arrow `Left; mods = [ Ctrl ] } ->
        send_action Prev_word;
        captured model
      | Insert, Key_press { key = Arrow `Right; mods = [ Ctrl ] } ->
        send_action Next_word;
        captured model
      | Normal, Key_press { key = Arrow `Left; mods = [] } ->
        send_action Prev_char;
        captured model
      | Normal, Key_press { key = Arrow `Right; mods = [] } ->
        send_action Next_char;
        captured model
      (* Ctrl+Arrow for word movement in normal mode *)
      | Normal, Key_press { key = Arrow `Left; mods = [ Ctrl ] } ->
        send_action Prev_word;
        captured model
      | Normal, Key_press { key = Arrow `Right; mods = [ Ctrl ] } ->
        send_action Next_word;
        captured model
      | Normal, Key_press { key = Arrow `Up; mods = [] } ->
        send_action Prev_line;
        captured model
      | Normal, Key_press { key = Arrow `Down; mods = [] } ->
        send_action Next_line;
        captured model
      | Normal, Key_press { key = Escape; mods = [] } ->
        captured { model with command_state = None }
      | (Normal | Insert), Key_press { key = Home; mods = [] } ->
        send_action Goto_bol;
        captured { model with command_state = None }
      | (Normal | Insert), Key_press { key = End; mods = [] } ->
        send_action Goto_eol;
        captured { model with command_state = None }
      | (Normal | Insert), Key_press { key = Delete; mods = [] } ->
        send_action Delete_next_char;
        captured { model with command_state = None }
      | _, _ -> ignored model
    ;;
  end

  let vim_keybindings_handler
    :  default_mode:Mode.t -> (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
    -> local_ Bonsai.graph -> t
    =
    fun ~default_mode send_actions (local_ graph) ->
    let send_action =
      let%arr send_actions in
      fun action -> send_actions [ action ]
    in
    let model, handler =
      (* NOTE: We intentionally use [actor_with_input] to avoid frame gaps when switching
         modes / performing sequences of keystrokes. It is somewhat important to make this
         as snappy as we can (within reason). The [recv] function returns
         [Captured_or_ignored.t] directly so that callers know whether the event was
         handled. *)
      Bonsai.actor_with_input
        ~default_model:
          { Model.mode = default_mode; command_state = None; last_yank_type = Char }
        ~recv:Model.recv
        (let%arr send_action in
         { Model.Input.send_action })
        graph
    in
    let mode =
      let%arr { mode; command_state = _; last_yank_type = _ } = model in
      mode
    in
    { mode; handler }
  ;;
end

module Emacs = struct
  let emacs_keybindings_handler
    :  (Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t -> local_ Bonsai.graph
    -> (Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t
    =
    fun send_actions (local_ _graph) ->
    let%arr send_actions in
    let send_action ~(here : [%call_pos]) action =
      Captured_or_ignored.capture ~here (send_actions [ action ])
    in
    let handler (event : Event.t) =
      match event with
      (* Basic character insertion *)
      | Key_press { key = ASCII c; mods = [] } -> send_action (Insert (Char.to_string c))
      | Key_press { key = Uchar c; mods = [] } ->
        send_action (Insert (Uchar.Utf8.to_string c))
      | Key_press { key = Enter; mods = [] } -> send_action Newline
      | Key_press { key = Backspace; mods = [] } -> send_action Delete_prev_char
      (* Emacs-style Ctrl keybindings *)
      | Key_press { key = ASCII 'A'; mods = [ Ctrl ] } -> send_action Goto_bol
      | Key_press { key = ASCII 'E'; mods = [ Ctrl ] } -> send_action Goto_eol
      | Key_press { key = ASCII 'F'; mods = [ Ctrl ] } -> send_action Next_char
      | Key_press { key = ASCII 'B'; mods = [ Ctrl ] } -> send_action Prev_char
      | Key_press { key = ASCII 'N'; mods = [ Ctrl ] } -> send_action Next_line
      | Key_press { key = ASCII 'P'; mods = [ Ctrl ] } -> send_action Prev_line
      | Key_press { key = ASCII 'D'; mods = [ Ctrl ] } -> send_action Delete_next_char
      | Key_press { key = ASCII 'H'; mods = [ Ctrl ] } -> send_action Delete_prev_char
      | Key_press { key = ASCII 'K'; mods = [ Ctrl ] } -> send_action Kill_next_line
      | Key_press { key = ASCII 'U'; mods = [ Ctrl ] } -> send_action Kill_prev_line
      | Key_press { key = ASCII 'Y'; mods = [ Ctrl ] } -> send_action Yank
      (* Word movement *)
      | Key_press { key = Arrow `Right; mods = [ Meta ] } -> send_action Next_word
      | Key_press { key = Arrow `Left; mods = [ Meta ] } -> send_action Prev_word
      | Key_press { key = ASCII 'f'; mods = [ Meta ] } -> send_action Next_word
      | Key_press { key = ASCII 'b'; mods = [ Meta ] } -> send_action Prev_word
      (* Word deletion *)
      | Key_press { key = Delete; mods = [ Ctrl ] }
      | Key_press { key = ASCII 'd'; mods = [ Meta ] } -> send_action Kill_next_word
      | Key_press { key = ASCII 'W'; mods = [ Ctrl ] }
      | Key_press { key = Backspace; mods = [ Ctrl ] }
      | Key_press { key = Backspace; mods = [ Meta ] } -> send_action Kill_prev_word
      (* Document navigation *)
      | Key_press { key = ASCII '<'; mods = [ Meta; Shift ] } -> send_action Goto_bot
      | Key_press { key = ASCII '>'; mods = [ Meta; Shift ] } -> send_action Goto_eot
      (* Undo *)
      | Key_press { key = ASCII 'Z'; mods = [ Ctrl ] } -> send_action Undo
      | Key_press { key = ASCII '_'; mods = [ Ctrl ] } -> send_action Undo
      (* Arrow keys (standard behavior) *)
      | Key_press { key = Arrow `Left; mods = [] } -> send_action Prev_char
      | Key_press { key = Arrow `Right; mods = [] } -> send_action Next_char
      | Key_press { key = Arrow `Up; mods = [] } -> send_action Prev_line
      | Key_press { key = Arrow `Down; mods = [] } -> send_action Next_line
      (* Ctrl+Arrow for word movement (common in many editors) *)
      | Key_press { key = Arrow `Right; mods = [ Ctrl ] } -> send_action Next_word
      | Key_press { key = Arrow `Left; mods = [ Ctrl ] } -> send_action Prev_word
      (* Home/End/Delete keys *)
      | Key_press { key = Home; mods = [] } -> send_action Goto_bol
      | Key_press { key = End; mods = [] } -> send_action Goto_eol
      | Key_press { key = Delete; mods = [] } -> send_action Delete_next_char
      | _ -> Captured_or_ignored.ignore
    in
    handler
  ;;
end

let restrict_to_single_line send_actions (actions : Action.t Nonempty_list.t) =
  let actions =
    Nonempty_list.to_list actions
    |> List.filter ~f:(fun x ->
      match (x : Action.t) with
      | Newline | Next_line | Prev_line | Goto_bot | Goto_eot ->
        (* NOTE: This text editor is a single line text editor. *)
        false
      | Insert _
      | Next_char
      | Prev_char
      | Prev_char_in_line
      | Goto_bol
      | Goto_eol
      | Delete_next_char
      | Delete_prev_char
      | Delete_next_line
      | Delete_prev_line
      | Kill_next_line
      | Kill_prev_line
      | Kill_next_char
      | Next_word
      | Prev_word
      | Delete_next_word
      | Delete_prev_word
      | Kill_next_word
      | Kill_prev_word
      | Yank
      | Undo
      | Clear
      | Replace_char _
      | Goto_first_non_whitespace_character_in_line
      | Next_word_vim
      | Prev_word_vim
      | Next_WORD
      | Prev_WORD
      | End_word
      | End_WORD
      | Find_char_forward _
      | Find_char_backward _
      | Till_char_forward _
      | Till_char_backward _
      | Vim_text_object_command _ -> true)
  in
  match actions with
  | [] -> Effect.Ignore
  | hd :: tl -> send_actions (Nonempty_list.create hd tl)
;;

module Buffer_and_apply_paste_events_in_bulk = struct
  (** [Buffer_and_apply_paste_events_in_bunk.f] will make pasting fast.

      When a paste begins, it will not immediately send the event to the editor, and will
      instead buffer the paste event's keystrokes. It will send the events to the text
      editor only when it sees that the paste has finished. *)
  let f
    :  send_actions:(Action.t Nonempty_list.t -> unit Effect.t) Bonsai.t
    -> handler:(Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t -> local_ Bonsai.graph
    -> (Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t
    =
    fun ~send_actions ~handler (local_ graph) ->
    let open struct
      module Model = struct
        type t =
          { inside_of_paste : bool
          ; paste_buffer :
              [ `Char of char | `Uchar of Uchar.t | `Newline ] Reversed_list.t
          }
      end

      module Input = struct
        type t =
          { handler : Event.t -> Captured_or_ignored.t Effect.t
          ; send_actions : Action.t Nonempty_list.t -> unit Effect.t
          }
      end

      let recv
        context
        (input : Input.t Bonsai.Computation_status.t)
        (model : Model.t)
        (event : Event.t)
        : Model.t * Captured_or_ignored.t Effect.t
        =
        let[@inline always] forward_event event =
          match input with
          | Inactive -> Captured_or_ignored.ignore
          | Active { handler; _ } ->
            (* We ignore the return value here since we're inside apply_action which
               doesn't return a value directly. We track the captured status separately. *)
            Effect.Expert.of_fun ~f:(fun ~callback ~on_exn:_ ->
              Bonsai.Apply_action_context.schedule_event
                context
                (let%bind.Effect (captured_or_ignored : Captured_or_ignored.t) =
                   handler event
                 in
                 callback captured_or_ignored;
                 Effect.return ()))
        in
        let[@inline always] send_actions actions =
          match input with
          | Inactive -> ()
          | Active { send_actions; _ } ->
            Bonsai.Apply_action_context.schedule_event context (send_actions actions)
        in
        let commit_paste_buffer ~paste_buffer =
          (* Combine all paste events into a single string to create only one undo
             checkpoint. This makes undo undo the entire paste operation atomically. *)
          let combined_string =
            paste_buffer
            |> Reversed_list.rev
            |> List.map ~f:(function
              | `Char c -> Char.to_string c
              | `Uchar uchar -> Uchar.Utf8.to_string uchar
              | `Newline -> "\n")
            |> String.concat ~sep:""
          in
          match combined_string with
          | "" -> ()
          | s -> send_actions [ Action.Insert s ]
        in
        let paste_buffer, captured_or_ignored =
          match model.inside_of_paste with
          | false ->
            let captured_or_ignored =
              match event with
              | Paste _ -> Effect.return (Captured_or_ignored.captured ())
              | _ -> forward_event event
            in
            (* For non-paste events, the underlying handler determines captured status. We
               don't have synchronous access to it here, so we conservatively assume
               Captured when forwarding. This is a limitation of the architecture. *)
            model.paste_buffer, captured_or_ignored
          | true ->
            let event, captured_or_ignored =
              let captured x = x, Effect.return (Captured_or_ignored.captured ()) in
              match event with
              | Key_press { key = ASCII char; mods = [] } -> captured (Some (`Char char))
              | Key_press { key = Uchar uchar; mods = [] } ->
                captured (Some (`Uchar uchar))
              | Key_press { key = Enter; mods = [] } -> captured (Some `Newline)
              | _ ->
                (* NOTE: we drop all other events that happen inside of a paste. I think
                   this is fine. *)
                None, Captured_or_ignored.ignore
            in
            ( (match event with
               | None -> model.paste_buffer
               | Some event -> event :: model.paste_buffer)
            , captured_or_ignored )
        in
        let inside_of_paste, paste_buffer =
          match event with
          | Paste `Start -> true, Reversed_list.[]
          | Paste `End ->
            commit_paste_buffer ~paste_buffer;
            false, Reversed_list.[]
          | _ -> model.inside_of_paste, paste_buffer
        in
        { Model.inside_of_paste; paste_buffer }, captured_or_ignored
      ;;
    end in
    let _model, inject =
      Bonsai.actor_with_input
        ~default_model:{ Model.inside_of_paste = false; paste_buffer = [] }
        ~recv
        (let%arr send_actions and handler in
         { Input.send_actions; handler })
        graph
    in
    let%arr inject in
    fun event -> Effect.join (inject event)
  ;;
end

module For_testing = struct
  let wrap_text = wrap_text
end
