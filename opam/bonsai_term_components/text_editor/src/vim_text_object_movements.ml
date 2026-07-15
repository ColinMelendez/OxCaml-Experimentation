open! Core
open Zed

let skip_forward_while ~rope ~position ~f =
  let len = Zed_rope.length rope in
  let pos = ref position in
  while !pos < len && f (Zed_rope.get rope !pos) do
    incr pos
  done;
  !pos
;;

let skip_backward_while ~rope ~position ~f =
  let pos = ref position in
  while !pos > 0 && f (Zed_rope.get rope !pos) do
    decr pos
  done;
  !pos
;;

let group_start_backward ~rope ~position ~in_group =
  let pos = ref position in
  while !pos > 0 && in_group (Zed_rope.get rope (!pos - 1)) do
    decr pos
  done;
  !pos
;;

let group_end_forward_inclusive ~rope ~position ~in_group =
  let len = Zed_rope.length rope in
  let pos = ref position in
  while !pos < len - 1 && in_group (Zed_rope.get rope (!pos + 1)) do
    incr pos
  done;
  !pos
;;

let is_word_char c =
  let[@inline always] is_word_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  Zed_char.is_latin1 c && is_word_char (Zed_char.to_char c)
;;

let zed_char_equal a b = [%compare.equal: Zed_char.t] a b
let is_newline c = zed_char_equal c (Zed_char.of_char '\n')
let is_whitespace c = Zed_char.is_space c
let is_inline_whitespace c = (not (is_newline c)) && is_whitespace c

let find_whitespace_text_object ~rope ~position =
  let start =
    if position = 0
    then 0
    else (
      let start_before_whitespace =
        skip_backward_while ~rope ~position:(position - 1) ~f:is_inline_whitespace
      in
      if is_inline_whitespace (Zed_rope.get rope start_before_whitespace)
      then start_before_whitespace
      else start_before_whitespace + 1)
  in
  let stop = skip_forward_while ~rope ~position ~f:is_inline_whitespace in
  { Text_object.start; stop }
;;

let find_word_text_object ~(word_kind : WORD_or_word.t) ~rope ~position =
  let in_group c =
    match word_kind with
    | WORD -> not (is_whitespace c)
    | Word ->
      let current = Zed_rope.get rope position in
      (match is_word_char current with
       | true -> is_word_char c
       | false -> (not (is_whitespace c)) && not (is_word_char c))
  in
  let start = group_start_backward ~rope ~position ~in_group in
  let stop = group_end_forward_inclusive ~rope ~position ~in_group + 1 in
  { Text_object.start; stop }
;;

let expand_text_object_to_an_around_object ~rope { Text_object.start; stop } =
  let stop_with_trailing_whitespace =
    skip_forward_while ~rope ~position:stop ~f:is_inline_whitespace
  in
  match stop_with_trailing_whitespace > stop with
  | true -> { Text_object.start; stop = stop_with_trailing_whitespace }
  | false ->
    let start =
      match start = 0 with
      | true -> start
      | false ->
        let start_before_whitespace =
          skip_backward_while ~rope ~position:(start - 1) ~f:is_inline_whitespace
        in
        if is_inline_whitespace (Zed_rope.get rope start_before_whitespace)
        then start_before_whitespace
        else start_before_whitespace + 1
    in
    { Text_object.start; stop }
;;

let text_object_for_word
  ~word_kind
  ~(adverb : Vim_text_object_command.Adverb.t)
  ~zed_context
  =
  let rope = Zed_edit.text (Zed_edit.edit zed_context) in
  let position = Zed_edit.position zed_context in
  let len = Zed_rope.length rope in
  match len = 0 with
  | true -> None
  | false ->
    let position = Int.min position (len - 1) in
    let current = Zed_rope.get rope position in
    let%tydi { start; stop } =
      match is_inline_whitespace current with
      | true -> find_whitespace_text_object ~rope ~position
      | false -> find_word_text_object ~word_kind ~rope ~position
    in
    (match adverb with
     | Inside -> Some { Text_object.start; stop }
     | Around -> Some (expand_text_object_to_an_around_object ~rope { start; stop }))
;;

let find_enclosing_pair ~rope ~position ~open_char ~close_char =
  let len = Zed_rope.length rope in
  let open_char = Zed_char.of_char open_char in
  let close_char = Zed_char.of_char close_char in
  let stack = Stack.create () in
  let best = ref None in
  for i = 0 to len - 1 do
    let current = Zed_rope.get rope i in
    if zed_char_equal current open_char
    then Stack.push stack i
    else if zed_char_equal current close_char
    then (
      match Stack.pop stack with
      | None -> ()
      | Some open_position when open_position <= position && position <= i ->
        best := Some (open_position, i)
      | Some _ -> ())
  done;
  !best
;;

let is_unescaped_quote ~rope ~quote_position =
  let rec count_preceding_backslashes count i =
    if i < 0
    then count
    else if zed_char_equal (Zed_rope.get rope i) (Zed_char.of_char '\\')
    then count_preceding_backslashes (count + 1) (i - 1)
    else count
  in
  count_preceding_backslashes 0 (quote_position - 1) % 2 = 0
;;

let find_line_bounds ~rope ~len ~position =
  let line_start =
    let rec loop i =
      if i <= 0
      then 0
      else if zed_char_equal (Zed_rope.get rope (i - 1)) (Zed_char.of_char '\n')
      then i
      else loop (i - 1)
    in
    loop position
  in
  let line_stop =
    let rec loop i =
      if i >= len
      then len
      else if zed_char_equal (Zed_rope.get rope i) (Zed_char.of_char '\n')
      then i
      else loop (i + 1)
    in
    loop position
  in
  line_start, line_stop
;;

let find_enclosing_quote_pair ~rope ~position ~quote_char =
  let len = Zed_rope.length rope in
  let line_start, line_stop = find_line_bounds ~rope ~len ~position in
  let quote_char = Zed_char.of_char quote_char in
  let quote_positions =
    List.init (line_stop - line_start) ~f:(fun offset -> line_start + offset)
    |> List.filter ~f:(fun i ->
      zed_char_equal (Zed_rope.get rope i) quote_char
      && is_unescaped_quote ~rope ~quote_position:i)
  in
  let rec loop = function
    | left :: (right :: _ as rest) ->
      if left <= position && position <= right then Some (left, right) else loop rest
    | _ -> None
  in
  loop quote_positions
;;

let text_object_for_delimited_target
  ~(delimited_target : Vim_text_object_command.Delimited_target.t)
  ~(adverb : Vim_text_object_command.Adverb.t)
  ~zed_context
  =
  let rope = Zed_edit.text (Zed_edit.edit zed_context) in
  let position = Zed_edit.position zed_context in
  let len = Zed_rope.length rope in
  match len = 0 with
  | true -> None
  | false ->
    let position = Int.min position (len - 1) in
    let pair =
      match delimited_target with
      | Parenthesis -> find_enclosing_pair ~rope ~position ~open_char:'(' ~close_char:')'
      | Square_bracket ->
        find_enclosing_pair ~rope ~position ~open_char:'[' ~close_char:']'
      | Curly_bracket ->
        find_enclosing_pair ~rope ~position ~open_char:'{' ~close_char:'}'
      | Double_quote -> find_enclosing_quote_pair ~rope ~position ~quote_char:'"'
      | Single_quote -> find_enclosing_quote_pair ~rope ~position ~quote_char:'\''
    in
    let%bind.Option left, right = pair in
    (match adverb with
     | Inside ->
       let start = left + 1
       and stop = right in
       if start <= stop then Some { Text_object.start; stop } else None
     | Around ->
       let start = left
       and stop = right + 1 in
       if start <= stop then Some { Text_object.start; stop } else None)
;;

let text_object_of_vim_command ~zed_context (command : Vim_text_object_command.t) =
  match command.target with
  | Vim_text_object_command.Target.Word Vim_text_object_command.Word_kind.Word ->
    text_object_for_word ~word_kind:WORD_or_word.Word ~adverb:command.adverb ~zed_context
  | Vim_text_object_command.Target.Word Vim_text_object_command.Word_kind.WORD ->
    text_object_for_word ~word_kind:WORD_or_word.WORD ~adverb:command.adverb ~zed_context
  | Vim_text_object_command.Target.Delimited delimited_target ->
    text_object_for_delimited_target ~delimited_target ~adverb:command.adverb ~zed_context
;;
