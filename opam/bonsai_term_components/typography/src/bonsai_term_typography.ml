open! Core
open Bonsai_term
module Text = Text
module Tty_width = Tty_width

let typeset_line ~max_width line = Typesetting.typeset_line ~max_width line

let typeset ~max_width (text : 'attr Text.t list list) : 'attr Text.t list list =
  List.concat_map text ~f:(fun line ->
    match line with
    | [] -> [ [] ]
    | _ -> typeset_line ~max_width line)
;;

module For_testing = Typesetting.For_testing

let text_wrap ?attrs ~max_width s =
  let lines = String.split_lines s in
  let text =
    List.map lines ~f:(fun line ->
      let span = Text.of_string ~attr:() line in
      List.singleton span)
  in
  let lines = typeset ~max_width text in
  let view =
    lines
    |> List.map ~f:(fun spans_of_line ->
      List.map spans_of_line ~f:(fun span ->
        let text = Text.to_string span in
        View.text ?attrs text)
      |> View.hcat)
    |> View.vcat
  in
  let exceeds_by = Int.max 0 (View.width view - max_width) in
  (* It is important to note that only the "visual" part of the line will be <= max_width.
     If a line has trailing whitespace, that whitespace can exceed [max_width] as long as
     the "visual" width is <= max_width.

     We do some croppin in case the non-visual width is bigger.
  *)
  match exceeds_by with
  | 0 -> view
  | _ -> View.crop ~r:exceeds_by view
;;
