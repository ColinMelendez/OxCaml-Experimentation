open! Core
open Bonsai_term
open Bonsai_test

let print_view ?(dimensions = { Dimensions.width = 4; height = 32 }) view =
  let handle =
    Bonsai_term_test.create_handle_without_handler (fun ~dimensions:_ _graph ->
      Bonsai.return view)
  in
  Bonsai_term_test.set_dimensions handle dimensions;
  Handle.show handle
;;

module Outcome = struct
  type t =
    | Ok
    | Raises of string
  [@@deriving sexp_of]
end

let outcome_of_string string =
  let black = Attr.Color.rgb ~r:0 ~g:0 ~b:0 in
  try
    let _view = View.with_colors ~fg:black ~bg:black (View.text string) in
    Outcome.Ok
  with
  | exn -> Raises (Exn.to_string exn)
;;

let%expect_test "repro: valid UTF-8 control characters crash Notty" =
  (* This is invalid UTF-8 (a single byte >= 0x80), so [View.text] sanitizes it before it
     reaches Notty. *)
  let byte_0x80 = String.of_char_list [ Char.of_int_exn 0x80 ] in
  (* This is the Unicode codepoint U+0080 encoded as UTF-8 (0xC2 0x80). Notty considers
     U+0080 a control character and throws. *)
  let u0080 = "\u{0080}" in
  List.iter
    [ "byte_0x80", byte_0x80; "u0080", u0080 ]
    ~f:(fun (name, string) ->
      let outcome = outcome_of_string string in
      print_s [%message (name : string) (outcome : Outcome.t) (string : string)]);
  [%expect
    {|
    ((name    byte_0x80)
     (outcome Ok)
     (string  "\128"))
    ((name    u0080)
     (outcome Ok)
     (string  "\194\128"))
    |}]
;;

let%expect_test "print View.vcat of Unicode C1 control range (0x80..0x9f)" =
  let view =
    List.range 0x80 0xA0
    |> List.map ~f:(fun codepoint ->
      let uchar = Option.value_exn (Uchar.of_scalar codepoint) in
      View.text (Uchar.Utf8.to_string uchar))
    |> View.vcat
  in
  print_view view;
  [%expect
    {|
    ┌────┐
    │\128│
    │\129│
    │\130│
    │\131│
    │\132│
    │\133│
    │\134│
    │\135│
    │\136│
    │\137│
    │\138│
    │\139│
    │\140│
    │\141│
    │\142│
    │\143│
    │\144│
    │\145│
    │\146│
    │\147│
    │\148│
    │\149│
    │\150│
    │\151│
    │\152│
    │\153│
    │\154│
    │\155│
    │\156│
    │\157│
    │\158│
    │\159│
    └────┘
    |}]
;;
