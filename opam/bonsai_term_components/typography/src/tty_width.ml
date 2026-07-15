open! Core

let string_width s =
  String.Utf8.fold (String.Utf8.of_string s) ~init:0 ~f:(fun acc uchar ->
    let width = Bonsai_term.View.uchar_tty_width uchar in
    let width = if width < 0 then 1 else width in
    (* Handle undefined width *)
    acc + width)
;;
