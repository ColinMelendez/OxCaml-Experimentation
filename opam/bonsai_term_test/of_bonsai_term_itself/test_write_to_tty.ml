open! Core
open Bonsai_term
open Bonsai.Let_syntax
open Bonsai_test

let%expect_test "Basic use of write to tty" =
  let handle =
    Bonsai_term_test.create_handle (fun ~dimensions:_ (local_ graph) ->
      let view = Bonsai.return View.none in
      let handler =
        let%arr write_string_to_tty =
          Bonsai_term.Expert.Write_to_tty.write_string_to_tty graph
        in
        fun _ -> write_string_to_tty "some string"
      in
      ~view, ~handler)
  in
  Handle.recompute_view handle;
  [%expect {| |}];
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'a'; mods = [] });
  Handle.recompute_view handle;
  [%expect {| ([write_string_to_tty] (string "some string")) |}]
;;
