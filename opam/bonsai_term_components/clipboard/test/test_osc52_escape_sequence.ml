open! Core

let%expect_test "OSC 52 escape sequence is base64 encoded" =
  (let content = "hello" in
   print_endline
     (Ansi_text.visualize
      @@ Bonsai_term_clipboard.Private.osc52_escape_sequence
           Bonsai_term_clipboard.Selection.Clipboard
           content));
  [%expect {| (OSC:52;c;aGVsbG8=) |}]
;;
