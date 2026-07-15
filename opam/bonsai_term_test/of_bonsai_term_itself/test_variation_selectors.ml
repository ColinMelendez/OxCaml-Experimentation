open! Core
open Bonsai_test
open Bonsai_term

module Test_case = struct
  type t =
    { test_case_name : string
    ; string : string
    }
end

let test_cases =
  [ { Test_case.test_case_name = "Caution emoji (VS-16)"; string = "⚠️" }
  ; { test_case_name = "Flag emoji / regional indicators"; string = "🇺🇸" }
  ; { test_case_name = "Composite emojis with the zero width join character"
    ; string = "🧑‍🌾"
    }
  ]
;;

let%expect_test "Print and dump all of the test cases" =
  List.iter test_cases ~f:(fun { Test_case.test_case_name; string } ->
    print_endline test_case_name;
    print_endline "---";
    let handle =
      Bonsai_term_test.create_handle
        ~initial_dimensions:{ height = 1; width = 20 }
        (fun ~dimensions:_ (local_ _graph) ->
           let view =
             let view = View.text string in
             let width = View.width view in
             print_s [%message (width : int)];
             Bonsai.return view
           in
           let handler = Bonsai.return (fun _ -> Effect.Ignore) in
           ~view, ~handler)
    in
    Handle.show handle;
    Expectable.print
    @@
    let%map.List uchar = String.Utf8.to_list (String.Utf8.of_string string) in
    let width = Notty.Tty_width_hint.tty_width_hint uchar in
    let uchar_as_string = String.Utf8.to_string (String.Utf8.of_list [ uchar ]) in
    [%message (uchar : Uchar.t) (width : int) (uchar_as_string : string)]);
  (* The caution emoji ⚠️ correctly renders as width 2 due to the variation selector *)
  [%expect
    {|
    Caution emoji (VS-16)
    ---
    (width 2)
    ┌────────────────────┐
    │⚠️                  │
    └────────────────────┘

    ┌────────┬───────┬─────────────────┐
    │ uchar  │ width │ uchar_as_string │
    ├────────┼───────┼─────────────────┤
    │ U+26A0 │ 1     │ ⚠               │
    │ U+FE0F │ 0     │ ️               │
    └────────┴───────┴─────────────────┘

    Flag emoji / regional indicators
    ---
    (width 2)
    ┌────────────────────┐
    │🇺🇸                  │
    └────────────────────┘

    ┌─────────┬───────┬─────────────────┐
    │ uchar   │ width │ uchar_as_string │
    ├─────────┼───────┼─────────────────┤
    │ U+1F1FA │ 1     │ 🇺               │
    │ U+1F1F8 │ 1     │ 🇸               │
    └─────────┴───────┴─────────────────┘

    Composite emojis with the zero width join character
    ---
    (width 4)
    ┌────────────────────┐
    │🧑‍🌾                │
    └────────────────────┘

    ┌─────────┬───────┬─────────────────┐
    │ uchar   │ width │ uchar_as_string │
    ├─────────┼───────┼─────────────────┤
    │ U+1F9D1 │ 2     │ 🧑               │
    │ U+200D  │ 0     │ ‍               │
    │ U+1F33E │ 2     │ 🌾               │
    └─────────┴───────┴─────────────────┘
    |}]
;;
