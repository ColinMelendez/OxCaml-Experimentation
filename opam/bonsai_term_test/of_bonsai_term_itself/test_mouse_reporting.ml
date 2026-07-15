open! Core
open Bonsai_test
open Bonsai_term
open Bonsai.Let_syntax

let%expect_test "set_mouse_reporting effect can be created and used in handler (mocked)" =
  let handle =
    Bonsai_term_test.create_handle (fun ~dimensions:_ (local_ graph) ->
      let view = Bonsai.return (View.text "hello") in
      let handler =
        let%arr set_mouse_reporting = Mouse_reporting.set_mouse_reporting graph in
        fun (_event : Event.t) ->
          let%bind.Effect () =
            set_mouse_reporting Mouse_reporting.All_mouse_events_except_hover
          in
          Effect.Ignore
      in
      ~view, ~handler)
  in
  Bonsai_term_test.set_dimensions handle { width = 10; height = 1 };
  Handle.recompute_view handle;
  (* Send an event to trigger the handler which uses [set_mouse_reporting]. *)
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'a'; mods = [] });
  Handle.recompute_view handle;
  [%expect {| ([set_mouse_reporting] (enabled All_mouse_events_except_hover)) |}]
;;
