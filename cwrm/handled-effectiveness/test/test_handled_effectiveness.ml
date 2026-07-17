open! Core
open Bonsai_test
open Bonsai_term

let demos = Handled_effectiveness.Demos.all ()
let dimensions = { Dimensions.width = 72; height = 22 }

let create_handle demos =
  Bonsai_term_test.create_handle_generic
    ~initial_dimensions:dimensions
    ~to_view_with_handler:(fun (r : Handled_effectiveness.App.result) ->
      ~view:r.view, ~handler:r.handler)
    ~handle_incoming:
      (fun (r : Handled_effectiveness.App.result)
        (incoming : Handled_effectiveness.App.Incoming.t) -> r.inject incoming)
    (fun ~dimensions (local_ graph) ->
      Handled_effectiveness.App.test_component ~demos ~dimensions graph)
;;

let%expect_test "state demo initial screenshot" =
  let handle = create_handle demos in
  Handle.show handle;
  [%expect {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ handled-effectiveness  -  state  -  step 1/7  [stop]                   │
    │╭ demo ────────────────────────────────────────────────────────────────╮│
    ││Shallow state (Get / Set)                                             ││
    ││A single handler interprets Get/Set operations, threading an int      ││
    ││through continue. The computation never sees the state cell - only the││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │╭ event ───────────────────────────────────────────────────────────────╮│
    ││> Resume comp                                                         ││
    ││install state handler with initial state 7                            ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │╭ runtime ────────────────────────────────╮╭ console ──────────────────╮│
    ││state = 7                                ││(empty)                    ││
    ││                                         ││                           ││
    ││                                         ││                           ││
    ││                                         ││                           ││
    │╰─────────────────────────────────────────╯╰───────────────────────────╯│
    │╭ timeline ────────────────────────────────────────────────────────────╮│
    ││RGTGTG|                                                               ││
    ││O......                                                               ││
    ││ F fork  Y yield  R resume  S say  G/T get/set  ></ send/recv         ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │ space/l:step  h:back  p:play  r:reset  [/]:demo  1-4:jump  q:quit      │
    └────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "step through state demo then switch to scheduler" =
  let handle = create_handle demos in
  Bonsai_term_test.send_event handle (Key_press { key = ASCII ' '; mods = [] });
  Bonsai_term_test.send_event handle (Key_press { key = ASCII ' '; mods = [] });
  Handle.show handle;
  [%expect {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ handled-effectiveness  -  state  -  step 3/7  [stop]                   │
    │╭ demo ────────────────────────────────────────────────────────────────╮│
    ││Shallow state (Get / Set)                                             ││
    ││A single handler interprets Get/Set operations, threading an int      ││
    ││through continue. The computation never sees the state cell - only the││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │╭ event ───────────────────────────────────────────────────────────────╮│
    ││> Set   7 -> 17                                                       ││
    ││handler updates state 7 -> 17                                         ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │╭ runtime ────────────────────────────────╮╭ console ──────────────────╮│
    ││state = 17                               ││(empty)                    ││
    ││                                         ││                           ││
    ││                                         ││                           ││
    ││                                         ││                           ││
    │╰─────────────────────────────────────────╯╰───────────────────────────╯│
    │╭ timeline ────────────────────────────────────────────────────────────╮│
    ││RGTGTG|                                                               ││
    ││..O....                                                               ││
    ││ F fork  Y yield  R resume  S say  G/T get/set  ></ send/recv         ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │ space/l:step  h:back  p:play  r:reset  [/]:demo  1-4:jump  q:quit      │
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (Key_press { key = ASCII '4'; mods = [] });
  Handle.show handle;
  [%expect {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ handled-effectiveness  -  scheduler  -  step 1/23  [stop]              │
    │╭ demo ────────────────────────────────────────────────────────────────╮│
    ││Round-robin scheduler                                                 ││
    ││Fork / Yield / Say under a cooperative scheduler. Parent continuations││
    ││are uniquely enqueued (Unique.Once); each Yield rotates the ready     ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │╭ event ───────────────────────────────────────────────────────────────╮│
    ││> Resume main                                                         ││
    ││spawn fiber main                                                      ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │╭ runtime ────────────────────────────────╮╭ console ──────────────────╮│
    ││scheduler                                ││(empty)                    ││
    ││running: main                            ││                           ││
    ││ready:   []                              ││                           ││
    ││                                         ││                           ││
    │╰─────────────────────────────────────────╯╰───────────────────────────╯│
    │╭ timeline ────────────────────────────────────────────────────────────╮│
    ││RFRSYRFRSYRSYRS.RS.RS.|                                               ││
    ││O......................                                               ││
    ││ F fork  Y yield  R resume  S say  G/T get/set  ></ send/recv         ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │ space/l:step  h:back  p:play  r:reset  [/]:demo  1-4:jump  q:quit      │
    └────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "demo traces are non-empty" =
  List.iter demos ~f:(fun demo ->
    print_s
      [%sexp
        { id = (demo.id : string)
        ; steps = (Handled_effectiveness.Types.Demo.num_steps demo : int)
        }]);
  [%expect {|
    ((id    state)
     (steps 7))
    ((id    protocol)
     (steps 6))
    ((id    generator)
     (steps 8))
    ((id    scheduler)
     (steps 23))
    |}]
;;
