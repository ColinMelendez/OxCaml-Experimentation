open! Core
open Bonsai_test
open Bonsai_term

let fixture_snapshot : Oxtop.Types.Snapshot.t =
  { hostname = "oxbox"
  ; ncpu = 8
  ; loadavg = { one = 1.25; five = 0.90; fifteen = 0.70 }
  ; memory =
      { total_bytes = Int64.(16L * 1024L * 1024L * 1024L)
      ; used_bytes = Int64.(8L * 1024L * 1024L * 1024L)
      }
  ; cpu_pct = 42.5
  ; cpu_history = [ 10.; 20.; 35.; 50.; 42.; 30.; 45.; 42.5 ]
  ; processes =
      [ { pid = 101; cpu_pct = 55.0; mem_pct = 12.5; rss_kb = 1200000; command = "firefox" }
      ; { pid = 202; cpu_pct = 30.0; mem_pct = 8.0; rss_kb = 512000; command = "node" }
      ; { pid = 303; cpu_pct = 12.0; mem_pct = 4.2; rss_kb = 256000; command = "dune" }
      ; { pid = 404; cpu_pct = 5.0; mem_pct = 1.1; rss_kb = 64000; command = "zsh" }
      ; { pid = 505; cpu_pct = 2.0; mem_pct = 0.5; rss_kb = 32000; command = "sshd" }
      ; { pid = 606; cpu_pct = 1.0; mem_pct = 20.0; rss_kb = 2048000; command = "chrome" }
      ]
  }
;;

let dimensions = { Dimensions.width = 72; height = 18 }

let create_handle snapshot =
  Bonsai_term_test.create_handle_generic
    ~initial_dimensions:dimensions
    ~to_view_with_handler:(fun (r : Oxtop.App.result) ->
      ~view:r.view, ~handler:r.handler)
    ~handle_incoming:(fun (r : Oxtop.App.result) (incoming : Oxtop.App.Incoming.t) ->
      r.inject incoming)
    (fun ~dimensions (local_ graph) ->
      Oxtop.App.test_component ~initial_snapshot:snapshot ~dimensions graph)
;;

let%expect_test "initial screenshot" =
  let handle = create_handle fixture_snapshot in
  Handle.show handle;
  [%expect {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ oxtop  oxbox  ·  8 cpu  ·  load 1.25 0.90 0.70                         │
    │CPU ███████████████████░░░░░░░░░░░░░░░░░░░░░░░░░  42.5%                 │
    │MEM ██████████████████████░░░░░░░░░░░░░░░░░░░░░░ 8.0G/16.0G             │
    │hst ▂▄▆█▇▅▇▇                                                            │
    │╭ processes · sort=cpu ────────────────────────────────────────────────╮│
    ││    PID   CPU%   MEM%      RSS  COMMAND                               ││
    ││>   101   55.0   12.5     1.1G  firefox                               ││
    ││    202   30.0    8.0     500M  node                                  ││
    ││    303   12.0    4.2     250M  dune                                  ││
    ││    404    5.0    1.1      62M  zsh                                   ││
    ││    505    2.0    0.5      31M  sshd                                  ││
    ││    606    1.0   20.0     2.0G  chrome                                ││
    ││                                                                      ││
    ││                                                                      ││
    ││                                                                      ││
    ││                                                                      ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │ q:quit  j/k:move  g/G:top/bot  c/m/p/n:sort  s:cycle                   │
    └────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "sort by memory and move selection" =
  let handle = create_handle fixture_snapshot in
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'm'; mods = [] });
  Handle.show handle;
  [%expect {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ oxtop  oxbox  ·  8 cpu  ·  load 1.25 0.90 0.70                         │
    │CPU ███████████████████░░░░░░░░░░░░░░░░░░░░░░░░░  42.5%                 │
    │MEM ██████████████████████░░░░░░░░░░░░░░░░░░░░░░ 8.0G/16.0G             │
    │hst ▂▄▆█▇▅▇▇                                                            │
    │╭ processes · sort=mem ────────────────────────────────────────────────╮│
    ││    PID   CPU%   MEM%      RSS  COMMAND                               ││
    ││>   606    1.0   20.0     2.0G  chrome                                ││
    ││    101   55.0   12.5     1.1G  firefox                               ││
    ││    202   30.0    8.0     500M  node                                  ││
    ││    303   12.0    4.2     250M  dune                                  ││
    ││    404    5.0    1.1      62M  zsh                                   ││
    ││    505    2.0    0.5      31M  sshd                                  ││
    ││                                                                      ││
    ││                                                                      ││
    ││                                                                      ││
    ││                                                                      ││
    │╰──────────────────────────────────────────────────────────────────────╯│
    │ q:quit  j/k:move  g/G:top/bot  c/m/p/n:sort  s:cycle                   │
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'j'; mods = [] });
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'j'; mods = [] });
  Handle.show_diff handle;
  [%expect {|
      ┌────────────────────────────────────────────────────────────────────────┐
      │ oxtop  oxbox  ·  8 cpu  ·  load 1.25 0.90 0.70                         │
      │CPU ███████████████████░░░░░░░░░░░░░░░░░░░░░░░░░  42.5%                 │
      │MEM ██████████████████████░░░░░░░░░░░░░░░░░░░░░░ 8.0G/16.0G             │
      │hst ▂▄▆█▇▅▇▇                                                            │
      │╭ processes · sort=mem ────────────────────────────────────────────────╮│
      ││    PID   CPU%   MEM%      RSS  COMMAND                               ││
    -|││>   606    1.0   20.0     2.0G  chrome                                ││
    +|││    606    1.0   20.0     2.0G  chrome                                ││
      ││    101   55.0   12.5     1.1G  firefox                               ││
    -|││    202   30.0    8.0     500M  node                                  ││
    +|││>   202   30.0    8.0     500M  node                                  ││
      ││    303   12.0    4.2     250M  dune                                  ││
      ││    404    5.0    1.1      62M  zsh                                   ││
      ││    505    2.0    0.5      31M  sshd                                  ││
      ││                                                                      ││
      ││                                                                      ││
      ││                                                                      ││
      ││                                                                      ││
      │╰──────────────────────────────────────────────────────────────────────╯│
      │ q:quit  j/k:move  g/G:top/bot  c/m/p/n:sort  s:cycle                   │
      └────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "ui state machine sorting and selection" =
  let open Oxtop.Types in
  let ui = Ui_state.initial in
  let ui = Ui_state.apply ui (Set_sort Mem) ~num_processes:6 ~visible_rows:4 in
  print_s [%sexp (ui : Ui_state.t)];
  [%expect {|
    ((selected      0)
     (sort_by       Mem)
     (scroll_offset 0))
    |}];
  let ui = Ui_state.apply ui Select_next ~num_processes:6 ~visible_rows:4 in
  let ui = Ui_state.apply ui Select_next ~num_processes:6 ~visible_rows:4 in
  let ui = Ui_state.apply ui Select_next ~num_processes:6 ~visible_rows:4 in
  let ui = Ui_state.apply ui Select_next ~num_processes:6 ~visible_rows:4 in
  print_s [%sexp (ui : Ui_state.t)];
  [%expect {|
    ((selected      4)
     (sort_by       Mem)
     (scroll_offset 1))
    |}]
;;

let%expect_test "resize keeps a coherent frame" =
  let handle = create_handle fixture_snapshot in
  Bonsai_term_test.set_dimensions handle { width = 50; height = 12 };
  Handle.show handle;
  [%expect {|
    ┌──────────────────────────────────────────────────┐
    │ oxtop  oxbox  ·  8 cpu  ·  load 1.25 0.90 0.70   │
    │CPU █████████░░░░░░░░░░░░░  42.5%                 │
    │MEM ███████████░░░░░░░░░░░ 8.0G/16.0G             │
    │hst ▂▄▆█▇▅▇▇                                      │
    │╭ processes · sort=cpu ──────────────────────────╮│
    ││    PID   CPU%   MEM%      RSS  COMMAND         ││
    ││>   101   55.0   12.5     1.1G  firefox         ││
    ││    202   30.0    8.0     500M  node            ││
    ││    303   12.0    4.2     250M  dune            ││
    ││    404    5.0    1.1      62M  zsh             ││
    │╰────────────────────────────────────────────────╯│
    │ q:quit  j/k:move  g/G:top/bot  c/m/p/n:sort  s:cy│
    └──────────────────────────────────────────────────┘
    |}]
;;
