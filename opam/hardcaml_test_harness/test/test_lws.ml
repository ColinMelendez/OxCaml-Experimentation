open Core
open Hardcaml
open Hardcaml_lws
open Hardcaml_test_harness

module I = struct
  type 'a t =
    { a : 'a [@bits 16]
    ; b : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { s : 'a [@bits 16] } [@@deriving hardcaml]
end

let create _scope { I.a; b } = { O.s = Signal.( +: ) a b }

module Bench = Lws_harness.Make (I) (O)

type sim_context = Lws_context.M(I)(O).t

module%test _ = struct
  let testbench (h @ local) ~inputs:(i : _ I.t) ~outputs:_ =
    let open Bits in
    i.a <--. 4;
    i.b <--. 5;
    Lws.step h
  ;;

  let%expect_test "no waves test" =
    Bench.run ~random_initial_state:`All ~create testbench;
    Bench.run
      ~random_initial_state:`All
      ~waves_config:Waves_config.no_waves
      ~create
      testbench;
    [%expect {| |}]
  ;;

  let%expect_test "prefix test (with test name and line numbers)" =
    Bench.run
      ~random_initial_state:`All
      ~waves_config:
        (Waves_config.to_directory "/tmp/"
         |> Waves_config.with_always_include_line_numbers)
      ~test_name:"hello_world"
      ~create
      testbench;
    [%expect {| Saved waves to /tmp/test_lws_ml_43_hello_world.hardcamlwaveform |}]
  ;;

  let%expect_test "print waves after test" =
    Bench.run
      ~create
      ~print_waves_after_test:
        (Hardcaml_waveterm.Waveform.print
           ~display_width:40
           ~display_height:8
           ~wave_width:2)
      (fun (h @ local) ~inputs:(i : _ I.t) ~outputs:_ ->
         let open Bits in
         i.a <--. 0;
         i.b <--. 0;
         Lws.step h;
         i.a <--. 3;
         i.b <--. 4;
         Lws.step h;
         i.a <--. 10;
         i.b <--. 20;
         Lws.step h);
    [%expect
      {|
      ┌Signals─┐┌Waves───────────────────────┐
      │        ││──────┬─────┬───────────    │
      │a       ││ 0000 │0003 │000A           │
      │        ││──────┴─────┴───────────    │
      │        ││──────┬─────┬───────────    │
      │b       ││ 0000 │0004 │0014           │
      │        ││──────┴─────┴───────────    │
      └────────┘└────────────────────────────┘
      |}]
  ;;

  let%expect_test "run_advanced with sim_context" =
    Bench.run_advanced ~create (fun (h @ local) (sim_context : sim_context) ->
      let open Bits in
      sim_context.inputs.a <--. 10;
      sim_context.inputs.b <--. 20;
      Lws.step h;
      printf "sum = %d\n" (to_unsigned_int !(sim_context.outputs.after_edge.s)));
    [%expect {| sum = 30 |}]
  ;;

  let%expect_test "concurrent tasks with spawn" =
    Bench.run_advanced ~create (fun (h @ local) (sim_context : sim_context) ->
      let open Bits in
      let task =
        Lws.spawn h (fun (h @ local) ->
          for i = 1 to 3 do
            printf "task: cycle %d\n" i;
            Lws.step h
          done)
      in
      for i = 1 to 3 do
        sim_context.inputs.a <--. i;
        sim_context.inputs.b <--. i;
        Lws.step h;
        printf "main: sum = %d\n" (to_unsigned_int !(sim_context.outputs.after_edge.s))
      done;
      Lws.wait h task);
    [%expect
      {|
      task: cycle 1
      main: sum = 2
      task: cycle 2
      main: sum = 4
      task: cycle 3
      main: sum = 6
      |}]
  ;;

  let%expect_test "timeout" =
    Stdio.print_s
      (Or_error.try_with (fun () ->
         Bench.run ~timeout:5 ~create (fun (h @ local) ~inputs:_ ~outputs:_ ->
           for _ = 1 to 100 do
             Lws.step h
           done))
       |> [%sexp_of: unit Or_error.t]);
    [%expect {| (Error ("LWS simulation timed out" (timeout 5))) |}]
  ;;
end
