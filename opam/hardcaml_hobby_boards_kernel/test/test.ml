open Core
open Hardcaml
open Signal
open Expect_test_helpers_core

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Nexys_a7_100t = Nexys_a7_100t
  module Xilinx_top = Xilinx_top.For_testing
end

open Nexys_a7_100t

let%expect_test "sexp of circuit" =
  let board = Board.create () in
  let clocks = Clock_and_reset.create board in
  let spec = Reg_spec.create ~clock:clocks.clock_100 () in
  let audio_amp = Audio_amplifier.create board in
  Audio_amplifier.complete
    board
    { Audio_amplifier.O.sd = reg spec vdd }
    { Audio_amplifier.T.pwm =
        { With_valid.valid = reg spec ~:(audio_amp.pwm); value = gnd }
    };
  Leds.complete board (Leds.O.Of_signal.zero ());
  print_s [%message (board : Board.t)];
  [%expect
    {|
    (board (
      (subsystems (
        (audio_amplifier (
          (inputs ())
          (outputs ((
            wire
            (names (aud_sd))
            (width   1)
            (data_in register))))
          (input_tristates ((wire (names (aud_pwm)) (width 1))))
          (output_tristates (
            (wire
              (names (aud_pwm$valid))
              (width   1)
              (data_in register))
            (wire
              (names (aud_pwm$value))
              (width   1)
              (data_in 0b0))))
          (complete true)))
        (clocking (
          (inputs (
            (wire (names (clock_100)) (width 1))
            (wire (names (reset_n))   (width 1))))
          (outputs          ())
          (input_tristates  ())
          (output_tristates ())
          (complete true)))
        (leds (
          (inputs ())
          (outputs ((
            wire
            (names (leds))
            (width   16)
            (data_in 0x0000))))
          (input_tristates  ())
          (output_tristates ())
          (complete true)))))
      (pins ())
      (scope (
        (path      ())
        (name_path ())
        (circuit_database              <opaque>)
        (flatten_design                false)
        (trace_properties              false)
        (naming_scheme                 No_path)
        (auto_label_hierarchical_ports false)
        (assertion_manager ((map ()) (is_finalized false)))
        (instantiation_mangler ((case_sensitive false) (table ())))
        (property_manager ((ltl ()) (is_finalized false) (aps ())))))))
    |}]
;;

module%test Generated_hierarchy = struct
  module I = Types.Value (struct
      let port_name = "switches_i"
      let port_width = 16
    end)

  module O = Types.Value (struct
      let port_name = "leds_o"
      let port_width = 16
    end)

  let hier _scope (i : _ I.t) = Signal.( ~: ) i

  let%expect_test "hierarchy" =
    let board = Board.create () in
    let switches = Switches.create board in
    let module H = Hierarchy.In_scope (I) (O) in
    Leds.complete
      board
      (H.hierarchical ~scope:(Board.scope board) ~name:"inner" hier switches);
    Xilinx_top.rtl_of_hardcaml_circuit board |> print_endline;
    [%expect
      {|
      module inner (
          switches_i,
          leds_o
      );

          input [15:0] switches_i;
          output [15:0] leds_o;

          wire [15:0] signal_wire;
          wire [15:0] signal_not;
          assign signal_wire = switches_i;
          assign signal_not = ~ signal_wire;
          assign leds_o = signal_not;

      endmodule
      module for_testing (
          switches,
          leds
      );

          input [15:0] switches;
          output [15:0] leds;

          wire [15:0] signal_inst;
          wire [15:0] signal_wire;
          inner
              inner
              ( .switches_i(switches),
                .leds_o(signal_inst[15:0]) );
          assign signal_wire = signal_inst;
          assign leds = signal_wire;

      endmodule
      |}]
  ;;
end

let%expect_test "invalid port widths" =
  require_does_raise (fun () ->
    let board = Board.create () in
    Nexys_a7_100t.Leds.complete board vdd);
  [%expect
    {|
    ("Output port width is incorrect"
      (subsystem      leds)
      (expected_width 16)
      (got_width      1)
      (port_name      leds))
    |}];
  require_does_raise (fun () ->
    let board = Board.create () in
    let (_ : _ Nexys_a7_100t.Temperature_sensor.T.t) =
      Nexys_a7_100t.Temperature_sensor.create board
    in
    Nexys_a7_100t.Temperature_sensor.complete
      board
      { Nexys_a7_100t.Temperature_sensor.T.scl = { valid = zero 2; value = vdd }
      ; sda = { valid = vdd; value = gnd }
      });
  [%expect
    {|
    ("Tristate enable width must be 1"
      (subsystem temperature_sensor)
      (port_name temp_scl))
    |}];
  require_does_raise (fun () ->
    let board = Board.create () in
    let (_ : _ Nexys_a7_100t.Temperature_sensor.T.t) =
      Nexys_a7_100t.Temperature_sensor.create board
    in
    Nexys_a7_100t.Temperature_sensor.complete
      board
      { Nexys_a7_100t.Temperature_sensor.T.scl = { valid = gnd; value = gnd }
      ; sda = { valid = vdd; value = zero 2 }
      });
  [%expect
    {|
    ("Tristate port width is incorrect"
      (subsystem      temperature_sensor)
      (expected_width 1)
      (got_width      2)
      (port_name      temp_sda))
    |}]
;;
