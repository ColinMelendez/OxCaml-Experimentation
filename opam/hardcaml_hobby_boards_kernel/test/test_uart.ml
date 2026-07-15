open Core
open Hardcaml
open Hardcaml_waveterm

include struct
  open Hardcaml_hobby_boards_kernel
  include Uart_types
  module Tx = Uart.Tx
  module Rx = Uart.Rx
end

let test_tx_waves
  (data_bits : Data_bits.Cases.t)
  (parity : Parity.Cases.t)
  (stop_bits : Stop_bits.Cases.t)
  =
  let module Sim = Cyclesim.With_interface (Tx.I) (Tx.O) in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Tx.create scope) in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  inputs.data_in := of_string "110110110";
  inputs.data_in_valid := vdd;
  inputs.config.clocks_per_bit <--. 1;
  Data_bits.Enum.sim_set inputs.config.data_bits data_bits;
  Parity.Enum.sim_set inputs.config.parity parity;
  Stop_bits.Enum.sim_set inputs.config.stop_bits stop_bits;
  Cyclesim.cycle sim;
  inputs.data_in_valid := gnd;
  while not (to_bool !(outputs.data_in_ready)) do
    Cyclesim.cycle sim
  done;
  Cyclesim.cycle sim;
  waves
;;

let test_tx
  (data_bits : Data_bits.Cases.t)
  (parity : Parity.Cases.t)
  (stop_bits : Stop_bits.Cases.t)
  =
  let display_rules =
    Display_rule.
      [ port_name_is "clock"
      ; port_name_is "txd" ~wave_format:Bit
      ; port_name_is "data_in_ready" ~wave_format:Bit
      ; port_name_is "sm_tx"
      ]
  in
  Waveform.expect_exact
    (test_tx_waves data_bits parity stop_bits)
    ~wave_width:2
    ~display_rules
    ~signals_width:7
;;

let%expect_test "tx configurations" =
  test_tx Five None One;
  [%expect_exact
    {|
┌Signa┐┌Waves────────────────────────────────────────────────────────────────────────────┐
│clock││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
│     ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
│txd  ││      ┌─────┐           ┌───────────┐     ┌─────────────────                     │
│     ││──────┘     └───────────┘           └─────┘                                      │
│data_││      ┌─────┐                                         ┌─────                     │
│     ││──────┘     └─────────────────────────────────────────┘                          │
│     ││────────────┬─────────────────────────────┬─────┬─────┬─────                     │
│sm_tx││ Start      │Data                         │Stop │Comp.│Start                     │
│     ││────────────┴─────────────────────────────┴─────┴─────┴─────                     │
└─────┘└─────────────────────────────────────────────────────────────────────────────────┘
9c8753f81a24375ae8f169299922d737
|}];
  test_tx Nine None One;
  [%expect_exact
    {|
┌Signa┐┌Waves────────────────────────────────────────────────────────────────────────────┐
│clock││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
│     ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
│txd  ││      ┌─────┐           ┌───────────┐     ┌───────────┐     ┌────────────────────│
│     ││──────┘     └───────────┘           └─────┘           └─────┘                    │
│data_││      ┌─────┐                                                                 ┌──│
│     ││──────┘     └─────────────────────────────────────────────────────────────────┘  │
│     ││────────────┬─────────────────────────────────────────────────────┬─────┬─────┬──│
│sm_tx││ Start      │Data                                                 │Stop │Comp.│St│
│     ││────────────┴─────────────────────────────────────────────────────┴─────┴─────┴──│
└─────┘└─────────────────────────────────────────────────────────────────────────────────┘
b200204ab3ec9d90070d2babea42bac3
|}];
  test_tx Six None Two;
  [%expect_exact
    {|
┌Signa┐┌Waves────────────────────────────────────────────────────────────────────────────┐
│clock││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
│     ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
│txd  ││      ┌─────┐           ┌───────────┐     ┌─────────────────────────────         │
│     ││──────┘     └───────────┘           └─────┘                                      │
│data_││      ┌─────┐                                                     ┌─────         │
│     ││──────┘     └─────────────────────────────────────────────────────┘              │
│     ││────────────┬───────────────────────────────────┬───────────┬─────┬─────         │
│sm_tx││ Start      │Data                               │Stop       │Comp.│Start         │
│     ││────────────┴───────────────────────────────────┴───────────┴─────┴─────         │
└─────┘└─────────────────────────────────────────────────────────────────────────────────┘
004bfd43c33390718da8fa2a59e97121
|}];
  test_tx Seven Even One;
  [%expect_exact
    {|
┌Signa┐┌Waves────────────────────────────────────────────────────────────────────────────┐
│clock││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
│     ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
│txd  ││      ┌─────┐           ┌───────────┐     ┌───────────┐           ┌───────────   │
│     ││──────┘     └───────────┘           └─────┘           └───────────┘              │
│data_││      ┌─────┐                                                           ┌─────   │
│     ││──────┘     └───────────────────────────────────────────────────────────┘        │
│     ││────────────┬─────────────────────────────────────────┬─────┬─────┬─────┬─────   │
│sm_tx││ Start      │Data                                     │Pari.│Stop │Comp.│Start   │
│     ││────────────┴─────────────────────────────────────────┴─────┴─────┴─────┴─────   │
└─────┘└─────────────────────────────────────────────────────────────────────────────────┘
ead094390c2e9d8003cd3d92cad08eac
|}];
  test_tx Seven Odd One;
  [%expect_exact
    {|
┌Signa┐┌Waves────────────────────────────────────────────────────────────────────────────┐
│clock││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
│     ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
│txd  ││      ┌─────┐           ┌───────────┐     ┌───────────┐     ┌─────────────────   │
│     ││──────┘     └───────────┘           └─────┘           └─────┘                    │
│data_││      ┌─────┐                                                           ┌─────   │
│     ││──────┘     └───────────────────────────────────────────────────────────┘        │
│     ││────────────┬─────────────────────────────────────────┬─────┬─────┬─────┬─────   │
│sm_tx││ Start      │Data                                     │Pari.│Stop │Comp.│Start   │
│     ││────────────┴─────────────────────────────────────────┴─────┴─────┴─────┴─────   │
└─────┘└─────────────────────────────────────────────────────────────────────────────────┘
b052bd78f6428e1d1cb4092008d9d271
|}]
;;

let test_rx
  (data_bits : Data_bits.Cases.t)
  (parity : Parity.Cases.t)
  (stop_bits : Stop_bits.Cases.t)
  =
  let module Sim = Cyclesim.With_interface (Rx.I) (Rx.O) in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Rx.create_internal scope) in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  inputs.config.clocks_per_bit <--. 1;
  Data_bits.Enum.sim_set inputs.config.data_bits data_bits;
  Parity.Enum.sim_set inputs.config.parity parity;
  Stop_bits.Enum.sim_set inputs.config.stop_bits stop_bits;
  (* start bit *)
  inputs.enable := Bits.vdd;
  inputs.rxd := Bits.gnd;
  Cyclesim.cycle sim;
  (* data *)
  for _ = 0 to 4 do
    inputs.rxd := Bits.vdd;
    Cyclesim.cycle sim
  done;
  (* stop *)
  inputs.rxd := Bits.vdd;
  Cyclesim.cycle sim;
  (* done *)
  Cyclesim.cycle sim;
  let display_rules =
    Display_rule.
      [ port_name_is "clock"
      ; port_name_is "rxd" ~wave_format:Bit
      ; port_name_is "data_out" ~wave_format:Hex
      ; port_name_is "data_out_valid" ~wave_format:Bit
      ; port_name_is "error" ~wave_format:Bit
      ; port_name_is "sm_rx"
      ]
  in
  Waveform.expect_exact waves ~wave_width:2 ~display_rules ~signals_width:7
;;

let%expect_test "rx configurations" =
  test_rx Five None One;
  [%expect_exact
    {|
┌Signa┐┌Waves────────────────────────────────────────────────────────────────────────────┐
│clock││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
│     ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
│rxd  ││            ┌─────────────────────────────────────────                           │
│     ││────────────┘                                                                    │
│     ││──────────────────┬─────┬─────┬─────┬─────┬───────────                           │
│data_││ 000              │100  │180  │1C0  │1E0  │1F0                                   │
│     ││──────────────────┴─────┴─────┴─────┴─────┴───────────                           │
│data_││                                                ┌─────                           │
│     ││────────────────────────────────────────────────┘                                │
│error││                                                                                 │
│     ││──────────────────────────────────────────────────────                           │
│     ││────────────┬─────────────────────────────┬─────┬─────                           │
│sm_rx││ Start      │Data                         │Stop │Start                           │
│     ││────────────┴─────────────────────────────┴─────┴─────                           │
└─────┘└─────────────────────────────────────────────────────────────────────────────────┘
d98e8198afaec5c2f51b02129c9ba9df
|}]
;;

(* Drive RX with TX. This is done without any sampling on the rx side and is about testing
   the statemachine transitions. *)

let test_tx_rx
  ?(top = false)
  ?(clocks_per_bit = 1)
  (data_bits : Data_bits.Cases.t)
  (parity : Parity.Cases.t)
  (stop_bits : Stop_bits.Cases.t)
  =
  let open Bits in
  let module Sim = Cyclesim.With_interface (Tx.I) (Rx.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (fun i ->
      let tx = Tx.create (Scope.sub_scope scope "tx") i in
      let rx =
        (if top then Rx.create else Rx.create_internal)
          ~align_rxdata_to_lsb:true
          (Scope.sub_scope scope "rx")
          { Rx.I.clocking = i.clocking
          ; config = i.config
          ; enable = Signal.vdd
          ; rxd = Signal.( -- ) tx.txd "txd"
          }
      in
      rx)
  in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  inputs.config.clocks_per_bit <--. clocks_per_bit;
  Data_bits.Enum.sim_set inputs.config.data_bits data_bits;
  Parity.Enum.sim_set inputs.config.parity parity;
  Stop_bits.Enum.sim_set inputs.config.stop_bits stop_bits;
  let timeout = ref 0 in
  let tx_and_rx value =
    inputs.data_in <--. value;
    inputs.data_in_valid := vdd;
    Cyclesim.cycle sim;
    inputs.data_in_valid := gnd;
    Cyclesim.cycle sim;
    while
      (not (Bits.to_bool !(outputs.data_out_valid)))
      && (not (Bits.to_bool !(outputs.error)))
      && !timeout < 1_000
    do
      Cyclesim.cycle sim;
      Int.incr timeout
    done;
    let result =
      if Bits.to_bool !(outputs.error) || !timeout = 1_000
      then Error (Bits.to_unsigned_int !(outputs.data_out))
      else Ok (Bits.to_unsigned_int !(outputs.data_out))
    in
    (* wait for the transmitter to complete - the receiver will stop first *)
    Cyclesim.cycle ~n:clocks_per_bit sim;
    result
  in
  waves, tx_and_rx
;;

let show_waves waves =
  let display_rules =
    Display_rule.
      [ port_name_is "clock"
      ; port_name_is "tx$sm_tx"
      ; port_name_is "data_in" ~wave_format:Hex
      ; port_name_is "data_in_valid" ~wave_format:Bit
      ; port_name_is "txd" ~wave_format:Bit
      ; port_name_is "data_out" ~wave_format:Hex
      ; port_name_is "data_out_valid" ~wave_format:Bit
      ; port_name_is "error" ~wave_format:Bit
      ; port_name_is "rx$sm_rx"
      ]
  in
  Waveform.expect
    waves
    ~wave_width:1
    ~display_rules
    ~signals_width:11
    ~display_width:90
    ~display_height:25
;;

let%expect_test "tx -> rx" =
  let waves, tx_and_rx = test_tx_rx Six None One in
  ignore (tx_and_rx 42 : (int, int) Result.t);
  ignore (tx_and_rx 43 : (int, int) Result.t);
  show_waves waves;
  [%expect_exact
    {|┌Signals──┐┌Waves────────────────────────────────────────────────────────────────────────┐
│clock    ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌│
│         ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘│
│         ││────────┬───────────────────────┬───┬───┬───────┬───────────────────────┬───┬│
│tx$sm_tx ││ Start  │Data                   │St.│Co.│Start  │Data                   │St.││
│         ││────────┴───────────────────────┴───┴───┴───────┴───────────────────────┴───┴│
│         ││────┬───────────────────────────────────────┬────────────────────────────────│
│data_in  ││ 000│02A                                    │02B                             │
│         ││────┴───────────────────────────────────────┴────────────────────────────────│
│data_in_v││    ┌───┐                                   ┌───┐                            │
│         ││────┘   └───────────────────────────────────┘   └────────────────────────────│
│txd      ││    ┌───┐       ┌───┐   ┌───┐   ┌───────────────┐   ┌───────┐   ┌───┐   ┌────│
│         ││────┘   └───────┘   └───┘   └───┘               └───┘       └───┘   └───┘    │
│         ││────────────────────┬───┬───┬───┬───┬───────────────────┬───┬───┬───┬───┬───┬│
│data_out ││ 000                │020│010│028│014│02A                │035│03A│01D│02E│017││
│         ││────────────────────┴───┴───┴───┴───┴───────────────────┴───┴───┴───┴───┴───┴│
│data_out_││                                        ┌───┐                                │
│         ││────────────────────────────────────────┘   └────────────────────────────────│
│error    ││                                                                             │
│         ││─────────────────────────────────────────────────────────────────────────────│
│         ││────────────┬───────────────────────┬───┬───────────┬───────────────────────┬│
│rx$sm_rx ││ Start      │Data                   │St.│Start      │Data                   ││
│         ││────────────┴───────────────────────┴───┴───────────┴───────────────────────┴│
│         ││                                                                             │
└─────────┘└─────────────────────────────────────────────────────────────────────────────┘
1efb7b58e8c3b3c7a42fb865f553a009
|}]
;;

let send_string
  ?top
  ?clocks_per_bit
  (data_bits : Data_bits.Cases.t)
  (parity : Parity.Cases.t)
  (stop_bits : Stop_bits.Cases.t)
  str
  =
  let waves, tx_and_rx = test_tx_rx ?top ?clocks_per_bit data_bits parity stop_bits in
  let result =
    List.map (String.to_list str) ~f:(fun c ->
      match tx_and_rx (Char.to_int c) with
      | Ok c -> Char.of_int_exn c
      | Error _ -> '?')
    |> String.of_char_list
  in
  waves, result
;;

let%expect_test "send strings" =
  let send_string d p s str = snd (send_string d p s str) in
  let result = send_string Seven None One "hello world!" in
  print_s [%message (result : string)];
  let result = send_string Eight Even One "I humbly decline." in
  print_s [%message (result : string)];
  let result = send_string Nine Odd Two "Though shalt not pass!" in
  print_s [%message (result : string)];
  [%expect
    {|
    (result "hello world!")
    (result "I humbly decline.")
    (result "Though shalt not pass!")
    |}]
;;
