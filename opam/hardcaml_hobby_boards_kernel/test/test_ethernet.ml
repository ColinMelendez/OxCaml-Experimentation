open Core
open Hardcaml
open Hardcaml_waveterm

include struct
  open Hardcaml_hobby_boards_kernel
  module Rx = Ethernet.Rx

  module Tx = Ethernet.Tx.Make (struct
      let max_packets = 4
      let average_packet_size = 128
    end)

  module Config = Ethernet.Config
  module Udp_packet_generator = Udp_packet_generator
  module Udp_packet_stream = Udp_packet_stream
  include Ethernet_types
  include Ethernet_utils.For_testing

  module Udp_packet_decoder = Udp_packet_decoder.Make (struct
      let filter_config = Udp_packet_decoder_intf.Filter_config.No_filter
    end)
end

let sim_frame sim (inputs : _ Rx.I.t) (outputs : _ Rx.O.t) data_width =
  let open Bits in
  let input_data_width = width !(inputs.rxd) in
  sim_preamble_sfd sim inputs;
  let data =
    sresize ~width:data_width (of_string "128'hBBBB_4444_DDDD_1111_AAAA_5555_CCCC_3333")
  in
  let input_data = bswap data in
  let output_data = ref [] in
  for i = 0 to (data_width / input_data_width) - 1 do
    inputs.rxd := input_data.:[((i + 1) * input_data_width) - 1, i * input_data_width];
    inputs.crsdv := vdd;
    Cyclesim.cycle sim;
    output_data := update_axi_output_data ~output_data ~axi_output:outputs.axi_tx
  done;
  inputs.crsdv := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  output_data := update_axi_output_data ~output_data ~axi_output:outputs.axi_tx;
  output_data
;;

let test_rx_waves (data_lengths : int list) =
  let module Sim = Cyclesim.With_interface (Rx.I) (Rx.O) in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Rx.create scope) in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  Cyclesim.cycle sim;
  let result =
    List.map data_lengths ~f:(fun data_width ->
      !(sim_frame sim inputs outputs data_width) |> List.rev)
  in
  Cyclesim.cycle sim;
  waves, result
;;

let expect_test_rx_waves data_lengths =
  let _waves, result = test_rx_waves data_lengths in
  print_s [%message (result : Bits.Hex.t List.t List.t)]
;;

let%expect_test "test_rx_single_frame" =
  expect_test_rx_waves [ 64 ];
  [%expect {| (result ((32'haaaa5555 32'hcccc3333))) |}]
;;

let%expect_test "test_rx_tkeep_frames" =
  expect_test_rx_waves [ 48 ];
  [%expect {| (result ((32'h5555cccc 16'h3333))) |}]
;;

let%expect_test "test_rx_multiple_frames" =
  expect_test_rx_waves [ 128; 48; 56; 40; 256 ];
  [%expect
    {|
    (result
     ((32'hbbbb4444 32'hdddd1111 32'haaaa5555 32'hcccc3333)
      (32'h5555cccc 16'h3333) (32'haa5555cc 24'hcc3333) (32'h55cccc33 8'h33)
      (32'hffffffff 32'hffffffff 32'hffffffff 32'hffffffff 32'hbbbb4444
       32'hdddd1111 32'haaaa5555 32'hcccc3333)))
    |}]
;;

let sim_data sim (inputs : _ Tx.I.t) (outputs : _ Tx.O.t) data_width ~with_data_gaps =
  let open Bits in
  let input_data_width = 32 in
  let byte_width = 8 in
  let input_data_bytes = input_data_width / byte_width in
  let output_data_width = 2 in
  inputs.clocking.clear := gnd;
  Cyclesim.cycle sim;
  let data = random ~width:data_width in
  let output_data = ref [] in
  let check_outputs () =
    if to_bool !(outputs.txen) then output_data := !(outputs.txd) :: !output_data else ()
  in
  let last_valid_bits = data_width % input_data_width in
  let last_valid_bytes, max_index =
    if last_valid_bits <> 0
    then last_valid_bits / byte_width, data_width / input_data_width
    else input_data_bytes, (data_width / input_data_width) - 1
  in
  for i = 0 to max_index do
    if i = max_index && last_valid_bytes <> 4
    then
      inputs.data_stream.tdata
      := data.:[(last_valid_bytes * 8) - 1, 0]
         @: zero (input_data_width - (last_valid_bytes * 8))
    else
      inputs.data_stream.tdata
      := data.:[( data_width - (i * input_data_width) - 1
                , data_width - ((i + 1) * input_data_width) )];
    inputs.data_stream.tvalid := vdd;
    inputs.data_stream.tstrb := zero input_data_bytes;
    inputs.data_stream.tkeep
    := sll (ones input_data_bytes) ~by:(input_data_bytes - last_valid_bytes);
    inputs.data_stream.tlast := of_bool (i = max_index);
    inputs.data_stream.tuser := gnd @: of_bool (i = 0);
    Cyclesim.cycle sim;
    check_outputs ();
    if with_data_gaps
    then
      for _ = 0 to Random.int_incl 10 30 do
        inputs.data_stream.tvalid := gnd;
        inputs.data_stream.tdata := zero input_data_width;
        Cyclesim.cycle sim;
        check_outputs ()
      done
  done;
  inputs.data_stream.tvalid := gnd;
  Cyclesim.cycle sim;
  check_outputs ();
  Cyclesim.cycle sim;
  let timeout_count = ref 0 in
  let max_timeout_count =
    (data_width + preamble_sfd_bits + fcs_bits) / output_data_width
  in
  while to_bool !(outputs.txen) && !timeout_count < max_timeout_count do
    output_data := !(outputs.txd) :: !output_data;
    timeout_count := !timeout_count + 1;
    Cyclesim.cycle sim
  done;
  for _ = 0 to (ifg_bits / output_data_width) - 1 do
    [%test_result: Bits.t] !(outputs.txen) ~expect:gnd;
    Cyclesim.cycle sim
  done;
  check_tx_data ~output_data:!output_data ~expected_data:data ()
;;

let test_tx_waves (data_lengths : int list) ~with_data_gaps =
  let module Sim = Cyclesim.With_interface (Tx.I) (Tx.O) in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Tx.create scope) in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := vdd;
  inputs.data_stream.tvalid := gnd;
  Cyclesim.cycle sim;
  List.iter data_lengths ~f:(fun data_width ->
    sim_data sim inputs outputs data_width ~with_data_gaps);
  Cyclesim.cycle sim;
  waves
;;

let expect_test_tx_waves data_lengths ~with_data_gaps =
  ignore (test_tx_waves data_lengths ~with_data_gaps : Waveform.t)
;;

let%expect_test "test_tx_single_frame" =
  expect_test_tx_waves [ 64 * 8 ] ~with_data_gaps:false
;;

let%expect_test "test_tx_tkeep_frames" =
  expect_test_tx_waves [ 50 * 8 ] ~with_data_gaps:false
;;

let%expect_test "test_multiple_frames" =
  expect_test_tx_waves [ 64 * 8; 78 * 8; 128 * 8; 223 * 8 ] ~with_data_gaps:false
;;

let%expect_test "test_multiple_frames_with_data_gaps " =
  expect_test_tx_waves [ 64 * 8; 78 * 8; 128 * 8; 223 * 8 ] ~with_data_gaps:true
;;

let test_udp_packet_generator =
  let module Sim = Cyclesim.With_interface (Udp_packet_generator.I) (Tx.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (fun i ->
      let udp_packet =
        Udp_packet_generator.create (Scope.sub_scope scope "udp_packet_generator") i
      in
      Tx.create
        (Scope.sub_scope scope "ethernet_tx")
        { Tx.I.clocking = i.clocking; data_stream = udp_packet.axi_tx })
  in
  let open Bits in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let config = get_config in
  let udp_packet_size =
    Ethernet_header.sum_of_port_widths
    + Ipv4.sum_of_port_widths
    + Udp.sum_of_port_widths
    + 240
  in
  let axi_word_bytes = Config.data_bits / 8 in
  let wait_counter_width = 19 in
  Udp_packet_generator.Config.iter2 inputs.config config ~f:( := );
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  Cyclesim.cycle sim;
  let max_wait_count = to_int_trunc (ones wait_counter_width) in
  for _ = 0 to 10 do
    let output_data = ref [] in
    for
      _ = 0
      to max_wait_count
         + (udp_packet_size / 8 / axi_word_bytes)
         + ((preamble_sfd_bits + udp_packet_size + fcs_bits) / 2)
    do
      Cyclesim.cycle sim;
      if to_bool !(outputs.txen)
      then output_data := !(outputs.txd) :: !output_data
      else ()
    done;
    check_tx_packet
      ~output_data:!output_data
      ~packet:(create_packet_from_fpga ~config ())
      ~loopback:false
      ()
  done;
  waves
;;

let%expect_test "test_udp_packet_generator" =
  let waves = test_udp_packet_generator in
  ignore waves
;;

let test_udp_packet_decoder =
  let module Sim = Cyclesim.With_interface (Rx.I) (Udp_packet_decoder.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (fun i ->
      let rx = Rx.create (Scope.sub_scope scope "ethernet_rx") i in
      Udp_packet_decoder.create
        (Scope.sub_scope scope "udp_packet_decoder")
        { Udp_packet_decoder.I.clocking = i.clocking; axi_rx = rx.axi_tx })
  in
  let open Bits in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := vdd;
  let input_data_width = width !(inputs.rxd) in
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  inputs.rxd := of_string "11";
  inputs.crsdv := vdd;
  inputs.rxerr := gnd;
  Cyclesim.cycle sim;
  sim_preamble_sfd sim inputs;
  let data = of_string "128'hBBBB_4444_DDDD_1111_AAAA_5555_CCCC_3333" in
  let packet = create_packet_from_host data in
  let packet_vector = bswap (Packet.Of_bits.pack ~rev:true packet) in
  let total_width = Packet.sum_of_port_widths - (max_data_bits - width data) in
  let output_data = ref [] in
  for i = 0 to (total_width / input_data_width) - 1 do
    inputs.rxd := packet_vector.:[((i + 1) * input_data_width) - 1, i * input_data_width];
    inputs.crsdv := vdd;
    Cyclesim.cycle sim;
    output_data := update_axi_output_data ~output_data ~axi_output:outputs.axi_tx
  done;
  inputs.crsdv := Bits.gnd;
  for _ = 0 to 2 do
    Cyclesim.cycle sim;
    output_data := update_axi_output_data ~output_data ~axi_output:outputs.axi_tx
  done;
  waves, concat_lsb !output_data
;;

let%expect_test "test_udp_packet_decoder" =
  let _waves, result = test_udp_packet_decoder in
  print_s [%message (result : Bits.Hex.t)];
  [%expect {| (result 128'hbbbb4444dddd1111aaaa5555cccc3333) |}]
;;

let test_loopback ?(rx_error = false) () =
  let module Sim = Cyclesim.With_interface (Rx.I) (Tx.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (fun i ->
      let rx = Rx.create (Scope.sub_scope scope "ethernet_rx") i in
      let%tydi { axi_udp_packet } =
        Udp_packet_stream.create
          (Scope.sub_scope scope "udp_packet_stream")
          { Udp_packet_stream.I.clocking = i.clocking
          ; config =
              Udp_packet_stream.Config.{ dst_mac = Signal.of_string "48'h0018_3e04_c882" }
          ; axi_rx = rx.axi_tx
          }
      in
      Tx.create
        (Scope.sub_scope scope "ethernet_tx")
        { clocking = i.clocking; data_stream = axi_udp_packet })
  in
  let open Bits in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := vdd;
  let input_data_width = width !(inputs.rxd) in
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  inputs.rxd := of_string "00";
  inputs.crsdv := vdd;
  inputs.rxerr := gnd;
  Cyclesim.cycle sim;
  sim_preamble_sfd sim inputs;
  let data = of_string "128'hBBBB_4444_DDDD_1111_AAAA_5555_CCCC_3333" in
  let packet = create_packet_from_host ~config:get_config data in
  let packet_vector = bswap (Packet.Of_bits.pack ~rev:true packet) in
  let fcs = random ~width:32 in
  let total_width = Packet.sum_of_port_widths - (max_data_bits - width data) in
  let input_data = fcs @: sel_bottom packet_vector ~width:total_width in
  let output_data = ref [] in
  let check_outputs () =
    if to_bool !(outputs.txen) then output_data := !(outputs.txd) :: !output_data else ()
  in
  for i = 0 to ((total_width + 32) / input_data_width) - 1 do
    inputs.rxd := input_data.:[((i + 1) * input_data_width) - 1, i * input_data_width];
    inputs.crsdv := vdd;
    inputs.rxerr := if i = 150 && rx_error then vdd else gnd;
    check_outputs ();
    Cyclesim.cycle sim
  done;
  inputs.crsdv := gnd;
  for _ = 0 to 2 do
    check_outputs ();
    Cyclesim.cycle sim
  done;
  let timeout_count = ref 0 in
  let output_data_width = width !(outputs.txd) in
  let max_timeout_count =
    (width data + preamble_sfd_bits + fcs_bits) / output_data_width
  in
  while to_bool !(outputs.txen) && !timeout_count < max_timeout_count do
    check_outputs ();
    Cyclesim.cycle sim;
    timeout_count := !timeout_count + 1
  done;
  for _ = 0 to 100 do
    check_outputs ();
    Cyclesim.cycle sim
  done;
  check_tx_packet ~output_data:!output_data ~packet ~loopback:true ~rx_error ();
  waves
;;

let%expect_test "test_loopback" =
  let waves = test_loopback ~rx_error:false in
  ignore waves
;;

let%expect_test "test_loopback_with_error" =
  let waves = test_loopback ~rx_error:true in
  ignore waves
;;
