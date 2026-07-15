open Core
open Hardcaml

include struct
  open Hardcaml_hobby_boards_kernel
  include Ethernet_types
  include Ethernet_utils.For_testing
  module Axi = Ethernet.Axi32

  module Udp_packet_decoder = Udp_packet_decoder.Make (struct
      let filter_config =
        Udp_packet_decoder_intf.Filter_config.Destination_mac_filter
          (Signal.of_bits get_config.fpga_mac)
      ;;
    end)

  let word_size = Axi.Source.port_widths.tdata
end

let sim_packet
  sim
  (inputs : _ Udp_packet_decoder.I.t)
  (outputs : _ Udp_packet_decoder.O.t)
  (packet : Bits.t Packet.t)
  with_data_gaps
  =
  let open Bits in
  let data_width = (to_int_trunc packet.udp_header.length * 8) - Udp.sum_of_port_widths in
  Cyclesim.cycle sim;
  let packet_vector = Packet.Of_bits.pack ~rev:true packet in
  let total_width = Packet.sum_of_port_widths - (max_data_bits - data_width) in
  let max_index =
    if Int.equal (total_width % word_size) 0
    then (total_width / word_size) - 1
    else total_width / word_size
  in
  let output_data = ref [] in
  for i = 0 to max_index do
    inputs.axi_rx.tvalid := vdd &: !(outputs.ready.tready);
    inputs.axi_rx.tdata
    := packet_vector.:[( Packet.sum_of_port_widths - (i * word_size) - 1
                       , Packet.sum_of_port_widths - ((i + 1) * word_size) )];
    inputs.axi_rx.tuser := if Int.equal i 0 then of_string "01" else of_string "00";
    Cyclesim.cycle sim;
    if to_bool !(outputs.axi_tx.tvalid)
    then output_data := !(outputs.axi_tx.tdata) :: !output_data
    else ();
    if with_data_gaps
    then
      for _ = 0 to 14 do
        inputs.axi_rx.tvalid := gnd;
        inputs.axi_rx.tdata := zero word_size;
        Cyclesim.cycle sim
      done
    else ()
  done;
  inputs.axi_rx.tvalid := vdd;
  inputs.axi_rx.tdata := random ~width:word_size;
  inputs.axi_rx.tlast := vdd;
  Cyclesim.cycle sim;
  if to_bool !(outputs.axi_tx.tvalid)
  then (
    let valid_bytes = leading_ones !(outputs.axi_tx.tkeep) in
    let byte_mask = sll (ones word_size) ~by:((4 - to_int_trunc valid_bytes) * 8) in
    output_data := (!(outputs.axi_tx.tdata) &: byte_mask) :: !output_data)
  else ();
  for _ = 0 to 14 do
    Axi.Source.iter2 inputs.axi_rx (Axi.Source.Of_bits.zero ()) ~f:( := );
    Cyclesim.cycle sim
  done;
  output_data
;;

let test_rx_waves ?(with_data_gaps = false) () =
  let module Sim = Cyclesim.With_interface (Udp_packet_decoder.I) (Udp_packet_decoder.O)
  in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (Udp_packet_decoder.create scope)
  in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let udp_packet_1 =
    create_packet_from_host ~config:get_config (of_string "32'hAAAA_AAAA")
  in
  let non_udp_packet =
    create_packet_from_host
      ~config:get_config
      ~protocol:(of_int_trunc ~width:Ipv4.port_widths.protocol tcp_protocol)
      (of_string "32'hAAAA_AAAA")
  in
  let udp_packet_2 =
    create_packet_from_host ~config:get_config (of_string "48'hF0F0_F0F0_F0F0")
  in
  let udp_packet_3 =
    create_packet_from_host
      ~config:get_config
      (of_string "128'hBBBB_4444_DDDD_1111_AAAA_5555_CCCC_3333")
  in
  let invalid_dst_mac_udp_packet =
    create_packet_from_host (of_string "128'hBBBB_4444_DDDD_1111_AAAA_5555_CCCC_3333")
  in
  let packets =
    [ udp_packet_1
    ; non_udp_packet
    ; udp_packet_2
    ; udp_packet_3
    ; invalid_dst_mac_udp_packet
    ]
  in
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  Axi.Source.iter2 inputs.axi_rx (Axi.Source.Of_bits.zero ()) ~f:( := );
  Cyclesim.cycle sim;
  let result =
    List.map packets ~f:(fun packet ->
      !(sim_packet sim inputs outputs packet with_data_gaps) |> List.rev)
  in
  waves, result
;;

let expect_test_rx_waves ~with_data_gaps =
  let _waves, result = test_rx_waves ~with_data_gaps () in
  print_s [%message (result : Bits.Hex.t List.t List.t)]
;;

let%expect_test "test_only_udp_packet_data_is_decoded" =
  expect_test_rx_waves ~with_data_gaps:false;
  [%expect
    {|
    (result
     ((32'haaaaaaaa) () (32'hf0f0f0f0 32'hf0f00000)
      (32'hbbbb4444 32'hdddd1111 32'haaaa5555 32'hcccc3333) ()))
    |}]
;;

let%expect_test "test_only_udp_packet_data_is_decoded_with_data_gaps" =
  expect_test_rx_waves ~with_data_gaps:true;
  [%expect
    {|
    (result
     ((32'haaaaaaaa) () (32'hf0f0f0f0 32'hf0f00000)
      (32'hbbbb4444 32'hdddd1111 32'haaaa5555 32'hcccc3333) ()))
    |}]
;;
