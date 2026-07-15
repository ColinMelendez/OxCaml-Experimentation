open Core
open Hardcaml
open Hardcaml_waveterm

include struct
  open Hardcaml_hobby_boards_kernel
  include Ethernet_types
  include Ethernet_utils.For_testing
  module Udp_packet_stream = Udp_packet_stream
  module Axi = Ethernet.Axi32

  let word_size = 32
end

let sim_packet
  sim
  (inputs : _ Udp_packet_stream.I.t)
  (outputs : _ Udp_packet_stream.O.t)
  (packet : Bits.t Packet.t)
  with_data_gaps
  strip_fcs_and_swap_address
  =
  let open Bits in
  let data_width = (to_int_trunc packet.udp_header.length * 8) - Udp.sum_of_port_widths in
  Cyclesim.cycle sim;
  let fcs = random ~width:32 in
  let packet_width = Packet.sum_of_port_widths - (max_data_bits - data_width) in
  let packet_vector =
    sel_top (Packet.Of_bits.pack ~rev:true packet) ~width:packet_width @: fcs
  in
  let total_width = width packet_vector in
  let max_index, remaining_bits =
    if Int.equal (total_width % word_size) 0
    then (total_width / word_size) - 2, word_size
    else (total_width / word_size) - 1, total_width % word_size
  in
  let output_data = ref [] in
  let expected_output = ref [] in
  for i = 0 to max_index do
    inputs.axi_rx.tvalid := vdd;
    let data_word =
      packet_vector.:[( total_width - (i * word_size) - 1
                      , total_width - ((i + 1) * word_size) )]
    in
    inputs.axi_rx.tdata := data_word;
    inputs.axi_rx.tuser := if Int.equal i 0 then of_string "01" else of_string "00";
    expected_output := data_word :: !expected_output;
    Cyclesim.cycle sim;
    output_data := update_axi_output_data ~output_data ~axi_output:outputs.axi_udp_packet;
    if with_data_gaps
    then
      for _ = 0 to 14 do
        inputs.axi_rx.tvalid := gnd;
        inputs.axi_rx.tdata := zero word_size;
        Cyclesim.cycle sim;
        output_data
        := update_axi_output_data ~output_data ~axi_output:outputs.axi_udp_packet
      done
    else ()
  done;
  inputs.axi_rx.tvalid := vdd;
  inputs.axi_rx.tdata
  := if word_size - remaining_bits > 0
     then
       sel_bottom packet_vector ~width:remaining_bits @: zero (word_size - remaining_bits)
     else sel_bottom packet_vector ~width:remaining_bits;
  inputs.axi_rx.tkeep := sll (ones 4) ~by:((word_size - remaining_bits) / 8);
  inputs.axi_rx.tlast := vdd;
  expected_output := fcs :: !expected_output;
  Cyclesim.cycle sim;
  for _ = 0 to 14 do
    Axi.Source.iter2 inputs.axi_rx (Axi.Source.Of_bits.zero ()) ~f:( := );
    output_data := update_axi_output_data ~output_data ~axi_output:outputs.axi_udp_packet;
    Cyclesim.cycle sim;
    if with_data_gaps
    then
      for _ = 0 to 14 do
        output_data
        := update_axi_output_data ~output_data ~axi_output:outputs.axi_udp_packet;
        Cyclesim.cycle sim
      done
    else ()
  done;
  if to_bool
       (packet.ipv4_header.protocol
        ==:. udp_protocol
        &: (!(inputs.config.dst_mac) ==: packet.ethernet_header.dst_mac))
  then (
    let expected_packet =
      if strip_fcs_and_swap_address
      then (
        let loopback_packet = swap_addressing packet in
        sel_top (Packet.Of_bits.pack ~rev:true loopback_packet) ~width:packet_width)
      else packet_vector
    in
    [%test_result: Bits.Hex.t] (concat_lsb !output_data) ~expect:expected_packet;
    print_string "Packet received; ")
  else (
    [%test_result: bool] (List.is_empty !output_data) ~expect:true;
    print_string "No packet received; ")
;;

let test_udp_packet_stream_waves
  ?(with_data_gaps = false)
  ?(strip_fcs_and_swap_address = false)
  ()
  =
  let module Sim = Cyclesim.With_interface (Udp_packet_stream.I) (Udp_packet_stream.O) in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    if strip_fcs_and_swap_address
    then Sim.create ~config:Cyclesim.Config.trace_all (Udp_packet_stream.create scope)
    else
      Sim.create
        ~config:Cyclesim.Config.trace_all
        (Udp_packet_stream.create_internal scope)
  in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let valid_dst_mac = random ~width:Ethernet_header.port_widths.dst_mac in
  let config = { get_config with fpga_mac = valid_dst_mac } in
  let udp_packet_1 = create_packet_from_host ~config (of_string "32'hAAAA_AAAA") in
  let non_udp_packet =
    create_packet_from_host
      ~config
      ~protocol:(of_int_trunc ~width:Ipv4.port_widths.protocol tcp_protocol)
      (of_string "32'hAAAA_AAAA")
  in
  let udp_packet_2 = create_packet_from_host ~config (of_string "48'hF0F0_F0F0_F0F0") in
  let udp_packet_3 =
    create_packet_from_host
      ~config
      (of_string "128'hBBBB_4444_DDDD_1111_AAAA_5555_CCCC_3333")
  in
  let udp_packet_with_invalid_dst_mac =
    create_packet_from_host (of_string "32'hAAAA_AAAA")
  in
  let udp_packet_4 =
    create_packet_from_host
      ~config
      (of_string "128'hBBBB_4444_DDDD_1111_AAAA_5555_CCCC_3333")
  in
  let packets =
    [ udp_packet_1
    ; non_udp_packet
    ; udp_packet_2
    ; udp_packet_3
    ; udp_packet_with_invalid_dst_mac
    ; udp_packet_4
    ]
  in
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  inputs.config.dst_mac := valid_dst_mac;
  Axi.Source.iter2 inputs.axi_rx (Axi.Source.Of_bits.zero ()) ~f:( := );
  Cyclesim.cycle sim;
  List.iter packets ~f:(fun packet ->
    sim_packet sim inputs outputs packet with_data_gaps strip_fcs_and_swap_address);
  waves
;;

let expect_udp_packet_stream_waves
  ?(with_data_gaps = false)
  ?(strip_fcs_and_swap_address = false)
  ()
  =
  ignore
    (test_udp_packet_stream_waves ~with_data_gaps ~strip_fcs_and_swap_address ()
     : Waveform.t)
;;

let%expect_test "test_only_udp_packet_data_is_decoded" =
  let waves = expect_udp_packet_stream_waves () in
  ignore waves;
  [%expect
    {| Packet received; No packet received; Packet received; Packet received; No packet received; Packet received; |}]
;;

let%expect_test "test_only_udp_packet_data_is_decoded_with_gaps" =
  expect_udp_packet_stream_waves ~with_data_gaps:true ~strip_fcs_and_swap_address:false ();
  [%expect
    {| Packet received; No packet received; Packet received; Packet received; No packet received; Packet received; |}]
;;

let%expect_test "test_only_udp_packet_data_is_decoded_and_swapped" =
  expect_udp_packet_stream_waves ~with_data_gaps:false ~strip_fcs_and_swap_address:true ();
  [%expect
    {| Packet received; No packet received; Packet received; Packet received; No packet received; Packet received; |}]
;;

let%expect_test "test_only_udp_packet_data_is_decoded_and_swapped_with_gaps" =
  expect_udp_packet_stream_waves ~with_data_gaps:true ~strip_fcs_and_swap_address:true ();
  [%expect
    {| Packet received; No packet received; Packet received; Packet received; No packet received; Packet received; |}]
;;
