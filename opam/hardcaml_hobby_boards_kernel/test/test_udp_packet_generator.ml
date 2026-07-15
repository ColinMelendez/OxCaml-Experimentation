open Core
open Hardcaml
open Hardcaml_waveterm

include struct
  open Hardcaml_hobby_boards_kernel
  include Ethernet_types
  include Ethernet_utils.For_testing
  module Udp_packet_generator = Udp_packet_generator
end

let test_tx_waves ~num_udp_packets =
  let module Sim =
    Cyclesim.With_interface (Udp_packet_generator.I) (Udp_packet_generator.O)
  in
  let open Bits in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (Udp_packet_generator.create scope)
  in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let config = get_random_config in
  let packet = create_packet_from_fpga ~config () in
  let udp_packet_size =
    Ethernet_header.sum_of_port_widths + (to_int_trunc packet.ipv4_header.length * 8)
  in
  let expected_udp_packet =
    sel_top (Packet.Of_bits.pack ~rev:true packet) ~width:udp_packet_size
  in
  let wait_counter_width = 19 in
  Udp_packet_generator.Config.iter2 inputs.config config ~f:( := );
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd;
  Cyclesim.cycle sim;
  let max_wait_count = to_int_trunc (ones wait_counter_width) in
  for _ = 0 to num_udp_packets - 1 do
    let output_data = ref [] in
    for _ = 0 to max_wait_count + (udp_packet_size / width !(outputs.axi_tx.tdata)) do
      Cyclesim.cycle sim;
      if to_bool !(outputs.axi_tx.tvalid)
      then output_data := !(outputs.axi_tx.tdata) :: !output_data
      else ()
    done;
    [%test_result: Bits.Hex.t] (concat_lsb !output_data) ~expect:expected_udp_packet
  done;
  waves
;;

let expect_test_tx_waves ~num_udp_packets =
  ignore (test_tx_waves ~num_udp_packets : Waveform.t)
;;

let%expect_test "test_single_udp_packet_is_generated" =
  expect_test_tx_waves ~num_udp_packets:1
;;

let%expect_test "test_multuple_udp_packets_are_generated" =
  expect_test_tx_waves ~num_udp_packets:10
;;
