open Base
open! Hardcaml
open Signal

module Config = struct
  let data_bits = 32
  let user_bits = 2
end

module Axi32 = Hardcaml_axi.Stream.Make (Config)

module Rx = struct
  let input_data_width = 2
  let word_size = Config.data_bits
  let word_bytes = word_size / 8
  let preamble_sfd_bits = Ethernet_types.preamble_sfd_bits
  let preamble_sfd_value = of_string "64'hd555_5555_5555_5555"

  module Var = Always.Variable

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; crsdv : 'a
      ; rxerr : 'a
      ; rxd : 'a [@bits input_data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { axi_tx : 'a Axi32.Source.t
      ; rx_error : 'a
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | SFD
      | Data
    [@@deriving sexp_of, enumerate, compare ~localize]
  end

  let create scope ({ clocking; crsdv; rxerr; rxd } : _ I.t) =
    let spec = Types.Clocking.to_spec clocking in
    let%hw_var data_counter =
      Var.reg spec ~width:(Int.ceil_log2 (word_size / input_data_width))
    in
    let%hw_var data_counter_delayed =
      Var.reg spec ~width:(Int.ceil_log2 (word_size / input_data_width))
    in
    let%hw_var byte_counter =
      Var.reg spec ~width:(Int.ceil_log2 (8 / input_data_width))
    in
    let%hw_var byte_buffer = Var.reg spec ~width:8 in
    let preamble_sfd_buffer = Var.reg spec ~width:preamble_sfd_bits in
    let data_buffer = Var.reg spec ~width:word_size in
    let%hw rx_valid = reg spec crsdv in
    let%hw rx_valid_delayed = reg spec rx_valid in
    let%hw rx_data = reg spec rxd in
    let start_of_frame = rx_valid &: ~:rx_valid_delayed in
    let start_of_data = Var.reg spec ~width:1 in
    let%hw end_of_frame = ~:rx_valid &: rx_valid_delayed in
    let%hw preamble_sfd_buffer_next =
      rx_data
      @: sel_top preamble_sfd_buffer.value ~width:(preamble_sfd_bits - input_data_width)
    in
    let byte_buffer_next =
      rx_data @: sel_top byte_buffer.value ~width:(8 - input_data_width)
    in
    let data_buffer_next =
      mux2
        (all_bits_set byte_counter.value)
        (sel_bottom data_buffer.value ~width:(word_size - 8) @: byte_buffer_next)
        data_buffer.value
    in
    let rx_error = Var.wire ~default:gnd () in
    let axi_tx = Axi32.Source.Of_always.wire zero in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module States) ~enable:vdd spec
    in
    Always.(
      compile
        [ start_of_data <-- gnd
        ; rx_error <-- gnd
        ; state.switch
            [ ( Idle
              , [ data_counter <-- zero 4
                ; byte_counter <-- zero 2
                ; preamble_sfd_buffer <-- zero preamble_sfd_bits
                ; when_
                    start_of_frame
                    [ preamble_sfd_buffer <-- preamble_sfd_buffer_next
                    ; state.set_next SFD
                    ]
                ] )
            ; ( SFD
              , [ preamble_sfd_buffer <-- preamble_sfd_buffer_next
                ; when_
                    (preamble_sfd_buffer_next ==: preamble_sfd_value)
                    [ start_of_data <-- vdd; state.set_next Data ]
                ; when_ rxerr [ rx_error <-- vdd; state.set_next Idle ]
                ] )
            ; ( Data
              , [ start_of_data <-- start_of_data.value
                ; byte_counter <-- byte_counter.value +:. 1
                ; byte_buffer <-- byte_buffer_next
                ; data_counter <-- data_counter.value +:. 1
                ; data_buffer <-- data_buffer_next
                ; data_counter_delayed <-- data_counter.value
                ; axi_tx.tdata <-- data_buffer.value
                ; axi_tx.tstrb <-- zero word_bytes
                ; axi_tx.tuser <-- gnd @: start_of_data.value
                ; when_
                    (all_bits_set data_counter_delayed.value)
                    [ axi_tx.tvalid <-- vdd; start_of_data <-- gnd ]
                ; when_
                    end_of_frame
                    [ (let%hw valid_data_bytes =
                         srl data_counter_delayed.value ~by:input_data_width
                       in
                       axi_tx.tkeep
                       <-- (List.init word_bytes ~f:(fun i -> valid_data_bytes >=:. i)
                            |> concat_msb))
                    ; axi_tx.tdata
                      <-- mux
                            (leading_ones axi_tx.tkeep.value -:. 1)
                            [ sll data_buffer.value ~by:(3 * 8)
                            ; sll data_buffer.value ~by:(2 * 8)
                            ; sll data_buffer.value ~by:(1 * 8)
                            ; data_buffer.value
                            ]
                    ; axi_tx.tvalid <-- vdd
                    ; axi_tx.tlast <-- vdd
                    ; state.set_next Idle
                    ]
                ; when_
                    rxerr
                    [ axi_tx.tvalid <-- vdd
                    ; axi_tx.tkeep <-- ones 4
                    ; axi_tx.tlast <-- vdd
                    ; axi_tx.tuser <-- vdd @: start_of_data.value
                    ; rx_error <-- vdd
                    ; state.set_next Idle
                    ]
                ] )
            ]
        ]);
    { O.axi_tx = Axi32.Source.Of_always.value axi_tx; rx_error = rx_error.value }
  ;;

  let hierarchical ?instance scope inputs =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"ethernet_rx" ?instance create inputs
  ;;
end

module Tx = struct
  module Make (M : sig
      val max_packets : int
      val average_packet_size : int
    end) =
  struct
    include Ethernet_types

    let output_data_width = 2
    let word_size = Config.data_bits
    let word_bytes = word_size / 8
    let preamble_sfd_value = of_string "64'h5555_5555_5555_5554"
    let fcs_cycles = fcs_bits / output_data_width
    let ifg_cycles = ifg_bits / output_data_width
    let max_packets = M.max_packets
    let average_packet_words = M.average_packet_size / word_bytes
    let log_max_packets = Int.ceil_log2 max_packets

    module Var = Always.Variable

    module I = struct
      type 'a t =
        { clocking : 'a Types.Clocking.t
        ; data_stream : 'a Axi32.Source.t
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { txen : 'a
        ; txd : 'a [@bits output_data_width]
        ; ready : 'a Axi32.Dest.t
        }
      [@@deriving hardcaml]
    end

    module States = struct
      type t =
        | Idle
        | SFD
        | Data
        | FCS
        | IFG
      [@@deriving sexp_of, enumerate, compare ~localize]
    end

    let crc_polynomial_802_3 = Bits.of_int_trunc ~width:32 0xEDB88320

    module Make_comb (C : Comb.S) = struct
      open C

      let update_bit ~(polynomial : Bits.t) ~crc bit_in =
        (* Add the incoming bit to the top bit of the current CRC. *)
        let last_bit = lsb crc in
        let add_bit = bit_in ^: last_bit in
        (* Shift in a zero. *)
        srl crc ~by:1
        |> bits_msb
        |> (* Add [add_bit] to every bit where the polynomial is nonzero. *)
        List.map2_exn (Bits.bits_msb polynomial) ~f:(fun polynomial_bit crc_bit ->
          if Bits.to_bool polynomial_bit then add_bit ^: crc_bit else crc_bit)
        |> concat_msb
      ;;

      let update_bits ~(polynomial : Bits.t) ~crc bits_in =
        List.fold (bits_lsb bits_in) ~init:crc ~f:(fun crc bit ->
          update_bit ~polynomial ~crc bit)
      ;;
    end

    module Crc = Make_comb (Signal)

    let create scope ({ clocking; data_stream } : _ I.t) =
      let spec = Types.Clocking.to_spec clocking in
      let%hw_var data_counter =
        Var.reg spec ~width:(Int.ceil_log2 (word_size / output_data_width))
      in
      let fcs_counter = Var.reg spec ~width:(Int.ceil_log2 fcs_cycles) in
      let ifg_counter = Var.reg spec ~width:(Int.ceil_log2 ifg_cycles) in
      let preamble_sfd_buffer = Var.reg spec ~width:preamble_sfd_bits in
      let%hw_var data_buffer = Var.reg spec ~width:word_size in
      let data_out = Var.wire ~default:(zero output_data_width) () in
      let data_out_valid = Var.wire ~default:gnd () in
      let%hw_var fcs = Var.reg spec ~width:fcs_bits in
      let%hw_var fcs_next = Var.wire ~default:(zero fcs_bits) () in
      let read_enable = Var.wire ~default:gnd () in
      let ready = Var.reg spec ~width:1 in
      let stored_packets = Var.reg spec ~width:log_max_packets in
      let send_packet = stored_packets.value <>:. 0 in
      let data_stream_errorr =
        data_stream.tvalid &: msb data_stream.tuser &: ready.value
      in
      let data_fifo =
        Fifo.create
          ~scope
          ~capacity:(max_packets * average_packet_words)
          ~clock:clocking.clock
          ~clear:clocking.clear
          ~wr:(data_stream.tvalid &: ready.value)
          ~d:
            (data_stream_errorr
             @: data_stream.tlast
             @: data_stream.tkeep
             @: data_stream.tdata)
          ~rd:read_enable.value
          ()
      in
      let%hw frame_error = msb data_fifo.q in
      let%hw last_data_word =
        data_fifo.q.:[width data_fifo.q - 2, width data_fifo.q - 2]
      in
      let%hw keep = data_fifo.q.:[word_size + word_bytes - 1, word_size] in
      let%hw last_word_valid_bytes = leading_ones keep in
      let%hw last_word_valid_cycles =
        sll (uresize last_word_valid_bytes ~width:(width last_word_valid_bytes + 2)) ~by:2
        -:. 1
      in
      let%hw data_buffer_next =
        mux2
          (data_counter.value ==:. 0)
          (bswap (sel_bottom data_fifo.q ~width:word_size))
          data_buffer.value
      in
      let%hw.Always.State_machine state =
        Always.State_machine.create (module States) ~enable:vdd spec
      in
      Always.(
        compile
          [ ready <-- ~:(all_bits_set stored_packets.value)
          ; read_enable <-- gnd
          ; data_out_valid <-- gnd
          ; fcs_next <-- fcs.value
          ; when_
              (data_stream.tlast &: data_stream.tvalid)
              [ stored_packets <-- stored_packets.value +:. 1 ]
          ; state.switch
              [ ( Idle
                , [ data_counter <-- zero (width data_counter.value)
                  ; ifg_counter <-- zero (Int.ceil_log2 ifg_cycles)
                  ; preamble_sfd_buffer <-- preamble_sfd_value
                  ; fcs <-- ones fcs_bits
                  ; when_ send_packet [ state.set_next SFD ]
                  ] )
              ; ( SFD
                , [ data_out_valid <-- vdd
                  ; data_out
                    <-- sel_top preamble_sfd_buffer.value ~width:output_data_width
                  ; preamble_sfd_buffer
                    <-- sll preamble_sfd_buffer.value ~by:output_data_width
                  ; when_
                      (no_bits_set preamble_sfd_buffer.value)
                      [ data_out_valid <-- vdd
                      ; data_out <-- of_string "11"
                      ; read_enable <-- (vdd &: ~:(data_fifo.empty))
                      ; state.set_next Data
                      ]
                  ] )
              ; ( Data
                , [ data_out_valid <-- vdd
                  ; data_out <-- sel_bottom data_buffer_next ~width:output_data_width
                  ; data_buffer <-- srl data_buffer_next ~by:output_data_width
                  ; data_counter <-- data_counter.value +:. 1
                  ; (fcs_next
                     <--
                     let crc =
                       Crc.update_bits
                         ~polynomial:crc_polynomial_802_3
                         ~crc:fcs.value
                         (sel_bottom data_buffer_next ~width:output_data_width)
                     in
                     msbs crc @: mux2 frame_error ~:(lsb crc) (lsb crc))
                  ; fcs <-- fcs_next.value
                  ; when_
                      (all_bits_set data_counter.value)
                      [ read_enable <-- (vdd &: ~:(data_fifo.empty)) ]
                  ; when_
                      (last_data_word
                       &: (uresize
                             data_counter.value
                             ~width:(width last_word_valid_cycles)
                           ==: last_word_valid_cycles))
                      [ fcs <-- ~:(fcs_next.value)
                      ; stored_packets <-- stored_packets.value -:. 1
                      ; state.set_next FCS
                      ]
                  ] )
              ; ( FCS
                , [ fcs_counter <-- fcs_counter.value +:. 1
                  ; data_out_valid <-- vdd
                  ; data_out <-- sel_bottom fcs.value ~width:output_data_width
                  ; fcs <-- srl fcs.value ~by:output_data_width
                  ; when_ (all_bits_set fcs_counter.value) [ state.set_next IFG ]
                  ] )
              ; ( IFG
                , [ ifg_counter <-- ifg_counter.value +:. 1
                  ; when_ (ifg_counter.value ==:. ifg_cycles - 1) [ state.set_next Idle ]
                  ] )
              ]
          ]);
      { O.txen = data_out_valid.value
      ; O.txd = data_out.value
      ; O.ready = { tready = ready.value }
      }
    ;;

    let hierarchical ?instance scope inputs =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"ethernet_tx" ?instance create inputs
    ;;
  end
end
