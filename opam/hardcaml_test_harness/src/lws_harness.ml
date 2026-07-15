open! Core
open Hardcaml
open Hardcaml_lws

module Make (I : Interface.S) (O : Interface.S) = struct
  module Sim = Cyclesim.With_interface (I) (O)
  module L = Lws_cyclesim.With_interface (I) (O)

  type sim_context = Lws_context.M(I)(O).t

  let run_advanced
    ~(here : [%call_pos])
    ?(waves_config : Waves_config.t option)
    ?random_initial_state
    ?trace
    ?handle_multiple_waveforms_with_same_test_name
    ?test_name_prefix
    ?test_name
    ?print_waves_after_test
    ?run_interactive
    ?clock_mode
    ?timeout
    ?(config = { Lws.Config.default with timeout })
    ~create
    testbench
    =
    Harness_base.run
      ~here
      ?waves_config
      ?random_initial_state
      ?trace
      ?handle_multiple_waveforms_with_same_test_name
      ?test_name_prefix
      ?test_name
      ?print_waves_after_test
      ?run_interactive
      ?clock_mode
      ~cycle_fn:(fun lws -> Lws_cyclesim.cycle lws)
      ~create:(fun ~always_wrap_waveterm ~wave_mode cyclesim_config (_ : Scope.t) ->
        let wave_mode_is_some =
          match wave_mode with
          | None -> false
          | Hardcamlwaveform -> true
          | Vcd _ -> failwith "VCD is not supported by lws_harness yet!"
        in
        let lws =
          L.create
            ~config
            ~backend_specific_config:
              { Lws_cyclesim.Backend_specific_config.default with
                waves = always_wrap_waveterm || wave_mode_is_some
              ; cyclesim_config
              }
            create
        in
        let waveform = Lws_context.waveform (Lws_cyclesim.context lws) in
        lws, waveform)
      (fun lws ->
        let ev = Lws_cyclesim.schedule_task ~here lws testbench in
        Lws_cyclesim.poll_task lws ev)
  ;;

  let run
    ~(here : [%call_pos])
    ?waves_config
    ?random_initial_state
    ?trace
    ?handle_multiple_waveforms_with_same_test_name
    ?test_name_prefix
    ?test_name
    ?print_waves_after_test
    ?run_interactive
    ?clock_mode
    ?timeout
    ?config
    ~create
    testbench
    =
    run_advanced
      ~here
      ?waves_config
      ?random_initial_state
      ?trace
      ?handle_multiple_waveforms_with_same_test_name
      ?test_name_prefix
      ?test_name
      ?print_waves_after_test
      ?run_interactive
      ?clock_mode
      ?timeout
      ?config
      ~create
      (fun h sim_context ->
         let inputs = sim_context.inputs in
         let outputs = sim_context.outputs in
         testbench h ~inputs ~outputs)
  ;;
end
