open Core

let () =
  Command.basic
    ~summary:"Demo of interactive waveform viewer via the test harness"
    (let%map_open.Command () = return () in
     fun () ->
       Hardcaml_step_test_harness_test.Test_interactive.run ~run_interactive:true ())
  |> Command_unix.run
;;
