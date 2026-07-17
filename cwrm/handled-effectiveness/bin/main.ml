open! Core
open Async

let () =
  Command_unix.run
    (Command.async_or_error
       ~summary:
         "handled-effectiveness - step through handled_effect demos (Bonsai_term)"
       (let%map_open.Command () = return () in
        fun () ->
          Bonsai_term.start_with_exit (fun ~exit ~dimensions graph ->
            Handled_effectiveness.App.app ~exit ~dimensions graph)))
;;
