open! Core
open Async

let () =
  Command_unix.run
    (Command.async_or_error
       ~summary:"oxtop — a light btop-style system monitor (Bonsai_term)"
       (let%map_open.Command () = return () in
        fun () ->
          Bonsai_term.start_with_exit (fun ~exit ~dimensions graph ->
            Oxtop.App.app ~exit ~dimensions graph)))
;;
