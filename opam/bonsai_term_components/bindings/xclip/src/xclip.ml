open! Core
open Async

module Selection = struct
  type t =
    | Primary
    | Clipboard

  let to_arg_string t =
    match t with
    | Primary -> "primary"
    | Clipboard -> "clipboard"
  ;;
end

let get_clipboard_contents selection =
  Process.run_lines
    ~prog:"xclip"
    ~args:[ "-selection"; Selection.to_arg_string selection; "-o" ]
    ()
;;

let set_clipboard_contents selection content =
  let open Async in
  let%bind process =
    Process.create
      ~prog:"xclip"
      ~args:[ "-selection"; Selection.to_arg_string selection; "-i" ]
      ()
    |> Deferred.Or_error.ok_exn
  in
  Writer.write (Process.stdin process) content;
  let%bind () = Writer.close (Process.stdin process) in
  let%bind (_ : Unix.Exit_or_signal.t) = Process.wait process in
  Deferred.Or_error.return ()
;;
