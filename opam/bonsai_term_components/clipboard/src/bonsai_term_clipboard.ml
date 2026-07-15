open! Core
module Selection = Xclip.Selection
open Bonsai_term
open Bonsai.Let_syntax

module Private = struct
  let selection_to_osc52_char = function
    | Selection.Primary -> 'p'
    | Selection.Clipboard -> 'c'
  ;;

  let osc52_escape_sequence selection content =
    let selection_char = selection_to_osc52_char selection in
    let encoded_content = Base64.encode_string content in
    sprintf "\x1b]52;%c;%s\x07" selection_char encoded_content
  ;;
end

let get_clipboard_contents = Xclip.get_clipboard_contents

let set_clipboard_contents_via_osc52 ~write_string_to_tty selection content =
  let escape_sequence = Private.osc52_escape_sequence selection content in
  write_string_to_tty escape_sequence
;;

let copy_to_clipboard (local_ graph) =
  let write_string_to_tty = Bonsai_term.Expert.Write_to_tty.write_string_to_tty graph in
  let%arr write_string_to_tty in
  fun selection content ->
    (* We intentionally attempt both mechanisms; different terminal environments support
       different clipboard strategies. *)
    let osc52_result =
      let%bind.Effect () =
        set_clipboard_contents_via_osc52 ~write_string_to_tty selection content
      in
      Effect.Or_error.return ()
    and xclip_result =
      match am_running_test with
      | true ->
        (* NOTE: We do not want to run [xclip] when we are in an expect test environment. *)
        Effect.Or_error.return ()
      | false ->
        Effect.of_deferred_thunk (fun () ->
          Xclip.set_clipboard_contents selection content)
    in
    let%bind.Effect osc52_result and xclip_result in
    match osc52_result, xclip_result with
    | Ok (), _ | _, Ok () -> Effect.Or_error.return ()
    | Error osc52_err, Error xclip_err ->
      Effect.return
        (Or_error.error_s
           [%message
             "Failed to copy to clipboard via both OSC 52 and xclip"
               (osc52_err : Error.t)
               (xclip_err : Error.t)])
;;
