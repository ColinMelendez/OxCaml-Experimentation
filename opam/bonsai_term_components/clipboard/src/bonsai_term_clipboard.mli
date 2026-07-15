open! Core
open Async
open Bonsai_term

module Selection : sig
  type t = Xclip.Selection.t =
    | Primary
    | Clipboard
end

(** [get_clipboard_contents] will attempt to read the current clipboard. This works by
    using [xclip] under the hood and may not work in all terminal emulators. *)
val get_clipboard_contents : Selection.t -> string list Or_error.t Deferred.t

(** Attempt to set the clipboard using both:

    - OSC 52
    - [xclip]

    This is intended to work in a wide range of terminals (including "island" where
    [xclip] is unavailable), but it is known to not work in:

    - PuTTY
    - the VS Code integrated terminal (despite it advertising OSC 52 support)

    NOTE: [Selection.t] only works in OS's that distinguish between a primary clipboard
    and a clipboard selection. *)
val copy_to_clipboard
  :  local_ Bonsai.graph
  -> (Selection.t -> string -> unit Or_error.t Effect.t) Bonsai.t

module Private : sig
  val osc52_escape_sequence : Selection.t -> string -> string
end
