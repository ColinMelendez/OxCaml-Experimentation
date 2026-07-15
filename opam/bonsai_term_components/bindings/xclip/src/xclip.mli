open! Core
open Async

(** A very simple module for getting clipboard contents using the xclip tool *)

module Selection : sig
  type t =
    | Primary
    | Clipboard
end

val get_clipboard_contents : Selection.t -> string list Or_error.t Deferred.t
val set_clipboard_contents : Selection.t -> string -> unit Or_error.t Deferred.t
