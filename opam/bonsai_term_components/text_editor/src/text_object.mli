open! Core

(** A tiny abstraction for representing start / stop offsets in a zed rope. *)
type t =
  { start : int
  ; stop : int
  }
