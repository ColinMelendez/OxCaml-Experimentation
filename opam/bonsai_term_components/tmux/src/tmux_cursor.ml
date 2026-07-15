open! Core
open Bonsai_term

type t =
  { position : Position.t
  ; cursor_character : string
  }
[@@deriving sexp_of]
