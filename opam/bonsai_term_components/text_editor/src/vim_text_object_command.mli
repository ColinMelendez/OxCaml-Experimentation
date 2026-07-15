open! Core

(** This module represents the "available" grammar for the vim motion commands that we
    support. (e.g. caw, diw, daW, ... *)

module Verb : sig
  type t =
    | Change
    | Delete
  [@@deriving sexp_of]
end

module Word_kind : sig
  type t =
    | Word
    | WORD
  [@@deriving sexp_of]
end

module Delimited_target : sig
  type t =
    | Parenthesis
    | Square_bracket
    | Curly_bracket
    | Double_quote
    | Single_quote
  [@@deriving sexp_of]
end

module Target : sig
  type t =
    | Word of Word_kind.t
    | Delimited of Delimited_target.t
  [@@deriving sexp_of]

  val of_char : char -> t option
end

module Adverb : sig
  type t =
    | Around
    | Inside
  [@@deriving sexp_of]
end

type t =
  { verb : Verb.t
  ; adverb : Adverb.t
  ; target : Target.t
  }
[@@deriving sexp_of]
