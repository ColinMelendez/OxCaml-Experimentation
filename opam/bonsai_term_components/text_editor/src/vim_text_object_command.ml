open! Core

module Verb = struct
  type t =
    | Change
    | Delete
  [@@deriving sexp_of]
end

module Word_kind = struct
  type t =
    | Word
    | WORD
  [@@deriving sexp_of]
end

module Delimited_target = struct
  type t =
    | Parenthesis
    | Square_bracket
    | Curly_bracket
    | Double_quote
    | Single_quote
  [@@deriving sexp_of]
end

module Target = struct
  type t =
    | Word of Word_kind.t
    | Delimited of Delimited_target.t
  [@@deriving sexp_of]

  let of_char = function
    | 'w' -> Some (Word Word_kind.Word)
    | 'W' -> Some (Word Word_kind.WORD)
    | '(' | ')' -> Some (Delimited Delimited_target.Parenthesis)
    | '[' | ']' -> Some (Delimited Delimited_target.Square_bracket)
    | '{' | '}' -> Some (Delimited Delimited_target.Curly_bracket)
    | '"' -> Some (Delimited Delimited_target.Double_quote)
    | '\'' -> Some (Delimited Delimited_target.Single_quote)
    | _ -> None
  ;;
end

module Adverb = struct
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
