open! Core
open Bonsai_term

(** [text_wrap ~max_width string] will "word wrap"
    https://en.wikipedia.org/wiki/Typesetting the text. It will break up your text
    attempting to do "word wrapping" so that each line fits in max_width so that the
    output lines fit within max_width.

    [max_width] is treated as [Int.max 5 max_width] so that the weird unicode characters
    don't get weirder. e.g. some unicode characters can take up 2 chars though this
    behavior is not stable across terminal emulators, as some terminal emulators will make
    some emojis look as wide as 4 chars. *)
val text_wrap : ?attrs:Attr.t list -> max_width:int -> string -> View.t

module Text : sig
  type 'attr t =
    { chars : Uchar.t iarray
    ; attr : 'attr
    }
  [@@deriving sexp_of]

  val of_string : attr:'attr -> string -> 'attr t
  val to_string : _ t -> string
end

module Tty_width : sig
  val string_width : string -> int
end

(** [typeset] will "typeset" https://en.wikipedia.org/wiki/Typesetting the text. It will
    break up your text attempting to do "word wrapping" so that each line fits in
    max_width so that the output lines fit within max_width.

    It is important to note that only the "visual" part of the line will be <= max_width.
    If a line has trailing whitespace, that whitespace can exceed [max_width] as long as
    the "visual" width is <= max_width.

    [max_width] is treated as [Int.max 5 max_width] so that the weird unicode characters
    don't get weirder. e.g. some unicode characters can take up 2 chars though this
    behavior is not stable across terminal emulators, as some terminal emulators will make
    some emojis look as wide as 4 chars. *)
val typeset : max_width:int -> 'attr Text.t list list -> 'attr Text.t list list

(** [typeset_line] is line [typeset], but will only do a single line. *)
val typeset_line : max_width:int -> 'attr Text.t list -> 'attr Text.t list list

module For_testing = Typesetting.For_testing
