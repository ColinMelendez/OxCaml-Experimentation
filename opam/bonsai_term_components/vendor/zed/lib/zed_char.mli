(** Minimal support for Unicode characters *)

type t = private int [@@deriving sexp, compare ~localize, bin_io]

val of_int : int -> t
val to_int : t -> int
val code : t -> int
val of_char : char -> t
val to_char : t -> char
val uppercase : t -> t
val lowercase : t -> t
val is_latin1 : t -> bool
val is_space : t -> bool
val escaped : t -> string
