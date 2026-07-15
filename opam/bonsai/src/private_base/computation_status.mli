open! Core

type 'input t =
  | Active of 'input
  | Inactive
[@@deriving sexp_of]

val of_option : 'a option -> 'a t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val map : 'a t -> f:('a -> 'b) -> 'b t
