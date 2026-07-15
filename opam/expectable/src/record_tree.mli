@@ portable

open! Core

type 'leaf t =
  | Choose of (string * 'leaf t) list
  | Leaf of 'leaf
  | All of 'leaf t list

val parse_sexp : ?max_depth:int -> Sexp.t -> parse_leaf:(Sexp.t -> 'a) -> 'a t

(** Returns the flattened result of traversing the tree with the given path.

    [All] nodes are flattened into a single list.

    Field lookups that are not found produce [None] results. *)
val get_keypath : 'a t -> string list -> 'a t option list
