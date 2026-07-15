@@ portable

(** This is a trie that remembers the insertion order of its values, but only within a
    single prefix. *)
type t

val create : unit -> t
val add : t -> ?only_if_empty:bool -> string list -> unit
val depth_first_traversal : t -> string list list
