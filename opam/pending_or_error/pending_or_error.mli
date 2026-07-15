open! Core

(** A ['a Pending_or_error.t] is a value that comes from an RPC server or some other
    fallible, asynchronous place. It's isomorphic to an ['a Or_error.t option], but
    flattened out and slightly more semantically specific. *)
type 'a t =
  | Pending
  | Error of Error.t
  | Ok of 'a
[@@deriving bin_io, compare ~localize, diff, equal ~localize, quickcheck, sexp, variants]

val of_or_error : 'a Or_error.t -> 'a t
val of_or_error_option : 'a Or_error.t option -> 'a t

(** Treats [Pending] as an error. *)
val to_or_error : 'a t -> 'a Or_error.t

val to_or_error_option : 'a t -> 'a Or_error.t option
val to_option : 'a t -> 'a option @@ portable
val error_s : Sexp.t -> _ t
val tag_error : 'a t -> tag:string -> 'a t
val value : 'a t -> default:'a -> 'a
val value_map : 'a t -> f:('a -> 'b) -> default:'b -> 'b

(** The semantics of [merge] are:
    - If either input is an Error, return an Error
    - If both inputs are Ok, call [f] and return the results
    - If a single input is Ok, return it

    The handling of Error makes this differ from:

    {[
      Option.merge (to_option t1) (to_option t2) ~f
    ]}

    This only merges Ok and Pending states, but never discards errors. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t

include Applicative.S with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val map4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t

val map5
  :  'a t
  -> 'b t
  -> 'c t
  -> 'd t
  -> 'e t
  -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f)
  -> 'f t

val map6
  :  'a t
  -> 'b t
  -> 'c t
  -> 'd t
  -> 'e t
  -> 'f t
  -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
  -> 'g t

val map7
  :  'a t
  -> 'b t
  -> 'c t
  -> 'd t
  -> 'e t
  -> 'f t
  -> 'g t
  -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h)
  -> 'h t

val join_or_error : 'a Or_error.t t -> 'a t
val of_map : ('key, 'data t, 'cmp) Map.t -> ('key, 'data, 'cmp) Map.t t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving sexp, bin_io, stable_witness]
  end
end
