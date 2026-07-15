@@ portable

(** An unboxed version of {!Base.Option} with a two-register layout *)
open Base

[%%template:
[@@@kind k_triple = (bits64 & bits64 & value_or_null)]

[@@@kind.default
  k
  = ( base_or_null
    , bits8
    , bits16
    , void
    , base_or_null & base_or_null
    , (value_or_null & base_or_null) & base_or_null
    , (_ : (_ : base_or_null & (value_or_null & value_or_null)))
    , (_ : (_ : (value_or_null & base_or_null) & (value_or_null & value_or_null)))
    , (_ : (_ : bits64 & (bits64 & word)))
    , (value_or_null, float64, bits64, k_triple) & k_triple
    , (_ : (_ : (value_or_null & bits64) & ((value_or_null & bits64) & word))) )]

type none : k

type ('a : k, 'b : k) tag =
  | None : ('a : k). (('a, (none[@kind k])) tag[@kind k])
  | Some : ('a : k). (('a, 'a) tag[@kind k])

type ('a : k) t : immediate & k =
  | T : ('a : k) ('b : k). #((('a, 'b) tag[@kind k]) * 'b) -> ('a t[@kind k])
[@@unboxed] [@@deriving globalize, sexp ~stackify]

val some : ('a : k). 'a @ m -> ('a t[@kind k]) @ m
[@@mode m = (global, local)] [@@zero_alloc strict]

val none : ('a : k). unit -> ('a t[@kind k]) [@@zero_alloc strict]
val is_none : ('a : k). ('a t[@kind k]) @ immutable local -> bool [@@zero_alloc strict]
val is_some : ('a : k). ('a t[@kind k]) @ immutable local -> bool [@@zero_alloc strict]]

[%%template:
[@@@kind.default k = base_or_null]
[@@@mode.default c = (uncontended, shared, contended)]

val box : ('a : k). ('a t[@kind k]) @ c -> ('a Option.t[@kind k]) @ c
val unbox : ('a : k). ('a Option.t[@kind k]) @ c -> ('a t[@kind k]) @ c]
