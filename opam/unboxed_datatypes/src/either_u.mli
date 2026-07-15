open Base

(* This template should support all non-product kinds; product kinds can be added ad-hoc
   as they are necessary. *)

(* Define all templated versions of [t] *)
[%%template:
[@@@kind.default
  k
  = ( value
    , immediate
    , immediate64
    , float64
    , bits32
    , bits64
    , vec128
    , word
    , value_or_null & value_or_null
    , value_or_null & float64
    , value_or_null & bits32
    , value_or_null & bits64
    , value_or_null & vec128
    , value_or_null & word
    , value & value
    , value & float64
    , value & bits32
    , value & bits64
    , value & vec128
    , value & word
    , immediate & immediate
    , float64 & float64
    , bits32 & bits32
    , bits64 & bits64
    , vec128 & vec128
    , word & word
    , (_ : (_ : value_or_null & (value_or_null & value_or_null)))
    , (_ : (_ : (value_or_null & value_or_null) & (value_or_null & value_or_null))) )]

type ('a : k, 'b : k, 'c : k) tag =
  | First : (('a, 'b, 'a) tag[@kind k])
  | Second : (('a, 'b, 'b) tag[@kind k])

type ('a : k, 'b : k) t : immediate & k =
  | T : #((('a, 'b, 'c) tag[@kind k]) * 'c) -> (('a, 'b) t[@kind k])
[@@unboxed] [@@deriving globalize]

(* template end *)]

(* Only defined for [value]: *)
[%%template:
[@@@mode.default m = (global, local)]

val of_either : ('a, 'b) Either.t @ m -> ('a, 'b) t @ m
val to_either : ('a, 'b) t @ m -> ('a, 'b) Either.t @ m
(* template end *)]

(* All functions and submodules below are templated over the kind of [t] *)
[%%template:
[@@@kind.default
  k
  = ( value
    , immediate
    , immediate64
    , float64
    , bits32
    , bits64
    , vec128
    , word
    , value_or_null & value_or_null
    , value_or_null & float64
    , value_or_null & bits32
    , value_or_null & bits64
    , value_or_null & vec128
    , value_or_null & word
    , value & value
    , value & float64
    , value & bits32
    , value & bits64
    , value & vec128
    , value & word
    , immediate & immediate
    , float64 & float64
    , bits32 & bits32
    , bits64 & bits64
    , vec128 & vec128
    , word & word
    , (_ : (_ : value_or_null & (value_or_null & value_or_null)))
    , (_ : (_ : (value_or_null & value_or_null) & (value_or_null & value_or_null))) )]

val is_first : ((_, _) t[@kind k]) @ local -> bool [@@zero_alloc]
val is_second : ((_, _) t[@kind k]) @ local -> bool [@@zero_alloc]

val hash_fold_t
  :  'a Ppx_hash_lib.hash_fold
  -> 'b Ppx_hash_lib.hash_fold
  -> (('a, 'b) t[@kind k]) Ppx_hash_lib.hash_fold

module type Focused := sig
  type nonrec ('a : k, 'b : k) t : immediate & k

  [%%template:
  [@@@mode.default m = (global, local)]

  val return : 'a @ m -> ('a, _) t @ m [@@zero_alloc]
  val bind : ('a, 'b) t @ m -> f:('a @ m -> ('c, 'b) t @ m) @ local -> ('c, 'b) t @ m
  val map : ('a, 'b) t @ m -> f:('a @ m -> 'c @ m) @ local -> ('c, 'b) t @ m
  val value : ('a, _) t @ m -> default:'a @ m -> 'a @ m [@@zero_alloc]

  val combine
    :  ('a, 'd) t @ m
    -> ('b, 'd) t @ m
    -> f:('a @ m -> 'b @ m -> 'c @ m) @ local
    -> other:('d @ m -> 'd @ m -> 'd @ m) @ local
    -> ('c, 'd) t @ m

  (* template end *)]

  module Let_syntax : sig
    val return : 'a -> ('a, _) t [@@zero_alloc]
    val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) @ local -> ('c, 'b) t
    val ( >>| ) : ('a, 'b) t -> ('a -> 'c) @ local -> ('c, 'b) t

    module Let_syntax : sig
      val return : 'a -> ('a, _) t [@@zero_alloc]
      val bind : ('a, 'b) t -> f:('a -> ('c, 'b) t) @ local -> ('c, 'b) t
      val map : ('a, 'b) t -> f:('a -> 'c) @ local -> ('c, 'b) t

      module Open_on_rhs : sig end
    end
  end

  module Local : sig
    module Let_syntax : sig
      val return : 'a @ local -> ('a, _) t @ local [@@zero_alloc]

      val ( >>= )
        :  ('a, 'b) t @ local
        -> ('a @ local -> ('c, 'b) t @ local) @ local
        -> ('c, 'b) t @ local

      val ( >>| )
        :  ('a, 'b) t @ local
        -> ('a @ local -> 'c @ local) @ local
        -> ('c, 'b) t @ local

      module Let_syntax : sig
        val return : 'a @ local -> ('a, _) t @ local [@@zero_alloc]

        val bind
          :  ('a, 'b) t @ local
          -> f:('a @ local -> ('c, 'b) t @ local) @ local
          -> ('c, 'b) t @ local

        val map
          :  ('a, 'b) t @ local
          -> f:('a @ local -> 'c @ local) @ local
          -> ('c, 'b) t @ local

        module Open_on_rhs : sig end
      end
    end
  end
end

module First : Focused [@kind k] with type ('a : k, 'b : k) t = (('a, 'b) t[@kind k])
module Second : Focused [@kind k] with type ('a : k, 'b : k) t = (('b, 'a) t[@kind k])

(* Functions templated over allocation in addition to [t]'s kind *)
[%%template:
[@@@alloc.default a @ m = (heap @ global, stack @ local)]
[@@@kind.default k]

val sexp_of_t
  :  ('a @ m -> Sexp.t @ m) @ local
  -> ('b @ m -> Sexp.t @ m) @ local
  -> (('a, 'b) t[@kind k]) @ m
  -> Sexp.t @ m]

(* Functions templated over locality in addition to [t]'s kind *)
[%%template:
[@@@mode.default m = (global, local)]
[@@@kind.default k]

val compare
  :  ('a @ m -> 'a @ m -> int) @ local
  -> ('b @ m -> 'b @ m -> int) @ local
  -> (('a, 'b) t[@kind k]) @ m
  -> (('a, 'b) t[@kind k]) @ m
  -> int

val equal
  :  ('a @ m -> 'a @ m -> bool) @ local
  -> ('b @ m -> 'b @ m -> bool) @ local
  -> (('a, 'b) t[@kind k]) @ m
  -> (('a, 'b) t[@kind k]) @ m
  -> bool

val swap : (('a, 'b) t[@kind k]) @ m -> (('b, 'a) t[@kind k]) @ m [@@zero_alloc]
val value : (('a, 'a) t[@kind k]) @ m -> 'a @ m [@@zero_alloc]

val iter
  :  (('a, 'b) t[@kind k]) @ m
  -> first:('a @ m -> unit)
  -> second:('b @ m -> unit)
  -> unit

val first : 'a @ m -> (('a, _) t[@kind k]) @ m [@@zero_alloc]
val second : 'b @ m -> ((_, 'b) t[@kind k]) @ m [@@zero_alloc]

(* template end *)]

(* Functions templated over locality and the return kind in addition to [t]'s kind *)
[%%template:
[@@@mode.default m = (global, local)]
[@@@kind.default k]

(* Instead of adding a new kind here, consider matching on the representation, as a new
   kind will create a new version of this function for every kind that [t] is templated
   over. *)
[@@@kind.default
  k' = (value, immediate, immediate64, float64, bits32, bits64, vec128, word, k)]

val value_map
  : ('a : k) ('b : k) ('c : k').
  (('a, 'b) t[@kind k]) @ m
  -> first:('a @ m -> 'c @ m) @ local
  -> second:('b @ m -> 'c @ m) @ local
  -> 'c @ m

val map
  : ('a : k) ('b : k) ('c : k') ('d : k').
  (('a, 'b) t[@kind k]) @ m
  -> first:('a @ m -> 'c @ m) @ local
  -> second:('b @ m -> 'd @ m) @ local
  -> (('c, 'd) t[@kind k']) @ m

(* template end *)]

(* template end *)]
