@@ portable

open! Base

(** Utilities for unboxed int64s. This module is mostly a copy of Base's Int64 module, but
    with much functionality missing because it can't yet be implemented for unboxed int64s
    or unboxed types generally.

    It's part of the same family of libraries as `lib/float_u`, `lib/nativeint_u`, and
    `lib/int32_u`. They share similar project structures, conventions, and tests. *)
type t = int64# [@@deriving globalize, quickcheck]

module Boxed = Base.Int64

(** These definitions are available. They're included from [O] below.
    {[
      external box : int64# -> (int64[@local_opt]) = "%box_int64"
      external unbox : (int64[@local_opt]) -> int64# = "%unbox_int64"
    ]} *)

(** Synonyms for [box] and [unbox]. *)

external to_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external of_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"

(** {1 [Int_intf.S] inlined} *)

val of_float : float# -> t [@@zero_alloc]
val to_float : t -> float# [@@zero_alloc]

(** {2 Inlined from [Intable]} *)

val of_int_exn : int -> t [@@zero_alloc]
val to_int_exn : t -> int [@@zero_alloc]

(** {2 Inlined from [Identifiable]} *)

(** {3 Inlined from [Sexpable]} *)

val%template sexp_of_t : t @ m -> Sexp.t @ m
[@@alloc a @ m = (heap @ global, stack @ local)]

val t_of_sexp : Sexp.t -> t
val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

(** {3 For [bin_io]} *)

include%template Bin_prot.Binable.S [@mode local] with type t := t

(** {3 For [hash]} *)

include Ppx_hash_lib.Hashable.S with type t := t

(** {3 From [Typerep]} *)

val typerep_of_t : t Typerep_lib.Std.Typerep.t

(** {3 Inlined from [Stringable]} *)

val of_string : string @ local -> t

val%template to_string : t -> string @ l [@@alloc a @ l = (heap @ global, stack @ local)]

[%%template:
[@@@mode.default m = (global, local)]

val equal : t @ m -> t @ m -> bool [@@zero_alloc]

(** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is less
    than [t2], and a positive integer if [t1] is greater than [t2]. *)
val compare : t @ m -> t @ m -> int
[@@zero_alloc]]

val min : t -> t -> t [@@zero_alloc]
val max : t -> t -> t [@@zero_alloc]

(** [ascending] is identical to [compare]. [descending x y = ascending y x]. These are
    intended to be mnemonic when used like [List.sort ~compare:ascending] and
    [List.sort ~cmp:descending], since they cause the list to be sorted in ascending or
    descending order, respectively. *)
val ascending : t -> t -> int
[@@zero_alloc]

val descending : t -> t -> int [@@zero_alloc]

(** [between t ~low ~high] means [low <= t <= high] *)
val between : t -> low:t -> high:t -> bool
[@@zero_alloc]

(** [clamp_exn t ~min ~max] returns [t'], the closest value to [t] such that
    [between t' ~low:min ~high:max] is true.

    Raises if [not (min <= max)]. *)
val clamp_exn : t -> min:t -> max:t -> t
[@@zero_alloc]

(** [clamp_unchecked t ~min ~max] returns [t'], the closest value to [t] such that
    [between t' ~low:min ~high:max] is true.

    Undefined result if [not (min <= max)]. *)
val clamp_unchecked : t -> min:t -> max:t -> t
[@@zero_alloc]

(** {3 Inlined from [Pretty_printer]} *)

val pp : Formatter.t -> t -> unit

(** {3 Inlined from [Comparable.With_zero]} *)

val is_positive : t -> bool [@@zero_alloc]
val is_non_negative : t -> bool [@@zero_alloc]
val is_negative : t -> bool [@@zero_alloc]
val is_non_positive : t -> bool [@@zero_alloc]

(** Returns [Neg], [Zero], or [Pos] in a way consistent with the above functions. *)
val sign : t -> Sign.t
[@@zero_alloc]

(** {3 Inlined from [Invariant.S]} *)
val invariant : t -> unit [@@zero_alloc]

(** [delimiter] is an underscore by default. *)
val to_string_hum : ?delimiter:char -> t -> string

(** {2 Infix operators and constants} *)

val one : t
val minus_one : t
val rem : t -> t -> t [@@zero_alloc]

(** {2 Other common functions} *)

(** {2 Inlined from [Round]} *)

(** [round] rounds an int to a multiple of a given [to_multiple_of] argument, according to
    a direction [dir], with default [dir] being [`Nearest]. [round] will raise if
    [to_multiple_of <= 0]. If the result overflows (too far positive or too far negative),
    [round] returns an incorrect result.

    {v
       | `Down    | rounds toward Int.neg_infinity                          |
       | `Up      | rounds toward Int.infinity                              |
       | `Nearest | rounds to the nearest multiple, or `Up in case of a tie |
       | `Zero    | rounds toward zero                                      |
    v}

    Here are some examples for [round ~to_multiple_of:10] for each direction:

    {v
       | `Down    | {10 .. 19} --> 10 | { 0 ... 9} --> 0 | {-10 ... -1} --> -10 |
       | `Up      | { 1 .. 10} --> 10 | {-9 ... 0} --> 0 | {-19 .. -10} --> -10 |
       | `Zero    | {10 .. 19} --> 10 | {-9 ... 9} --> 0 | {-19 .. -10} --> -10 |
       | `Nearest | { 5 .. 14} --> 10 | {-5 ... 4} --> 0 | {-15 ... -6} --> -10 |
    v}

    For convenience and performance, there are variants of [round] with [dir] hard-coded.
    If you are writing performance-critical code you should use these. *)

val round : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> to_multiple_of:t -> t

(* val round_towards_zero : t -> to_multiple_of:t -> t *)
val round_down : t -> to_multiple_of:t -> t [@@zero_alloc]
val round_up : t -> to_multiple_of:t -> t [@@zero_alloc]
val round_nearest : t -> to_multiple_of:t -> t [@@zero_alloc]

(** {2 Successor and predecessor functions} *)

val succ : t -> t [@@zero_alloc]
val pred : t -> t [@@zero_alloc]

(** {2 Exponentiation} *)

(** [pow base exponent] returns [base] raised to the power of [exponent]. It is OK if
    [base <= 0]. [pow] raises if [exponent < 0], or an integer overflow would occur. *)
val pow : t -> t -> t
[@@zero_alloc]

(** {2 Bit-wise logical operations} *)

(** These are identical to [land], [lor], etc. except they're not infix and have different
    names. *)
val bit_and : t -> t -> t
[@@zero_alloc]

val bit_or : t -> t -> t [@@zero_alloc]
val bit_xor : t -> t -> t [@@zero_alloc]
val bit_not : t -> t [@@zero_alloc]

(** Returns the number of 1 bits in the binary representation of the input. *)
val popcount : t -> t
[@@zero_alloc]

(** {2 Bit-shifting operations}

    The results are unspecified for negative shifts and shifts [>= num_bits]. *)

(** Shifts left, filling in with zeroes. *)
val shift_left : t -> int -> t [@@zero_alloc]

(** Shifts right, preserving the sign of the input. *)
val shift_right : t -> int -> t
[@@zero_alloc]

(** {2 Increment and decrement functions for integer references} *)

(** {2 Conversion functions to related integer types} *)

val of_int32_exn : int32 -> t [@@zero_alloc]
val to_int32_exn : t -> int32
val of_int64_exn : int64 -> t [@@zero_alloc]
val of_nativeint_exn : nativeint -> t
val to_nativeint_exn : t -> nativeint

(** [of_float_unchecked] truncates the given floating point number to an integer, rounding
    towards zero. The result is unspecified if the argument is nan or falls outside the
    range of representable integers. *)
val of_float_unchecked : float# -> t
[@@zero_alloc]

(** The number of bits available in this integer type. Note that the integer
    representations are signed. *)
val num_bits : int64

(** The largest representable integer. *)
val max_value : t

(** The smallest representable integer. *)
val min_value : t

(** Shifts right, filling in with zeroes, which will not preserve the sign of the input. *)
val shift_right_logical : t -> int -> t
[@@zero_alloc]

(** [ceil_pow2 x] returns the smallest power of 2 that is greater than or equal to [x].
    The implementation may only be called for [x > 0]. Example: [ceil_pow2 17 = 32] *)
val ceil_pow2 : t -> t
[@@zero_alloc]

(** [floor_pow2 x] returns the largest power of 2 that is less than or equal to [x]. The
    implementation may only be called for [x > 0]. Example: [floor_pow2 17 = 16] *)
val floor_pow2 : t -> t
[@@zero_alloc]

(** [ceil_log2 x] returns the ceiling of log-base-2 of [x], and raises if [x <= 0]. *)
val ceil_log2 : t -> t
[@@zero_alloc]

(** [floor_log2 x] returns the floor of log-base-2 of [x], and raises if [x <= 0]. *)
val floor_log2 : t -> t
[@@zero_alloc]

(** [is_pow2 x] returns true iff [x] is a power of 2. [is_pow2] raises if [x <= 0]. *)
val is_pow2 : t -> bool
[@@zero_alloc]

(** Returns the number of leading zeros in the binary representation of the input, as an
    integer between 0 and one less than [num_bits].

    The results are unspecified for [t = 0]. *)
val clz : t -> t
[@@zero_alloc]

(** Returns the number of trailing zeros in the binary representation of the input, as an
    integer between 0 and one less than [num_bits].

    The results are unspecified for [t = 0]. *)
val ctz : t -> t
[@@zero_alloc]

(** [range n ~f] runs [f i] for [0 <= i < n]. Analogous to python's [for x in range(n)]. *)
val range : t -> f:(t -> unit) @ local -> unit

(** [range_rev n ~f] runs [f i] for [0 <= i < n], but in reverse. Analogous to python's
    [for x in reversed(range(n))]. *)
val range_rev : t -> f:(t -> unit) @ local -> unit

(** Runs [f i] for [start_incl <= i < end_excl]. "C for loop" semantics. *)
val for_loop : start_incl:t -> end_excl:t -> f:(t -> unit) @ local -> unit

(** Runs [f i] for [start_incl <= i <= end_incl]. "OCaml for loop" semantics, and
    generates less efficient code than [for_loop]. *)
val for_loop_incl : start_incl:t -> end_incl:t -> f:(t -> unit) @ local -> unit

(** A sub-module designed to be opened to make working with ints more convenient. *)
module O : sig
  val ( + ) : t -> t -> t [@@zero_alloc]
  val ( - ) : t -> t -> t [@@zero_alloc]
  val ( * ) : t -> t -> t [@@zero_alloc]
  val ( / ) : t -> t -> t [@@zero_alloc]
  val ( ~- ) : t -> t [@@zero_alloc]

  (** Integer exponentiation *)
  val ( ** ) : t -> t -> t

  val ( = ) : t -> t -> bool [@@zero_alloc]
  val ( <> ) : t -> t -> bool [@@zero_alloc]
  val ( < ) : t -> t -> bool [@@zero_alloc]
  val ( > ) : t -> t -> bool [@@zero_alloc]
  val ( <= ) : t -> t -> bool [@@zero_alloc]
  val ( >= ) : t -> t -> bool [@@zero_alloc]

  (** Same as [bit_and/or/xor/not] *)
  val ( land ) : t -> t -> t [@@zero_alloc]

  val ( lor ) : t -> t -> t [@@zero_alloc]
  val ( lxor ) : t -> t -> t [@@zero_alloc]
  val lnot : t -> t [@@zero_alloc]

  (** Returns the absolute value of the argument. May be negative if the input is
      [min_value]. *)
  val abs : t -> t
  [@@zero_alloc]

  (** Negation *)
  val neg : t -> t [@@zero_alloc]

  val zero : t

  (** There are two pairs of integer division and remainder functions, [/%] and [%], and
      [/] and [rem]. They both satisfy the same equation relating the quotient and the
      remainder:

      {[
        x = (x /% y * y) + (x % y);
        x = (x / y * y) + rem x y
      ]}

      The functions return the same values if [x] and [y] are positive. They all raise if
      [y = 0].

      The functions differ if [x < 0] or [y < 0].

      If [y < 0], then [%] and [/%] raise, whereas [/] and [rem] do not.

      [x % y] always returns a value between 0 and [y - 1], even when [x < 0]. On the
      other hand, [rem x y] returns a negative value if and only if [x < 0]; that value
      satisfies [abs (rem x y) <= abs y - 1]. *)

  val ( % ) : t -> t -> t [@@zero_alloc]
  val ( /% ) : t -> t -> t [@@zero_alloc]

  (** Float division of integers. *)
  val ( // ) : t -> t -> float# [@@zero_alloc]

  (** Same as [shift_left] *)
  val ( lsl ) : t -> int -> t [@@zero_alloc]

  (** Same as [shift_right]. *)
  val ( asr ) : t -> int -> t [@@zero_alloc]

  (** Same as [shift_right_logical]. *)
  val ( lsr ) : t -> int -> t [@@zero_alloc]

  external box : int64# -> (int64[@local_opt]) = "%box_int64"
  external unbox : (int64[@local_opt]) -> int64# = "%unbox_int64"
end

include module type of O (** @inline *)

(** {2 Conversion functions} *)

val of_int : int -> t [@@zero_alloc]
val of_int32 : int32 -> t [@@zero_alloc]
val to_int : t -> int option
val to_int32 : t -> int32 option
val of_nativeint : nativeint -> t [@@zero_alloc]
val to_nativeint : t -> nativeint option
val of_bool : bool -> t [@@zero_alloc]

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val to_int_trunc : t -> int [@@zero_alloc]
val to_int32_trunc : t -> int32
val to_nativeint_trunc : t -> nativeint

(** {3 Low-level float conversions} *)

val bits_of_float : float# -> t [@@zero_alloc]
val float_of_bits : t -> float# [@@zero_alloc]

(** {2 Byte- and bit-swap operations}

    See {{!modtype:Int.Int_without_module_types} [Int]'s byte swap section} for a
    description of Base's approach to exposing byte swap primitives.

    As of writing, these operations do not sign extend unnecessarily on 64 bit machines,
    unlike their int32 counterparts, and hence, are more performant. See the {!Int32}
    module for more details of the overhead entailed by the int32 byteswap functions. *)

val bswap16 : t -> t [@@zero_alloc]
val bswap32 : t -> t [@@zero_alloc]
val bswap48 : t -> t [@@zero_alloc]
val bswap64 : t -> t [@@zero_alloc]

(** Reverses the bits. *)
val rev_bits : t -> t [@@zero_alloc]

(** Branchless, as [Bool.select]. *)
val select : bool -> t -> t -> t [@@zero_alloc]

(** {1 Indexing into an array} *)

module Array_index : sig
  external get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    = "%array_safe_get_indexed_by_int64#"
  [@@layout_poly]

  external set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    = "%array_safe_set_indexed_by_int64#"
  [@@layout_poly]

  external unsafe_get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    = "%array_unsafe_get_indexed_by_int64#"
  [@@layout_poly]

  external unsafe_set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    = "%array_unsafe_set_indexed_by_int64#"
  [@@layout_poly]
end

module Iarray_index : sig
  external get
    : ('a : any mod separable).
    ('a iarray[@local_opt]) -> (t[@local_opt]) -> 'a
    = "%array_safe_get_indexed_by_int64#"
  [@@layout_poly]

  external unsafe_get
    : ('a : any mod separable).
    ('a iarray[@local_opt]) -> (t[@local_opt]) -> 'a
    = "%array_unsafe_get_indexed_by_int64#"
  [@@layout_poly]
end

(** {1 An array of [_ : bits64]} *)

module Array : sig
  type ('a : bits64) t = 'a array [@@deriving sexp_of]

  val equal : ('a -> 'a -> bool) -> local_ 'a t -> local_ 'a t -> bool
  external length : ('a t[@local_opt]) -> int = "%array_length"
  external get : ('a t[@local_opt]) -> int -> 'a = "%array_safe_get"
  external set : ('a t[@local_opt]) -> int -> 'a -> unit = "%array_safe_set"
  external unsafe_get : ('a t[@local_opt]) -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : ('a t[@local_opt]) -> int -> 'a -> unit = "%array_unsafe_set"
  val map : 'a t -> f:local_ ('a -> 'b) -> 'b t
  val init : int -> f:local_ (int -> 'a) -> 'a t

  (** The contents of the created array are unspecified. *)
  external unsafe_create_uninitialized
    :  len:int
    -> ('a : bits64) t
    = "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"

  external create_uninitialized
    :  len:int
    -> int64# t
    = "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"

  external unsafe_blit
    :  src:('a t[@local_opt])
    -> src_pos:int
    -> dst:('a t[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    = "caml_array_blit" "caml_unboxed_int64_vect_blit"

  val copy : 'a t @ local -> 'a t
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving stable_witness, globalize]

    (** We derive [sexp], [bin_io], [hash], [typerep], [string], [equal], and [compare]. *)

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t

    include%template Bin_prot.Binable.S [@mode local] with type t := t

    include Ppx_hash_lib.Hashable.S with type t := t

    val typerep_of_t : t Typerep_lib.Std.Typerep.t
    val of_string : string -> t
    val to_string : t -> string
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end
end

module Ref : sig
  type i64 := int64#
  type ('a : bits64) t = { mutable contents : 'a }

  val create : 'a -> 'a t
  val create_local : 'a -> local_ 'a t [@@zero_alloc]
  val get : local_ 'a t -> 'a [@@zero_alloc]
  val set : local_ 'a t -> 'a -> unit [@@zero_alloc]
  val add : local_ i64 t -> i64 -> unit [@@zero_alloc]
  val incr : local_ i64 t -> unit [@@zero_alloc]
  val decr : local_ i64 t -> unit [@@zero_alloc]

  module O : sig
    val ref : 'a -> local_ 'a t
    val ( ! ) : local_ 'a t -> 'a [@@zero_alloc]
    val ( := ) : local_ 'a t -> 'a -> unit [@@zero_alloc]
    val ( += ) : local_ i64 t -> i64 -> unit [@@zero_alloc]
    val ( |= ) : local_ i64 t -> i64 -> unit [@@zero_alloc]
    val incr : local_ i64 t -> unit [@@zero_alloc]
    val decr : local_ i64 t -> unit [@@zero_alloc]
  end
end

(** Prints [-#0x1L] as [0xffffffffffffffff], not [-0x1]. *)
module Hex_unsigned : sig
  module To_string_config : sig
    module Case : sig
      type t =
        | Lowercase
        | Uppercase
      [@@deriving equal ~localize, sexp_of]
    end

    (** We define both [t] and [t#] so we can provide [default : t], and [unbox] it for
        use as [~config] later. We can unwind this and provide only the unboxed type once
        unboxed values can be exported as top-level values in modules. *)
    type t =
      { prefix_with_0x : bool
      ; render_letters : Case.t
      }
    [@@deriving box, equal ~localize, sexp_of]

    val default : t
  end

  type nonrec t = t

  include Ppx_hash_lib.Hashable.S with type t := t

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t @ local -> t [@@zero_alloc]
  val to_string : t -> string
  val to_string_custom : t -> config:To_string_config.t# -> string
  val of_string : string @ local -> t [@@zero_alloc]

  module Local : sig
    type nonrec t = t

    include Ppx_hash_lib.Hashable.S with type t := t

    val compare : t -> t -> int
    val sexp_of_t : t -> Sexp.t @ local [@@zero_alloc]
    val t_of_sexp : Sexp.t @ local -> t [@@zero_alloc]

    val to_string_custom : t -> config:To_string_config.t# -> string @ local
    [@@zero_alloc]

    val to_string : t -> string @ local [@@zero_alloc]
    val of_string : string @ local -> t [@@zero_alloc]
  end

  module Private : sig
    type digits_to_process : bits64

    (** Precondition: [max_digits] is strictly positive. *)
    val digits_to_process : t -> max_digits:t -> digits_to_process

    val of_string : string @ local -> max_digits:t -> t

    val to_string_required_length
      :  config:To_string_config.t#
      -> digits_to_process:digits_to_process
      -> int

    val to_string_into
      :  t
      -> bytes @ local
      -> config:To_string_config.t#
      -> digits_to_process:digits_to_process
      -> unit

    val t_of_sexp_failed : Sexp.t @ local -> Nothing.t
  end
end

(** This allows us to represent [t option] without allocating for [Some _] tags.
    [Some min_value] is not representable because its representation would overlap with
    the representation of [None]. *)
module Option : sig
  type value = t
  type t : bits64 mod everything

  val none : t
  val some : value -> t [@@zero_alloc]
  val unchecked_some : value -> t [@@zero_alloc]
  val some_is_representable : value -> bool [@@zero_alloc]
  val is_none : t -> bool [@@zero_alloc]
  val is_some : t -> bool [@@zero_alloc]
  val value : t -> default:value -> value [@@zero_alloc]
  val unchecked_value : t -> value [@@zero_alloc]
  val to_option : t -> Boxed.t option
  val of_option : Boxed.t option -> t [@@zero_alloc]
  val to_local_option : t -> Boxed.t option @ local [@@zero_alloc]
  val of_local_option : Boxed.t option @ local -> t [@@zero_alloc]

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool [@@zero_alloc]
      val unsafe_value : t -> value [@@zero_alloc]
    end
  end
end
