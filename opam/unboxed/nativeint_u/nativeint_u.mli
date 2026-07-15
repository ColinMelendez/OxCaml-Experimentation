@@ portable

open! Base

(** Utilities for unboxed nativeints. This module is mostly a copy of Base's Nativeint
    module, but with much functionality missing because it can't yet be implemented for
    unboxed nativeints or unboxed types generally.

    It's part of the same family of libraries as `lib/float_u`, `lib/int32_u`, and
    `lib/int64_u`. They share similar project structures, conventions, and tests. *)
type t = nativeint# [@@deriving globalize, quickcheck]

module Boxed = Core.Nativeint

(** These definitions are available. They're included from [O] below.
    {[
      external box : nativeint# -> (nativeint[@local_opt]) = "%box_nativeint"
      external unbox : (nativeint[@local_opt]) -> nativeint# = "%unbox_nativeint"
    ]} *)

(** Synonyms for [box] and [unbox]. *)

external of_nativeint : (nativeint[@local_opt]) -> t = "%unbox_nativeint"
external to_nativeint : t -> (nativeint[@local_opt]) = "%box_nativeint"

(** {1 [Int_intf.S] inlined} *)

(** {2 Inlined from [Floatable]} *)

val of_float : float -> t
val to_float : t -> float

(** {2 Inlined from [Intable]} *)

val of_int_exn : int -> t
val to_int_exn : t -> int [@@zero_alloc]

(** {2 Inlined from [Identifiable]} *)

(** {3 Inlined from [Sexpable]} *)

val%template sexp_of_t : t @ m -> Sexp.t @ m
[@@alloc a @ m = (heap @ global, stack @ local)]

val t_of_sexp : Sexp.t -> t

(** {3 For [bin_io]} *)

include%template Bin_prot.Binable.S [@mode local] with type t := t

(** {3 For [hash]} *)

include Ppx_hash_lib.Hashable.S with type t := t

(** {3 From [Typerep]} *)

val typerep_of_t : t Typerep_lib.Std.Typerep.t

(** {3 Inlined from [Stringable]} *)

val of_string : string @ local -> t

val%template to_string : t -> string @ l [@@alloc a @ l = (heap @ global, stack @ local)]

(** {3 Inlined from [Comparable]} *)

[%%template:
[@@@mode.default m = (global, local)]

val equal : t @ m -> t @ m -> bool [@@zero_alloc]

(** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is less
    than [t2], and a positive integer if [t1] is greater than [t2]. *)
val compare : t @ m -> t @ m -> int
[@@zero_alloc]]

val min : t -> t -> t
val max : t -> t -> t

(** [ascending] is identical to [compare]. [descending x y = ascending y x]. These are
    intended to be mnemonic when used like [List.sort ~compare:ascending] and
    [List.sort ~cmp:descending], since they cause the list to be sorted in ascending or
    descending order, respectively. *)
val ascending : t -> t -> int

val descending : t -> t -> int

(** [between t ~low ~high] means [low <= t <= high] *)
val between : t -> low:t -> high:t -> bool

(** [clamp_exn t ~min ~max] returns [t'], the closest value to [t] such that
    [between t' ~low:min ~high:max] is true.

    Raises if [not (min <= max)]. *)
val clamp_exn : t -> min:t -> max:t -> t

(** {3 Inlined from [Pretty_printer]} *)

val pp : Formatter.t -> t -> unit

(** {3 Inlined from [Comparable.With_zero]} *)

val is_positive : t -> bool
val is_non_negative : t -> bool
val is_negative : t -> bool
val is_non_positive : t -> bool

(** Returns [Neg], [Zero], or [Pos] in a way consistent with the above functions. *)
val sign : t -> Sign.t

(** {3 Inlined from [Invariant.S]} *)
val invariant : t -> unit

module Hex_unsigned : sig
  type nonrec t = t

  include Ppx_hash_lib.Hashable.S with type t := t

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t @ local -> t [@@zero_alloc]
  val to_string : t -> string
  val of_string : string @ local -> t [@@zero_alloc]

  module Local : sig
    type nonrec t = t

    include Ppx_hash_lib.Hashable.S with type t := t

    val compare : t -> t -> int
    val sexp_of_t : t -> Sexp.t @ local [@@zero_alloc]
    val t_of_sexp : Sexp.t @ local -> t [@@zero_alloc]
    val to_string : t -> string @ local [@@zero_alloc]
    val of_string : string @ local -> t [@@zero_alloc]
  end
end

(** [delimiter] is an underscore by default. *)
val to_string_hum : ?delimiter:char -> t -> string

(** {2 Infix operators and constants} *)

val one : t
val minus_one : t

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
val round_down : t -> to_multiple_of:t -> t
val round_up : t -> to_multiple_of:t -> t
val round_nearest : t -> to_multiple_of:t -> t

(** {2 Successor and predecessor functions} *)

val succ : t -> t
val pred : t -> t

(** {2 Exponentiation} *)

(** [pow base exponent] returns [base] raised to the power of [exponent]. It is OK if
    [base <= 0]. [pow] raises if [exponent < 0], or an integer overflow would occur. *)
val pow : t -> t -> t

(** {2 Bit-wise logical operations} *)

(** These are identical to [land], [lor], etc. except they're not infix and have different
    names. *)
val bit_and : t -> t -> t

val bit_or : t -> t -> t
val bit_xor : t -> t -> t
val bit_not : t -> t

(** Returns the number of 1 bits in the binary representation of the input. *)
val popcount : t -> t

(** {2 Bit-shifting operations}

    The results are unspecified for negative shifts and shifts [>= num_bits]. *)

(** Shifts left, filling in with zeroes. *)
val shift_left : t -> int -> t

(** Shifts right, preserving the sign of the input. *)
val shift_right : t -> int -> t

(** {2 Increment and decrement functions for integer references} *)

(** {2 Conversion functions to related integer types} *)

val of_int32_exn : int32 -> t
val to_int32_exn : t -> int32
val of_int64_exn : int64 -> t
val to_int64 : t -> int64
val of_nativeint_exn : nativeint -> t
val to_nativeint_exn : t -> nativeint

(** [of_float_unchecked] truncates the given floating point number to an integer, rounding
    towards zero. The result is unspecified if the argument is nan or falls outside the
    range of representable integers. *)
val of_float_unchecked : float -> t

(** The number of bits available in this integer type. Note that the integer
    representations are signed. *)
val num_bits : nativeint

(** The largest representable integer. *)
val max_value : t

(** The smallest representable integer. *)
val min_value : t

(** Shifts right, filling in with zeroes, which will not preserve the sign of the input. *)
val shift_right_logical : t -> int -> t

(** [ceil_pow2 x] returns the smallest power of 2 that is greater than or equal to [x].
    The implementation may only be called for [x > 0]. Example: [ceil_pow2 17 = 32] *)
val ceil_pow2 : t -> t

(** [floor_pow2 x] returns the largest power of 2 that is less than or equal to [x]. The
    implementation may only be called for [x > 0]. Example: [floor_pow2 17 = 16] *)
val floor_pow2 : t -> t

(** [ceil_log2 x] returns the ceiling of log-base-2 of [x], and raises if [x <= 0]. *)
val ceil_log2 : t -> t

(** [floor_log2 x] returns the floor of log-base-2 of [x], and raises if [x <= 0]. *)
val floor_log2 : t -> t

(** [is_pow2 x] returns true iff [x] is a power of 2. [is_pow2] raises if [x <= 0]. *)
val is_pow2 : t -> bool

(** Returns the number of leading zeros in the binary representation of the input, as an
    integer between 0 and one less than [num_bits].

    The results are unspecified for [t = 0]. *)
val clz : t -> t

(** Returns the number of trailing zeros in the binary representation of the input, as an
    integer between 0 and one less than [num_bits].

    The results are unspecified for [t = 0]. *)
val ctz : t -> t

(** A sub-module designed to be opened to make working with ints more convenient. *)

module O : sig
  external box : nativeint# -> (nativeint[@local_opt]) = "%box_nativeint"
  external unbox : (nativeint[@local_opt]) -> nativeint# = "%unbox_nativeint"
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( ** ) : t -> t -> t

  (** {2 Inlined from [Comparisons.Infix]} *)

  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <> ) : t -> t -> bool

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

  val ( /% ) : t -> t -> t
  val ( % ) : t -> t -> t
  val ( / ) : t -> t -> t
  val rem : t -> t -> t

  (** Float division of integers. *)
  val ( // ) : t -> t -> float

  (** Same as [bit_and]. *)
  val ( land ) : t -> t -> t

  (** Same as [bit_or]. *)
  val ( lor ) : t -> t -> t

  (** Same as [bit_xor]. *)
  val ( lxor ) : t -> t -> t

  (** Same as [bit_not]. *)
  val lnot : t -> t

  (** Same as [shift_left]. *)
  val ( lsl ) : t -> int -> t

  (** Same as [shift_right]. *)
  val ( asr ) : t -> int -> t

  (** Same as [shift_right_logical]. *)
  val ( lsr ) : t -> int -> t

  (** Returns the absolute value of the argument. May be negative if the input is
      [min_value]. *)
  val abs : t -> t

  val neg : t -> t
  val zero : t
end

include module type of O

(** {2 Conversion functions} *)

val of_int : int -> t
val to_int : t -> int option
val of_int32 : int32 -> t
val to_int32 : t -> int32 option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val to_int_trunc : t -> int
val to_int32_trunc : t -> int32
val of_int64_trunc : int64 -> t

(** {2 Byte swap functions}

    See {{!modtype:Int.Int_without_module_types} [Int]'s byte swap section} for a
    description of Base's approach to exposing byte swap primitives. *)

val bswap : t -> t

(** Branchless, as [Bool.select]. *)
val select : bool -> t -> t -> t

(** {1 Indexing into an array} *)

module Array_index : sig
  external get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    = "%array_safe_get_indexed_by_nativeint#"
  [@@layout_poly]

  external set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    = "%array_safe_set_indexed_by_nativeint#"
  [@@layout_poly]

  external unsafe_get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    = "%array_unsafe_get_indexed_by_nativeint#"
  [@@layout_poly]

  external unsafe_set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    = "%array_unsafe_set_indexed_by_nativeint#"
  [@@layout_poly]
end

(** {1 An array of [_ : word]} *)

module Array : sig
  type ('a : word) t = 'a array [@@deriving sexp_of]

  external length : ('a t[@local_opt]) -> int = "%array_length"
  external get : ('a t[@local_opt]) -> int -> 'a = "%array_safe_get"
  external set : ('a t[@local_opt]) -> int -> 'a -> unit = "%array_safe_set"
  external unsafe_get : ('a t[@local_opt]) -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : ('a t[@local_opt]) -> int -> 'a -> unit = "%array_unsafe_set"

  (** The contents of the created array are unspecified. *)
  external create_uninitialized
    :  len:int
    -> nativeint# t
    = "caml_make_unboxed_nativeint_vect_bytecode" "caml_make_unboxed_nativeint_vect"

  external unsafe_blit
    :  src:('a t[@local_opt])
    -> src_pos:int
    -> dst:('a t[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    = "caml_array_blit" "caml_unboxed_nativeint_vect_blit"
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving stable_witness]

    (** We derive [sexp], [bin_io], [hash], [typerep], [string], [equal], and [compare]. *)

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t

    include Bin_prot.Binable.S with type t := t
    include Ppx_hash_lib.Hashable.S with type t := t

    val typerep_of_t : t Typerep_lib.Std.Typerep.t
    val of_string : string -> t
    val to_string : t -> string
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end
end
