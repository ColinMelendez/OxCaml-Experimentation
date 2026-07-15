@@ portable

open! Core

(** Utilities for unboxed float32s. This module is mostly a copy of Float32, but with much
    functionality missing because it can't yet be implemented for unboxed float32s or
    unboxed types generally. *)
type t = float32# [@@deriving globalize, quickcheck]

module Boxed = Float32

(** These definitions are available. They're included from [O] below.
    {[
      external box : float32# -> (float32[@local_opt]) = "%box_float32"
      external unbox : (float32[@local_opt]) -> float32# = "%unbox_float32"
    ]} *)

(** Synonyms for [box] and [unbox]. *)

external of_float32 : (float32[@local_opt]) -> t = "%unbox_float32"
external to_float32 : t -> (float32[@local_opt]) = "%box_float32"

(** [max] and [min] will return nan if either argument is nan.

    The [validate_*] functions always fail if class is [Nan] or [Infinite]. *)

(** {2 Inlined from [Identifiable], which comprises [Sexpable], [Stringable],
    [Comparable], and [Pretty_printer]} *)

(** {3 Inlined from [Sexpable]} *)

val%template sexp_of_t : t @ m -> Sexp.t @ m
[@@alloc a @ m = (heap @ global, stack @ local)]

val t_of_sexp : Sexp.t -> t

(** {3 For [bin_io]} *)

include%template Bin_prot.Binable.S [@mode local] with type t := t

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

(** {2 Inlined from [Invariant]} *)

val invariant : t -> unit

(** {2 Constants} *)

val nan : t
val infinity : t
val neg_infinity : t

(** Equal to [infinity]. *)
val max_value : t

(** Equal to [neg_infinity]. *)
val min_value : t

val zero : t
val one : t
val minus_one : t

(** The constant pi. *)
val pi : t

(** The constant sqrt(pi). *)
val sqrt_pi : t

(** The constant sqrt(2 * pi). *)
val sqrt_2pi : t

(** Euler-Mascheroni constant (γ). *)
val euler_gamma_constant : t

(** The difference between 1.0s and the smallest exactly representable float32 number
    greater than 1.0s. That is:

    [epsilon_float = (one_ulp `Up 1.0s) - 1.0s]

    This gives the relative accuracy of type [t], in the sense that for numbers on the
    order of [x], the roundoff error is on the order of [x * float_epsilon].

    See also: {{:http://en.wikipedia.org/wiki/Machine_epsilon} Machine epsilon}. *)
val epsilon_float : t

val max_finite_value : t

(** - [min_positive_subnormal_value = 2 ** -149]
    - [min_positive_normal_value    = 2 ** -126] *)

val min_positive_subnormal_value : t
val min_positive_normal_value : t

(** {2 Rounding and integer conversion} *)

(** An order-preserving bijection between all float32s except for nans, and all int32s
    with absolute value smaller than or equal to [2**31 - 2**23]. Note both 0.s and -0.s
    map to 0L. *)
val to_int32_preserve_order : t -> int32 option

val to_int32_preserve_order_exn : t -> int32

(** Returns [nan] if the absolute value of the argument is too large. *)
val of_int32_preserve_order : int32 -> t

(** The next or previous representable float32. ULP stands for "unit of least precision",
    and is the spacing between floating point numbers. Both [one_ulp `Up infinity] and
    [one_ulp `Down neg_infinity] return a nan. *)
val one_ulp : [ `Up | `Down ] -> t -> t

(** Converts a 64-bit float to the nearest representable float32. *)
val of_float : local_ float -> t

(** Converts a float32 to a 64-bit float. *)
val to_float : t -> float

(** Converts a 64-bit unboxed float to the nearest representable float32. *)
val of_float_u : float# -> t

(** Converts a float32 to a 64-bit float#. *)
val to_float_u : t -> float#

(** Convert an integer to a float32. Note that this doesn't round trip in either
    direction. For example, [Float32.to_int (Float32.of_int max_int) <> max_int]. *)
val of_int : int -> t

(** Truncate the given float32 to an integer. The result is unspecified if the argument is
    [nan] or falls outside the range of representable integers. *)
val to_int : t -> int

(** Converts the given int64 to the nearest representable float32. The amd64
    flambda-backend compiler translates this call to CVTSI2SS. *)
val of_int64 : local_ int64 -> t

(** Truncate the given float32 number to an int64. The result is unspecified if the
    argument is [nan] or falls outside the range of representable int64s. The amd64
    flambda-backend compiler translates this call to CVTTSS2SI. *)
val to_int64 : t -> int64

(** Converts the given int64# to the nearest representable float32. The amd64
    flambda-backend compiler translates this call to CVTSI2SS. *)
val of_int64_u : int64# -> t

(** Truncate the given float32 number to an int64#. The result is unspecified if the
    argument is [nan] or falls outside the range of representable int64s. The amd64
    flambda-backend compiler translates this call to CVTTSS2SI. *)
val to_int64_u : t -> int64#

(** Converts an int32 to a float32 with the same bit pattern. The amd64 flambda-backend
    compiler translates this call to MOVD. *)
val of_bits : int32# -> t

(** Converts a float32 to an int32 with the same bit pattern. The amd64 flambda-backend
    compiler translates this call to MOVD. *)
val to_bits : t -> int32#

(** [round] rounds a float32 to an integer float32. [iround{,_exn}] rounds a float32 to an
    int. Both round according to a direction [dir], with default [dir] being [`Nearest].

    {v
      | `Down    | rounds toward Float32.neg_infinity                           |
      | `Up      | rounds toward Float32.infinity                               |
      | `Nearest | rounds to the nearest int ("round half-integers up")         |
      | `Zero    | rounds toward zero                                           |
    v}

    [iround_exn] raises when trying to handle nan or trying to handle a float32 outside
    the range \[float32 min_int, float32 max_int).

    Here are some examples for [round] for each direction:

    {v
      | `Down    | [-2.s,-1s.)   to -2.s | [-1.s,0.s)   to -1.s | [0.s,1.s) to 0.s, [1.s,2.s) to 1.s |
      | `Up      | (-2.s,-1s.]   to -1.s | (-1.s,0.s]   to -0.s | (0.s,1.s] to 1.s, (1.s,2.s] to 2.s |
      | `Zero    | (-2.s,-1s.]   to -1.s | (-1.s,1.s)   to 0.s  | [1.s,2.s) to 1.s                   |
      | `Nearest | [-1.5s,-0.5s) to -1.s | [-0.5s,0.5s) to 0.s  | [0.5s,1.5s) to 1.s                 |
    v}

    For convenience, versions of these functions with the [dir] argument hard-coded are
    provided. If you are writing performance-critical code you should use the versions
    with the hard-coded arguments (e.g. [iround_down_exn]). The [_exn] ones are the
    fastest.

    The following properties hold:

    - [of_int (iround_*_exn i) = i] for any float32 [i] that is an integer with
      [min_int <= i <= max_int].

    - [round_* i = i] for any float32 [i] that is an integer.

    - [iround_*_exn (of_int i) = i] for any int [i] with [-2**52 <= i <= 2**52]. *)
val round : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> t

val iround : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> int option
val iround_exn : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> int

(** Rounds a float32 to the next integer float32 toward zero. The amd64 flambda-backend
    compiler translates this call to ROUNDSS. *)
val round_towards_zero : t -> t

(** Rounds a float32 down to the next integer float32 toward negative infinity. The amd64
    flambda-backend compiler translates this call to ROUNDSS. *)
val round_down : t -> t

(** Rounds a float32 up to the next integer float32 toward positive infinity. The amd64
    flambda-backend compiler translates this call to ROUNDSS. *)
val round_up : t -> t

(** Rounds half integers up. *)
val round_nearest : t -> t

(** Rounds a float32 to an integer float32 using the current rounding mode. The default
    rounding mode is "round half to even", and we expect that no program will change the
    rounding mode. The amd64 flambda-backend compiler translates this call to ROUNDSS. *)
val round_nearest_half_to_even : t -> t

val iround_towards_zero : t -> int option
val iround_down : t -> int option
val iround_up : t -> int option
val iround_nearest : t -> int option
val iround_towards_zero_exn : t -> int
val iround_down_exn : t -> int
val iround_up_exn : t -> int
val iround_nearest_exn : t -> int

(** Rounds a float32 to an int64 using the current rounding mode. The default rounding
    mode is "round half to even", and we expect that no program will change the rounding
    mode. If the argument is NaN, infinite, or otherwise cannot be represented, no
    exception is raised and the result is an unspecified int64. The amd64 flambda-backend
    compiler translates this call to CVTSS2SI. *)
val iround_nearest_half_to_even : local_ t -> int64

val iround_lbound : t
val iround_ubound : t

(** {2 Tests} *)

val is_nan : t -> bool [@@zero_alloc]

(** A float32 is infinite when it is either [infinity] or [neg_infinity]. *)
val is_inf : t -> bool

(** A float32 is finite when neither [is_nan] nor [is_inf] is true. *)
val is_finite : t -> bool

(** [is_integer x] is [true] if and only if [x] is an integer. *)
val is_integer : t -> bool

(** {2 Arithmetic} *)

(** [min_inan] and [max_inan] return, respectively, the min and max of the two given
    values, except when one of the values is a [nan], in which case the other is returned.
    (Returns [nan] if both arguments are [nan].) *)

val min_inan : t -> t -> t
val max_inan : t -> t -> t

(** [mod_float x y] returns a result with the same sign as [x]. It returns [nan] if [y] is
    [0]. It is basically

    {[
      let mod_float x y = x -. (float32 (truncate (x /. y)) *. y)
    ]}

    not

    {[
      let mod_float x y = x -. (floor (x /. y) *. y)
    ]}

    and therefore resembles [mod] on integers more than [%]. *)
val mod_float : t -> t -> t

(** {6 Ordinary functions for arithmetic operations}

    These are for modules that inherit from [t], since the infix operators are more
    convenient. *)
val add : t -> t -> t

val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t

(** A sub-module designed to be opened to make working with float32s more convenient. *)
module O : sig
  external box : float32# -> (float32[@local_opt]) = "%box_float32"
  external unbox : (float32[@local_opt]) -> float32# = "%unbox_float32"
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t

  (** In analogy to Int.( % ), ( % ):
      - always produces non-negative (or NaN) result
      - raises when given a negative modulus.

      Like the other infix operators, NaNs in mean NaNs out.

      Other cases: (a % Infinity) = a when 0 <= a < Infinity, (a % Infinity) = Infinity
      when -Infinity < a < 0, (+/- Infinity % a) = NaN, (a % 0) = NaN. *)
  val ( % ) : t -> t -> t

  val ( ** ) : t -> t -> t
  val ( ~- ) : t -> t

  (* Comparisons.Infix *)
  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val abs : t -> t
  val neg : t -> t
end

include module type of O

(** Similar to [O], except that operators are suffixed with a dot, allowing one to have
    both int and float32 operators in scope simultaneously. *)
module O_dot : sig
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
  val ( %. ) : t -> t -> t
  val ( **. ) : t -> t -> t
  val ( ~-. ) : t -> t
end

(** {2 String conversions} *)

(** [to_string x] builds a string [s] representing the float32 [x] that guarantees the
    round trip, that is such that [Float32_u.equal x (Float32_u.of_string s)]. *)
val to_string : t -> string

(** [of_string] is inverse to [to_string]. *)
val of_string : string @ local -> t

(** Pretty print float32, for example [to_string_hum ~decimals:3 1234.1999s = "1_234.200"]
    [to_string_hum ~decimals:3 ~strip_zero:true 1234.1999s = "1_234.2" ]. No delimiters
    are inserted to the right of the decimal. *)
val to_string_hum
  :  ?delimiter:char (** defaults to ['_'] *)
  -> ?decimals:int (** defaults to [3] *)
  -> ?strip_zero:bool (** defaults to [false] *)
  -> ?explicit_plus:bool
       (** Forces a + in front of non-negative values. Defaults to [false] *)
  -> t
  -> string

(** Produce a lossy compact string representation of the float32. The float32 is scaled by
    an appropriate power of 1000 and rendered with one digit after the decimal point,
    except that the decimal point is written as '.', 'k', 'm', 'g', 't', or 'p' to
    indicate the scale factor. (However, if the digit after the "decimal" point is 0, it
    is suppressed.)

    The smallest scale factor that allows the number to be rendered with at most 3 digits
    to the left of the decimal is used. If the number is too large for this format (i.e.,
    the absolute value is at least 999.95e15), scientific notation is used instead. E.g.:

    - [to_padded_compact_string     (-0.01s) =  "-0  "]
    - [to_padded_compact_string       1.89s  =   "1.9"]
    - [to_padded_compact_string 999_949.99s  = "999k9"]
    - [to_padded_compact_string 999_950.s    =   "1m "]

    In the case where the digit after the "decimal", or the "decimal" itself is omitted,
    the numbers are padded on the right with spaces to ensure the last two columns of the
    string always correspond to the decimal and the digit afterward (except in the case of
    scientific notation, where the exponent is the right-most element in the string and
    could take up to four characters).

    - [to_padded_compact_string    1.s =    "1  "]
    - [to_padded_compact_string  1.e6s =    "1m "]
    - [to_padded_compact_string 1.e16s = "1.e+16"]
    - [to_padded_compact_string max_finite_value = "1.8e+308"]

    Numbers in the range -.05 < x < .05 are rendered as "0 " or "-0 ".

    Other cases:

    - [to_padded_compact_string nan          =  "nan  "]
    - [to_padded_compact_string infinity     =  "inf  "]
    - [to_padded_compact_string neg_infinity = "-inf  "]

    Exact ties are resolved to even in the decimal:

    - [to_padded_compact_string      3.25s =  "3.2"]
    - [to_padded_compact_string      3.75s =  "3.8"]
    - [to_padded_compact_string 33_250.s   = "33k2"]
    - [to_padded_compact_string 33_350.s   = "33k4"]

    [to_padded_compact_string] is defined in terms of [to_padded_compact_string_custom]
    below as
    {[
      let to_padded_compact_string t =
        to_padded_compact_string_custom
          t
          ?prefix:None
          ~kilo:"k"
          ~mega:"m"
          ~giga:"g"
          ~tera:"t"
          ~peta:"p"
          ()
      ;;
    ]} *)
val to_padded_compact_string : t -> string

(** Similar to [to_padded_compact_string] but allows the user to provide different
    abbreviations. This can be useful to display currency values, e.g. $1mm3, where
    prefix="$", mega="mm". *)
val to_padded_compact_string_custom
  :  t
  -> ?prefix:string
  -> kilo:string
  -> mega:string
  -> giga:string
  -> tera:string
  -> ?peta:string
  -> unit
  -> string

(** {2 Exponents and trigonometry} *)

(** [int_pow x n] computes [x ** float32 n] via repeated squaring. It is generally much
    faster than [**].

    Note that [int_pow x 0] always returns [1.s], even if [x = nan]. This coincides with
    [x ** 0.s] and is intentional.

    For [n >= 0] the result is identical to an n-fold product of [x] with itself under
    [*], with a certain placement of parentheses. For [n < 0] the result is identical to
    [int_pow (1.s / x) (-n)].

    The error will be on the order of [|n|] ulps, essentially the same as if you perturbed
    [x] by up to a ulp and then exponentiated exactly.

    Benchmarks show a factor of 5-10 speedup (relative to [**]) for exponents up to about
    1000 (approximately 10ns vs. 70ns). For larger exponents the advantage is smaller but
    persists into the trillions. For a recent or more detailed comparison, run the
    benchmarks.

    Depending on context, calling this function might or might not allocate 2 minor words.
    Even if called in a way that causes allocation, it still appears to be faster than
    [**]. *)
val int_pow : t -> int -> t

(** [square x] returns [x *. x]. *)
val square : t -> t

(** [ldexp x n] returns [x *. 2 ** n] *)
val ldexp : t -> int -> t

(** Base 10 logarithm. *)
val log10 : t -> t

(** Base 2 logarithm. *)
val log2 : t -> t

(** [expm1 x] computes [exp x - 1.0s], giving numerically-accurate results even if [x] is
    close to [0.0s]. *)
val expm1 : t -> t

(** [log1p x] computes [log(1.0s + x)] (natural logarithm), giving numerically-accurate
    results even if [x] is close to [0.0s]. *)
val log1p : t -> t

(** [copysign x y] returns a float32 whose absolute value is that of [x] and whose sign is
    that of [y]. If [x] is [nan], returns [nan]. If [y] is [nan], returns either [x] or
    [-. x], but it is not specified which. *)
val copysign : t -> t -> t

(** Cosine. Argument is in radians. *)
val cos : t -> t

(** Sine. Argument is in radians. *)
val sin : t -> t

(** Tangent. Argument is in radians. *)
val tan : t -> t

(** Arc cosine. The argument must fall within the range [[-1.0s, 1.0s]]. Result is in
    radians and is between [0.0s] and [pi]. *)
val acos : t -> t

(** Arc sine. The argument must fall within the range [[-1.0s, 1.0s]]. Result is in
    radians and is between [-pi/2] and [pi/2]. *)
val asin : t -> t

(** Arc tangent. Result is in radians and is between [-pi/2] and [pi/2]. *)
val atan : t -> t

(** [atan2 y x] returns the arc tangent of [y /. x]. The signs of [x] and [y] are used to
    determine the quadrant of the result. Result is in radians and is between [-pi] and
    [pi]. *)
val atan2 : t -> t -> t

(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length of the hypotenuse of
    a right-angled triangle with sides of length [x] and [y], or, equivalently, the
    distance of the point [(x,y)] to origin. *)
val hypot : t -> t -> t

(** Hyperbolic cosine. Argument is in radians. *)
val cosh : t -> t

(** Hyperbolic sine. Argument is in radians. *)
val sinh : t -> t

(** Hyperbolic tangent. Argument is in radians. *)
val tanh : t -> t

(** Hyperbolic arc cosine. The argument must fall within the range [[1.0s, inf]]. Result
    is in radians and is between [0.0s] and [inf]. *)
val acosh : t -> t

(** Hyperbolic arc sine. The argument and result range over the entire real line. Result
    is in radians. *)
val asinh : t -> t

(** Hyperbolic arc tangent. The argument must fall within the range [[-1.0s, 1.0s]].
    Result is in radians and ranges over the entire real line. *)
val atanh : t -> t

(** Square root. *)
val sqrt : t -> t

(** Cube root. *)
val cbrt : t -> t

(** Exponential. *)
val exp : t -> t

(** Natural logarithm. *)
val log : t -> t

(** {2 Classification and representation} *)

(** Excluding nan the floating-point "number line" looks like:
    {v
             t                Class.t    example
           ^ neg_infinity     Infinite   neg_infinity
           | neg normals      Normal     -3.14
           | neg subnormals   Subnormal  -.2. ** -127.
           | (-/+) zero       Zero       0.
           | pos subnormals   Subnormal  2. ** -127.
           | pos normals      Normal     3.14
           v infinity         Infinite   infinity
    v} *)
module Class = Base.Float.Class

val classify : t -> Class.t

(*_ Caution: If we remove this sig item, [sign] will still be present from
    [Comparable.With_zero]. *)

val sign : t -> Sign.t
[@@deprecated "[since 2016-01] Replace [sign] with [robust_sign] or [sign_exn]"]

(** The sign of a float32. Both [-0.s] and [0.s] map to [Zero]. Raises on nan. All other
    values map to [Neg] or [Pos]. *)
val sign_exn : t -> Sign.t

(** The sign of a float32, with support for NaN. Both [-0.s] and [0.s] map to [Zero]. All
    NaN values map to [Nan]. All other values map to [Neg] or [Pos]. *)
val sign_or_nan : t -> Sign_or_nan.t

(** These functions construct and destruct 32-bit floating point numbers based on their
    IEEE representation with a sign bit, an 8-bit non-negative (biased) exponent, and a
    23-bit non-negative mantissa (or significand). See
    {{:http://en.wikipedia.org/wiki/Double-precision_floating-point_format} Wikipedia} for
    details of the encoding.

    In particular, if 1 <= exponent <= 126, then:

    {[
      create_ieee_exn ~negative:false ~exponent ~mantissa
      = (2 ** (exponent - 127)) * (1 + ((2 ** -23) * mantissa))
    ]} *)

val create_ieee_exn : negative:bool -> exponent:int -> mantissa:int -> t
val ieee_negative : t -> bool
val ieee_exponent : t -> int
val ieee_mantissa : t -> int

(** Branchless, as [Bool.select]. *)
val select : bool -> t -> t -> t

(** Branchless. *)
val first_non_nan : t -> t -> t

module Ref : sig
  type elt := float32#
  type t = { mutable contents : float32# }

  val create : elt -> t
  val create_zero : unit -> t
  val get : t -> elt
  val set : t -> elt -> unit
  val add : t -> elt -> unit

  module O : sig
    val ( ! ) : t -> elt
    val ( := ) : t -> elt -> unit
  end
end

module Array : sig
  type ('a : float32) t = 'a array [@@deriving sexp]

  (*_ [equal*] and [compare*] are declared manually so they can be annotated with
      [[@@zero_alloc]]. *)

  [%%template:
  [@@@mode.default m = (global, local)]

  val equal : ('a @ m -> 'a @ m -> bool) -> ('a t @ m -> 'a t @ m -> bool) [@@zero_alloc]
  val compare : ('a @ m -> 'a @ m -> int) -> ('a t @ m -> 'a t @ m -> int) [@@zero_alloc]]

  val globalize : _ -> local_ 'a t -> 'a t
  external length : local_ 'a t -> int = "%array_length"
  external get : local_ 'a t -> int -> 'a = "%array_safe_get"
  external set : local_ 'a t -> int -> 'a -> unit = "%array_safe_set"
  external unsafe_get : local_ 'a t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : local_ 'a t -> int -> 'a -> unit = "%array_unsafe_set"

  (** The contents of the created array are unspecified. *)
  external unsafe_create_uninitialized
    :  len:int
    -> 'a t
    = "caml_make_unboxed_float32_vect_bytecode" "caml_make_unboxed_float32_vect"

  (** The contents of the created array are unspecified. *)
  external create_uninitialized
    :  len:int
    -> float32# t
    = "caml_make_unboxed_float32_vect_bytecode" "caml_make_unboxed_float32_vect"

  (** The contents of the created array are unspecified. *)
  external create_local_uninitialized
    :  len:int
    -> float32# t @ local
    = "caml_make_unboxed_float32_vect_bytecode" "caml_make_local_unboxed_float32_vect"

  external unsafe_blit
    :  src:local_ 'a t
    -> src_pos:int
    -> dst:local_ 'a t
    -> dst_pos:int
    -> len:int
    -> unit
    = "caml_array_blit" "caml_unboxed_float32_vect_blit"

  val init : int -> f:local_ (int -> 'a) -> 'a t
  val copy : 'a t @ local -> 'a t
end

module (Bigarray @@ nonportable) : sig
  open Stdlib.Bigarray

  module Array1 : sig
    (** [Array1.get a x], or alternatively [a.{x}], returns the element of [a] at index
        [x]. [x] must be greater or equal than [0] and strictly less than [Array1.dim a]
        if [a] has C layout. If [a] has Fortran layout, [x] must be greater or equal than
        [1] and less or equal than [Array1.dim a]. Otherwise, [Invalid_argument] is
            raised. *)
    val get : ('a, float32_elt, 'c) Array1.t -> int -> float32#

    (** [Array1.set a x v], also written [a.{x} <- v], stores the value [v] at index [x]
        in [a]. [x] must be inside the bounds of [a] as described in
        {!Bigarray.Array1.get}; otherwise, [Invalid_argument] is raised. *)
    val set : ('a, float32_elt, 'c) Array1.t -> int -> float32# -> unit

    (** Like {!Bigarray.Array1.get}, but bounds checking is not always performed. Use with
        caution and only when the program logic guarantees that the access is within
        bounds. *)
    val unsafe_get : ('a, float32_elt, 'c) Array1.t -> int -> float32#

    (** Like {!Bigarray.Array1.set}, but bounds checking is not always performed. Use with
        caution and only when the program logic guarantees that the access is within
        bounds. *)
    val unsafe_set : ('a, float32_elt, 'c) Array1.t -> int -> float32# -> unit
  end

  module Array2 : sig
    (** [Array2.get a x y], also written [a.{x,y}], returns the element of [a] at
        coordinates ([x], [y]). [x] and [y] must be within the bounds of [a], as described
        for {!Bigarray.Genarray.get}; otherwise, [Invalid_argument] is raised. *)
    val get : ('a, float32_elt, 'c) Array2.t -> int -> int -> float32#

    (** [Array2.set a x y v], or alternatively [a.{x,y} <- v], stores the value [v] at
        coordinates ([x], [y]) in [a]. [x] and [y] must be within the bounds of [a], as
        described for {!Bigarray.Genarray.set}; otherwise, [Invalid_argument] is raised. *)
    val set : ('a, float32_elt, 'c) Array2.t -> int -> int -> float32# -> unit

    (** Like {!Bigarray.Array2.get}, but bounds checking is not always performed. *)
    val unsafe_get : ('a, float32_elt, 'c) Array2.t -> int -> int -> float32#

    (** Like {!Bigarray.Array2.set}, but bounds checking is not always performed. *)
    val unsafe_set : ('a, float32_elt, 'c) Array2.t -> int -> int -> float32# -> unit
  end

  module Array3 : sig
    (** [Array3.get a x y z], also written [a.{x,y,z}], returns the element of [a] at
        coordinates ([x], [y], [z]). [x], [y] and [z] must be within the bounds of [a], as
        described for {!Bigarray.Genarray.get}; otherwise, [Invalid_argument] is raised. *)
    val get : ('a, float32_elt, 'c) Array3.t -> int -> int -> int -> float32#

    (** [Array3.set a x y v], or alternatively [a.{x,y,z} <- v], stores the value [v] at
        coordinates ([x], [y], [z]) in [a]. [x], [y] and [z] must be within the bounds of
        [a], as described for {!Bigarray.Genarray.set}; otherwise, [Invalid_argument] is
        raised. *)
    val set : ('a, float32_elt, 'c) Array3.t -> int -> int -> int -> float32# -> unit

    (** Like {!Bigarray.Array3.get}, but bounds checking is not always performed. *)
    val unsafe_get : ('a, float32_elt, 'c) Array3.t -> int -> int -> int -> float32#

    (** Like {!Bigarray.Array3.set}, but bounds checking is not always performed. *)
    val unsafe_set
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32#
      -> unit
  end
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving globalize, sexp, stable_witness, string]

    (** We derive [bin_io], [equal], and [compare]. *)

    include Bin_prot.Binable.S [@mode local] with type t := t

    val equal : t -> t -> bool
    val compare : t -> t -> int
  end
end
