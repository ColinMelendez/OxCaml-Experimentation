@@ portable

(** 32-bit floating-point representation and utilities. This is essentially a copy of the
    Base.Float API. *)

open! Base

type t = float32
[@@deriving_inline bin_io ~localize, globalize, sexp ~stackify, quickcheck]

include sig
  [@@@ocaml.warning "-32"]

  include Bin_prot.Binable.S__local with type t := t

  val globalize : local_ t -> t

  include Sexplib0.Sexpable.S__stack with type t := t
  include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
end
[@@ocaml.doc "@inline"]

[@@@end]

include Floatable.S with type t := t

(** [max] and [min] will return nan if either argument is nan.

    The [validate_*] functions always fail if class is [Nan] or [Infinite]. *)

include%template Identifiable.S [@mode local] with type t := t

val of_string : local_ string -> t
val of_string_opt : local_ string -> t option

module Util : sig
    type float32 = t
    [@@deriving
      bin_io ~localize, compare ~localize, equal ~localize, hash, sexp_of ~stackify]
  end
  with type float32 := t

include Comparable.With_zero with type t := t
include Invariant.S with type t := t

include%template Comparisons.S_with_local_opt [@mode local] with type t := t

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

(** The difference between 1.0s and the smallest exactly representable float32 greater
    than 1.0s. That is:

    [epsilon_float = (one_ulp `Up 1.0s) -. 1.0s]

    This gives the relative accuracy of type [t], in the sense that for numbers on the
    order of [x], the roundoff error is on the order of [x *. float_epsilon].

    See also: {{:http://en.wikipedia.org/wiki/Machine_epsilon} Machine epsilon}. *)
val epsilon_float : t

val max_finite_value : t

(** - [min_positive_subnormal_value = 2 ** -149]
    - [min_positive_normal_value    = 2 ** -126] *)

val min_positive_subnormal_value : t
val min_positive_normal_value : t

(** An order-preserving bijection between all float32s except for nans, and all int32s
    with absolute value smaller than or equal to [2**31 - 2**23]. Note both 0.s and -0.s
    map to 0l. *)
val to_int32_preserve_order : local_ t -> int32 option

val to_int32_preserve_order_exn : local_ t -> int32

(** Returns [nan] if the absolute value of the argument is too large. *)
val of_int32_preserve_order : local_ int32 -> t

(** The next or previous representable float32. ULP stands for "unit of least precision",
    and is the spacing between floating point numbers. Both [one_ulp `Up infinity] and
    [one_ulp `Down neg_infinity] return a nan. *)
val one_ulp : [ `Up | `Down ] -> local_ t -> t

(** Converts a 64-bit float to the nearest representable float32. *)
external of_float : local_ float -> (t[@local_opt]) = "%float32offloat"

(** Converts a float32 to a 64-bit float. *)
external to_float : local_ t -> (float[@local_opt]) = "%floatoffloat32"

(** Convert an integer to a float32. Note that this doesn't round trip in either
    direction. For example, [Float32.to_int (Float32.of_int max_int) <> max_int]. *)
external of_int : int -> (t[@local_opt]) = "%float32ofint"

(** Truncate the given float32 to an integer. The result is unspecified if the argument is
    [nan] or falls outside the range of representable integers. *)
val to_int : local_ t -> int

(** Converts the given int64 to the nearest representable float32. The amd64
    flambda-backend compiler translates this call to CVTSI2SS. *)
val of_int64 : local_ int64 -> t

(** Truncate the given float32 number to an int64. The result is unspecified if the
    argument is [nan] or falls outside the range of representable int64s. The amd64
    flambda-backend compiler translates this call to CVTTSS2SI. *)
val to_int64 : local_ t -> int64

(** Converts an int32 to a float32 with the same bit pattern. The amd64 flambda-backend
    compiler translates this call to MOVD. *)
val of_bits : local_ int32 -> t

(** Converts a float32 to an int32 with the same bit pattern. The amd64 flambda-backend
    compiler translates this call to MOVD. *)
val to_bits : local_ t -> int32

(** [round] rounds a float32 to an integer float32. [iround{,_exn}] rounds a float32 to an
    int. Both round according to a direction [dir], with default [dir] being [`Nearest].

    {v
      | `Down    | rounds toward Float32.neg_infinity                             |
      | `Up      | rounds toward Float32.infinity                                 |
      | `Nearest | rounds to the nearest int ("round half-integers up")         |
      | `Zero    | rounds toward zero                                           |
    v}

    [iround_exn] raises when trying to handle nan or trying to handle a float32 outside
    the range \[float32 min_int, float32 max_int).

    Here are some examples for [round] for each direction:

    {v
      | `Down    | [-2.s,-1.s)   to -2.s | [-1.s,0.s)   to -1.s | [0.s,1.s)   to 0.s, [1.s,2.s) to 1.s |
      | `Up      | (-2.s,-1.s]   to -1.s | (-1.s,0.s]   to -0.s | (0.s,1.s]   to 1.s, (1.s,2.s] to 2.s |
      | `Zero    | (-2.s,-1.s]   to -1.s | (-1.s,1.s)   to 0.s  | [1.s,2.s)   to 1.s                   |
      | `Nearest | [-1.5s,-0.5s) to -1.s | [-0.5s,0.5s) to 0.s  | [0.5s,1.5s) to 1.s                   |
    v}

    For convenience, versions of these functions with the [dir] argument hard-coded are
    provided. If you are writing performance-critical code you should use the versions
    with the hard-coded arguments (e.g. [iround_down_exn]). The [_exn] ones are the
    fastest.

    The following properties hold:

    - [of_int (iround_*_exn i) = i] for any float32 [i] that is an integer with
      [min_int <= i <= max_int].

    - [round_* i = i] for any float32 [i] that is an integer.

    - [iround_*_exn (of_int i) = i] for any int [i] with [-2**23 <= i <= 2**23]. *)
val round : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> local_ t -> t

val iround : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> local_ t -> int option
val iround_exn : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> local_ t -> int

(** Rounds a float32 to the next integer float32 toward zero. The amd64 flambda-backend
    compiler translates this call to ROUNDSS. *)
val round_towards_zero : local_ t -> t

(** Rounds a float32 down to the next integer float32 toward negative infinity. The amd64
    flambda-backend compiler translates this call to ROUNDSS. *)
val round_down : local_ t -> t

(** Rounds a float32 up to the next integer float32 toward positive infinity. The amd64
    flambda-backend compiler translates this call to ROUNDSS. *)
val round_up : local_ t -> t

(** Rounds half integers up. *)
val round_nearest : local_ t -> t

(** Rounds half integers to the even integer. *)
val round_nearest_half_to_even : local_ t -> t

val iround_towards_zero : local_ t -> int option
val iround_down : local_ t -> int option
val iround_up : local_ t -> int option
val iround_nearest : local_ t -> int option
val iround_towards_zero_exn : local_ t -> int
val iround_down_exn : local_ t -> int
val iround_up_exn : local_ t -> int
val iround_nearest_exn : local_ t -> int

(** If the argument is NaN, infinite, or otherwise cannot be represented, no exception is
    raised and the result is an unspecified int64. *)
val iround_nearest_half_to_even : local_ t -> int64

(** If [f < iround_lbound || f > iround_ubound], then [iround*] functions will refuse to
    round [f], returning [None] or raising as appropriate. *)
val iround_lbound : t

val iround_ubound : t

(** A float32 is nan when it is not a rational number or an infinity. *)
val is_nan : local_ t -> bool

(** A float32 is infinite when it is either [infinity] or [neg_infinity]. *)
val is_inf : local_ t -> bool

(** A float32 is finite when neither [is_nan] nor [is_inf] is true. *)
val is_finite : local_ t -> bool

(** [is_integer x] is [true] if and only if [x] is an integer. *)
val is_integer : local_ t -> bool

[%%template:
[@@@mode.default m = (global, local)]

(** [min_inan] and [max_inan] return, respectively, the min and max of the two given
    values, except when one of the values is a [nan], in which case the other is returned.
    (Returns [nan] if both arguments are [nan].) *)

val min_inan : t @ m -> t @ m -> t @ m
val max_inan : t @ m -> t @ m -> t @ m]

external ( + ) : local_ t -> local_ t -> (t[@local_opt]) = "%addfloat32"
external ( - ) : local_ t -> local_ t -> (t[@local_opt]) = "%subfloat32"
external ( * ) : local_ t -> local_ t -> (t[@local_opt]) = "%mulfloat32"
external ( / ) : local_ t -> local_ t -> (t[@local_opt]) = "%divfloat32"
external ( ~- ) : local_ t -> (t[@local_opt]) = "%negfloat32"

(** In analogy to Int.( % ), ( % ):
    - always produces non-negative (or NaN) result
    - raises when given a negative modulus.

    Like the other infix operators, NaNs in mean NaNs out.

    Other cases: (a % Infinity) = a when 0 <= a < Infinity, (a % Infinity) = Infinity when
    -Infinity < a < 0, (+/- Infinity % a) = NaN, (a % 0) = NaN. *)
val ( % ) : local_ t -> local_ t -> t

val ( ** ) : local_ t -> local_ t -> t

(** Returns the fractional part and the whole (i.e., integer) part. For example,
    [modf (-3.14s)] returns [{ fractional = -0.14s; integral = -3.s; }]! *)
module Parts : sig
    type outer
    type t

    val fractional : t -> outer
    val integral : t -> outer
  end
  with type outer := t

val modf : local_ t -> Parts.t

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
val mod_float : local_ t -> local_ t -> t

(** {6 Ordinary functions for arithmetic operations}

    These are for modules that inherit from [t], since the infix operators are more
    convenient. *)

external add : local_ t -> local_ t -> (t[@local_opt]) = "%addfloat32"
external sub : local_ t -> local_ t -> (t[@local_opt]) = "%subfloat32"
external mul : local_ t -> local_ t -> (t[@local_opt]) = "%mulfloat32"
external div : local_ t -> local_ t -> (t[@local_opt]) = "%divfloat32"
external neg : local_ t -> (t[@local_opt]) = "%negfloat32"
external abs : local_ t -> (t[@local_opt]) = "%absfloat32"
val pow : local_ t -> local_ t -> t

(** A sub-module designed to be opened to make working with float32s more convenient. *)
module O : sig
  external ( + ) : local_ t -> local_ t -> (t[@local_opt]) = "%addfloat32"
  external ( - ) : local_ t -> local_ t -> (t[@local_opt]) = "%subfloat32"
  external ( * ) : local_ t -> local_ t -> (t[@local_opt]) = "%mulfloat32"
  external ( / ) : local_ t -> local_ t -> (t[@local_opt]) = "%divfloat32"
  external ( ~- ) : local_ t -> (t[@local_opt]) = "%negfloat32"
  val ( % ) : local_ t -> local_ t -> t
  val ( ** ) : local_ t -> local_ t -> t

  include Comparisons.Infix_with_local_opt with type t := t

  external abs : local_ t -> (t[@local_opt]) = "%absfloat32"
  external neg : local_ t -> (t[@local_opt]) = "%negfloat32"
  val zero : t
  external of_int : int -> (t[@local_opt]) = "%float32ofint"
  external of_float : local_ float -> (t[@local_opt]) = "%float32offloat"
end

(** Similar to [O], except that operators are suffixed with a dot, allowing one to have
    both int and float32 operators in scope simultaneously. *)
module O_dot : sig
  external ( +. ) : local_ t -> local_ t -> (t[@local_opt]) = "%addfloat32"
  external ( -. ) : local_ t -> local_ t -> (t[@local_opt]) = "%subfloat32"
  external ( *. ) : local_ t -> local_ t -> (t[@local_opt]) = "%mulfloat32"
  external ( /. ) : local_ t -> local_ t -> (t[@local_opt]) = "%divfloat32"
  external ( ~-. ) : local_ t -> (t[@local_opt]) = "%negfloat32"
  val ( %. ) : local_ t -> local_ t -> t
  val ( **. ) : local_ t -> local_ t -> t
end

(** [to_string x] builds a string [s] representing the float32 [x] that guarantees the
    round trip, that is such that [Float32.equal x (Float32.of_string s)]. *)
val to_string : local_ t -> string

(** Pretty print float32, for example [to_string_hum ~decimals:3 1234.1999s = "1_234.200"]
    [to_string_hum ~decimals:3 ~strip_zero:true 1234.1999s = "1_234.2" ]. No delimiters
    are inserted to the right of the decimal. *)
val to_string_hum
  :  ?delimiter:char (** defaults to ['_'] *)
  -> ?decimals:int (** defaults to [3] *)
  -> ?strip_zero:bool (** defaults to [false] *)
  -> ?explicit_plus:bool
       (** Forces a + in front of non-negative values. Defaults to [false] *)
  -> local_ t
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
val to_padded_compact_string : local_ t -> string

(** Similar to [to_padded_compact_string] but allows the user to provide different
    abbreviations. This can be useful to display currency values, e.g. $1mm3, where
    prefix="$", mega="mm". *)
val to_padded_compact_string_custom
  :  local_ t
  -> ?prefix:string
  -> kilo:string
  -> mega:string
  -> giga:string
  -> tera:string
  -> ?peta:string
  -> unit
  -> string

(** [int_pow x n] computes [x ** float32 n] via repeated squaring. It is generally much
    faster than [**].

    Note that [int_pow x 0] always returns [1.], even if [x = nan]. This coincides with
    [x ** 0.] and is intentional.

    For [n >= 0] the result is identical to an n-fold product of [x] with itself under
    [*.], with a certain placement of parentheses. For [n < 0] the result is identical to
    [int_pow (1. /. x) (-n)].

    The error will be on the order of [|n|] ulps, essentially the same as if you perturbed
    [x] by up to a ulp and then exponentiated exactly.

    Benchmarks show a factor of 5-10 speedup (relative to [**]) for exponents up to about
    1000 (approximately 10ns vs. 70ns). For larger exponents the advantage is smaller but
    persists into the trillions. For a recent or more detailed comparison, run the
    benchmarks.

    Depending on context, calling this function might or might not allocate 2 minor words.
    Even if called in a way that causes allocation, it still appears to be faster than
    [**]. *)
val int_pow : local_ t -> int -> t

(** [square x] returns [x *. x]. *)
val%template square : t @ m -> t @ m
[@@mode m = (global, local)]

(** [ldexp x n] returns [x *. 2 ** n] *)
val ldexp : local_ t -> int -> t

(** [frexp f] returns the pair of the significant and the exponent of [f]. When [f] is
    zero, the significant [x] and the exponent [n] of [f] are equal to zero. When [f] is
    non-zero, they are defined by [f = x *. 2 ** n] and [0.5s <= x < 1.0s]. *)
val frexp : local_ t -> t * int

(** Base 10 logarithm. *)
val log10 : local_ t -> t

(** Base 2 logarithm. *)
val log2 : local_ t -> t

(** [expm1 x] computes [exp x -. 1.0s], giving numerically-accurate results even if [x] is
    close to [0.0s]. *)
val expm1 : local_ t -> t

(** [log1p x] computes [log(1.0s +. x)] (natural logarithm), giving numerically-accurate
    results even if [x] is close to [0.0s]. *)
val log1p : local_ t -> t

(** [copysign x y] returns a float whose absolute value is that of [x] and whose sign is
    that of [y]. If [x] is [nan], returns [nan]. If [y] is [nan], returns either [x] or
    [-. x], but it is not specified which. *)
val copysign : local_ t -> local_ t -> t

(** Cosine. Argument is in radians. *)
val cos : local_ t -> t

(** Sine. Argument is in radians. *)
val sin : local_ t -> t

(** Tangent. Argument is in radians. *)
val tan : local_ t -> t

(** Arc cosine. The argument must fall within the range [[-1.0s, 1.0s]]. Result is in
    radians and is between [0.0s] and [pi]. *)
val acos : local_ t -> t

(** Arc sine. The argument must fall within the range [[-1.0s, 1.0s]]. Result is in
    radians and is between [-pi/2] and [pi/2]. *)
val asin : local_ t -> t

(** Arc tangent. Result is in radians and is between [-pi/2] and [pi/2]. *)
val atan : local_ t -> t

(** [atan2 y x] returns the arc tangent of [y /. x]. The signs of [x] and [y] are used to
    determine the quadrant of the result. Result is in radians and is between [-pi] and
    [pi]. *)
val atan2 : local_ t -> local_ t -> t

(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length of the hypotenuse of
    a right-angled triangle with sides of length [x] and [y], or, equivalently, the
    distance of the point [(x,y)] to origin. *)
val hypot : local_ t -> local_ t -> t

(** Hyperbolic cosine. Argument is in radians. *)
val cosh : local_ t -> t

(** Hyperbolic sine. Argument is in radians. *)
val sinh : local_ t -> t

(** Hyperbolic tangent. Argument is in radians. *)
val tanh : local_ t -> t

(** Hyperbolic arc cosine. The argument must fall within the range [[1.0s, inf]]. Result
    is in radians and is between [0.0s] and [inf]. *)
val acosh : local_ t -> t

(** Hyperbolic arc sine. The argument and result range over the entire real line. Result
    is in radians. *)
val asinh : local_ t -> t

(** Hyperbolic arc tangent. The argument must fall within the range [[-1.0s, 1.0s]].
    Result is in radians and ranges over the entire real line. *)
val atanh : local_ t -> t

(** Square root. *)
val sqrt : local_ t -> t

(** Cube root. *)
val cbrt : local_ t -> t

(** Exponential. *)
val exp : local_ t -> t

(** Natural logarithm. *)
val log : local_ t -> t

val classify : local_ t -> Float.Class.t

(*_ Caution: If we remove this sig item, [sign] will still be present from
    [Comparable.With_zero]. *)

val sign : t -> Sign.t
[@@deprecated "[since 2016-01] Replace [sign] with [robust_sign] or [sign_exn]"]

(** The sign of a float32. Both [-0.s] and [0.s] map to [Zero]. Raises on nan. All other
    values map to [Neg] or [Pos]. *)
val sign_exn : local_ t -> Sign.t

(** The sign of a float32, with support for NaN. Both [-0.] and [0.] map to [Zero]. All
    NaN values map to [Nan]. All other values map to [Neg] or [Pos]. *)
val sign_or_nan : local_ t -> Sign_or_nan.t

(** These functions construct and destruct 32-bit floating point numbers based on their
    IEEE representation with a sign bit, an 8-bit non-negative (biased) exponent, and a
    23-bit non-negative mantissa (or significand). See
    {{:http://en.wikipedia.org/wiki/Single-precision_floating-point_format} Wikipedia} for
    details of the encoding.

    In particular, if 1 <= exponent <= 254, then:

    {[
      create_ieee_exn ~negative:false ~exponent ~mantissa
      = (2 ** (exponent - 127)) * (1 + ((2 ** -23) * mantissa))
    ]} *)
val create_ieee : negative:bool -> exponent:int -> mantissa:int -> t Or_error.t

val create_ieee_exn : negative:bool -> exponent:int -> mantissa:int -> t
val ieee_negative : local_ t -> bool
val ieee_exponent : local_ t -> int
val ieee_mantissa : local_ t -> int

(** S-expressions contain at most 8 significant digits. *)
module Terse : sig
  type nonrec t = t [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  include Stringable.S [@mode local] with type t := t
end

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val box : local_ t -> t
  val lower_bound_for_int : int -> t
  val upper_bound_for_int : int -> t
  val specialized_hash : t -> int
  val one_ulp_less_than_half : t
  val exponent_bits : int
  val mantissa_bits : int
  val exponent_mask32 : int32
  val exponent_mask : int
  val mantissa_mask32 : int32
  val mantissa_mask : int
end

module Bytes : sig
  (** [get b ~pos] loads a float32 from [b] at an offset of [pos] bytes.

      @raise Invalid_argument if [pos] is outside the range 0 to [length b - 4]. *)
  external get : bytes -> pos:int -> float32 = "%caml_bytes_getf32"

  (** [unsafe_get b ~pos] loads a float32 from [b] at an offset of [pos] bytes. Does not
      check that [pos] is a valid offset. *)
  external unsafe_get : bytes -> pos:int -> float32 = "%caml_bytes_getf32u"

  (** [set b ~pos f] stores a float32 to [b] at an offset of [pos] bytes.

      @raise Invalid_argument if [pos] is outside the range 0 to [length b - 4]. *)
  external set : bytes -> pos:int -> float32 -> unit = "%caml_bytes_setf32"

  (** [unsafe_set b ~pos f] stores a float32 to [b] at an offset of [pos] bytes. Does not
      check that [pos] is a valid offset. *)
  external unsafe_set : bytes -> pos:int -> float32 -> unit = "%caml_bytes_setf32u"
end

module String : sig
  (** [get s ~pos] loads a float32 from [s] at an offset of [pos] bytes.

      @raise Invalid_argument if [pos] is outside the range 0 to [length s - 4]. *)
  external get : string -> pos:int -> float32 = "%caml_string_getf32"

  (** [unsafe_get s ~pos] loads a float32 from [s] at an offset of [pos] bytes. Does not
      check that [pos] is a valid offset. *)
  external unsafe_get : string -> pos:int -> float32 = "%caml_string_getf32u"
end

module Bigstring : sig
  open Stdlib.Bigarray

  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  (** [get b ~pos] loads a float32 from [b] at an offset of [pos] bytes.

      @raise Invalid_argument if [pos] is outside the range 0 to [length b - 4]. *)
  external get : t @ shared -> pos:int -> float32 = "%caml_bigstring_getf32"

  (** [unsafe_get b ~pos] loads a float32 from [b] at an offset of [pos] bytes. Does not
      check that [pos] is a valid offset. *)
  external unsafe_get : t @ shared -> pos:int -> float32 = "%caml_bigstring_getf32u"

  (** [set b ~pos f] stores a float32 to [b] at an offset of [pos] bytes.

      @raise Invalid_argument if [pos] is outside the range 0 to [length b - 4]. *)
  external set : t -> pos:int -> float32 -> unit = "%caml_bigstring_setf32"

  (** [unsafe_set b ~pos f] stores a float32 to [b] at an offset of [pos] bytes. Does not
      check that [pos] is a valid offset. *)
  external unsafe_set : t -> pos:int -> float32 -> unit = "%caml_bigstring_setf32u"
end

module Bigarray : sig
  open Stdlib.Bigarray

  module Array1 : sig
    (** [Array1.get a x], or alternatively [a.{x}], returns the element of [a] at index
        [x]. [x] must be greater or equal than [0] and strictly less than [Array1.dim a]
        if [a] has C layout. If [a] has Fortran layout, [x] must be greater or equal than
        [1] and less or equal than [Array1.dim a]. Otherwise, [Invalid_argument] is
            raised. *)
    external get
      :  ('a, float32_elt, 'c) Array1.t
      -> int
      -> float32
      = "%caml_ba_float32_ref_1"

    (** [Array1.set a x v], also written [a.{x} <- v], stores the value [v] at index [x]
        in [a]. [x] must be inside the bounds of [a] as described in
        {!Bigarray.Array1.get}; otherwise, [Invalid_argument] is raised. *)
    external set
      :  ('a, float32_elt, 'c) Array1.t
      -> int
      -> float32
      -> unit
      = "%caml_ba_float32_set_1"

    (** Like {!Bigarray.Array1.get}, but bounds checking is not always performed. Use with
        caution and only when the program logic guarantees that the access is within
        bounds. *)
    external unsafe_get
      :  ('a, float32_elt, 'c) Array1.t
      -> int
      -> float32
      = "%caml_ba_float32_unsafe_ref_1"

    (** Like {!Bigarray.Array1.set}, but bounds checking is not always performed. Use with
        caution and only when the program logic guarantees that the access is within
        bounds. *)
    external unsafe_set
      :  ('a, float32_elt, 'c) Array1.t
      -> int
      -> float32
      -> unit
      = "%caml_ba_float32_unsafe_set_1"
  end

  module Array2 : sig
    (** [Array2.get a x y], also written [a.{x,y}], returns the element of [a] at
        coordinates ([x], [y]). [x] and [y] must be within the bounds of [a], as described
        for {!Bigarray.Genarray.get}; otherwise, [Invalid_argument] is raised. *)
    external get
      :  ('a, float32_elt, 'c) Array2.t
      -> int
      -> int
      -> float32
      = "%caml_ba_float32_ref_2"

    (** [Array2.set a x y v], or alternatively [a.{x,y} <- v], stores the value [v] at
        coordinates ([x], [y]) in [a]. [x] and [y] must be within the bounds of [a], as
        described for {!Bigarray.Genarray.set}; otherwise, [Invalid_argument] is raised. *)
    external set
      :  ('a, float32_elt, 'c) Array2.t
      -> int
      -> int
      -> float32
      -> unit
      = "%caml_ba_float32_set_2"

    (** Like {!Bigarray.Array2.get}, but bounds checking is not always performed. *)
    external unsafe_get
      :  ('a, float32_elt, 'c) Array2.t
      -> int
      -> int
      -> float32
      = "%caml_ba_float32_unsafe_ref_2"

    (** Like {!Bigarray.Array2.set}, but bounds checking is not always performed. *)
    external unsafe_set
      :  ('a, float32_elt, 'c) Array2.t
      -> int
      -> int
      -> float32
      -> unit
      = "%caml_ba_float32_unsafe_set_2"
  end

  module Array3 : sig
    (** [Array3.get a x y z], also written [a.{x,y,z}], returns the element of [a] at
        coordinates ([x], [y], [z]). [x], [y] and [z] must be within the bounds of [a], as
        described for {!Bigarray.Genarray.get}; otherwise, [Invalid_argument] is raised. *)
    external get
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32
      = "%caml_ba_float32_ref_3"

    (** [Array3.set a x y v], or alternatively [a.{x,y,z} <- v], stores the value [v] at
        coordinates ([x], [y], [z]) in [a]. [x], [y] and [z] must be within the bounds of
        [a], as described for {!Bigarray.Genarray.set}; otherwise, [Invalid_argument] is
        raised. *)
    external set
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32
      -> unit
      = "%caml_ba_float32_set_3"

    (** Like {!Bigarray.Array3.get}, but bounds checking is not always performed. *)
    external unsafe_get
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32
      = "%caml_ba_float32_unsafe_ref_3"

    (** Like {!Bigarray.Array3.set}, but bounds checking is not always performed. *)
    external unsafe_set
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32
      -> unit
      = "%caml_ba_float32_unsafe_set_3"
  end
end

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp ~stackify
      , stable_witness
      , string]

    include Stringable.S [@mode local] with type t := t
  end
end
