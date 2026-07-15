@@ portable

open! Core

(** Utilities for unboxed floats. This module is mostly a copy of Base's Float module, but
    with much functionality missing because it can't yet be implemented for unboxed floats
    or unboxed types generally. *)
type t = float# [@@deriving globalize, quickcheck]

module Boxed = Float

(** These definitions are available. They're included from [O] below.
    {[
      external box : float# -> (float[@local_opt]) = "%box_float"
      external unbox : (float[@local_opt]) -> float# = "%unbox_float"
    ]} *)

(** Synonyms for [box] and [unbox]. *)

external to_float : float# -> (float[@local_opt]) = "%box_float"
external of_float : (float[@local_opt]) -> float# = "%unbox_float"

(** {2 Validation}

    [validate_lbound], [validate_ubound], and [validate_bound] always fail if class is
    [Nan] or [Infinite]. The other validation functions still fail on [Nan], but permit
    [Infinite] values of the correct sign. *)

include Comparable.Validate_with_zero [@kind float64] with type t := t

(** [validate_ordinary] fails if class is [Nan] or [Infinite]. *)
val validate_ordinary : t Validate.check

(** {2 Inlined from [Identifiable], which comprises [Sexpable], [Stringable],
    [Comparable], and [Pretty_printer]} *)

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

val typerep_of_t : t Typerep.t
val typename_of_t : t Typerep_lib.Typename.t

(** {3 Inlined from [Comparable]} *)

[%%template:
[@@@mode.default m = (global, local)]

val equal : t @ m -> t @ m -> bool [@@zero_alloc]

(** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is less
    than [t2], and a positive integer if [t1] is greater than [t2]. *)
val compare : t @ m -> t @ m -> int
[@@zero_alloc]]

(** [max] and [min] will return nan if either argument is nan. *)

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

(** {3 Inlined from [Pretty_printer]} *)

val pp : Formatter.t -> t -> unit

(** {2 Inlined from [Invariant]} *)

val invariant : float# -> unit

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

(** The difference between 1.0 and the smallest exactly representable floating-point
    number greater than 1.0. That is:

    [epsilon_float = (one_ulp `Up 1.0) -. 1.0]

    This gives the relative accuracy of type [t], in the sense that for numbers on the
    order of [x], the roundoff error is on the order of [x *. float_epsilon].

    See also: {{:http://en.wikipedia.org/wiki/Machine_epsilon} Machine epsilon}. *)
val epsilon_float : t

val max_finite_value : t

(** - [min_positive_subnormal_value = 2 ** -1074]
    - [min_positive_normal_value    = 2 ** -1022] *)

val min_positive_subnormal_value : t
val min_positive_normal_value : t

(** {2 Rounding and integer conversion} *)

(** An order-preserving bijection between all floats except for nans, and all int64s with
    absolute value smaller than or equal to [2**63 - 2**52]. Note both 0. and -0. map to
    0L. *)
val to_int64_preserve_order : t -> int64 option

val to_int64_preserve_order_exn : t -> int64

(** Returns [nan] if the absolute value of the argument is too large. *)
val of_int64_preserve_order : int64 -> t

(** The next or previous representable float. ULP stands for "unit of least precision",
    and is the spacing between floating point numbers. Both [one_ulp `Up infinity] and
    [one_ulp `Down neg_infinity] return a nan. *)
val one_ulp : [ `Up | `Down ] -> t -> t

val to_int : t -> int [@@zero_alloc]
val to_int_unchecked : t -> int [@@zero_alloc]
val truncate : t -> int
val of_int63 : Int63.t -> t [@@zero_alloc]
val of_int64 : int64 -> t
val to_int64 : t -> int64

(* Convert a float# to the nearest representable float32#. *)
val to_float32_u : t -> float32# [@@zero_alloc]

(* Convert a float32# to a float# (exactly). *)
val of_float32_u : float32# -> t [@@zero_alloc]

(** [round] rounds a float to an integer float. [iround{,_exn}] rounds a float to an int.
    Both round according to a direction [dir], with default [dir] being [`Nearest].

    {v
      | `Down    | rounds toward Float.neg_infinity                             |
      | `Up      | rounds toward Float.infinity                                 |
      | `Nearest | rounds to the nearest int ("round half-integers up")         |
      | `Zero    | rounds toward zero                                           |
    v}

    [iround_exn] raises when trying to handle nan or trying to handle a float outside the
    range \[float min_int, float max_int).

    Here are some examples for [round] for each direction:

    {v
      | `Down    | [-2.,-1.)   to -2. | [-1.,0.)   to -1. | [0.,1.) to 0., [1.,2.) to 1. |
      | `Up      | (-2.,-1.]   to -1. | (-1.,0.]   to -0. | (0.,1.] to 1., (1.,2.] to 2. |
      | `Zero    | (-2.,-1.]   to -1. | (-1.,1.)   to 0.  | [1.,2.) to 1.                |
      | `Nearest | [-1.5,-0.5) to -1. | [-0.5,0.5) to 0.  | [0.5,1.5) to 1.              |
    v}

    For convenience, versions of these functions with the [dir] argument hard-coded are
    provided. If you are writing performance-critical code you should use the versions
    with the hard-coded arguments (e.g. [iround_down_exn]). The [_exn] ones are the
    fastest.

    The following properties hold:

    - [of_int (iround_*_exn i) = i] for any float [i] that is an integer with
      [min_int <= i <= max_int].

    - [round_* i = i] for any float [i] that is an integer.

    - [iround_*_exn (of_int i) = i] for any int [i] with [-2**52 <= i <= 2**52]. *)
val round : ?dir:local_ [ `Zero | `Nearest | `Up | `Down ] -> t -> t

val iround : ?dir:local_ [ `Zero | `Nearest | `Up | `Down ] -> t -> int option
val iround_exn : ?dir:local_ [ `Zero | `Nearest | `Up | `Down ] -> t -> int
val round_towards_zero : t -> t [@@zero_alloc]
val round_down : t -> t [@@zero_alloc]
val round_up : t -> t [@@zero_alloc]

(** Rounds half integers up. *)
val round_nearest : t -> t [@@zero_alloc]

(** Rounds half integers to the even integer. *)
val round_nearest_half_to_even : t -> t
[@@zero_alloc]

val iround_towards_zero : t -> int option
val iround_down : t -> int option
val iround_up : t -> int option
val iround_nearest : t -> int option
val iround_towards_zero_or_null : t -> int or_null [@@zero_alloc]
val iround_down_or_null : t -> int or_null [@@zero_alloc]
val iround_up_or_null : t -> int or_null [@@zero_alloc]
val iround_nearest_or_null : t -> int or_null [@@zero_alloc]
val iround_towards_zero_exn : t -> int [@@zero_alloc]
val iround_down_exn : t -> int [@@zero_alloc]
val iround_up_exn : t -> int [@@zero_alloc]
val iround_nearest_exn : t -> int [@@zero_alloc]
val int63_round_down_exn : t -> Int63.t
val int63_round_up_exn : t -> Int63.t
val int63_round_nearest_exn : t -> Int63.t [@@zero_alloc]
val iround_lbound : t
val iround_ubound : t
val int63_round_lbound : t
val int63_round_ubound : t

(** [round_significant x ~significant_digits:n] rounds to the nearest number with [n]
    significant digits. More precisely: it returns the representable float closest to
    [x rounded to n significant digits]. It is meant to be equivalent to
    [sprintf "%.*g" n x |> Float.of_string] but faster (10x-15x). Exact ties are resolved
    as round-to-even.

    However, it might in rare cases break the contract above.

    It might in some cases appear as if it violates the round-to-even rule:

    {[
      let x = 4.36083208835
      let z = 4.3608320883;;

      assert (z = fast_approx_round_significant x ~sf:11)
    ]}

    But in this case so does sprintf, since [x] as a float is slightly under-represented:

    {[
      sprintf "%.11g" x = "4.3608320883";;
      sprintf "%.30g" x = "4.36083208834999958014577714493"
    ]}

    More importantly, [round_significant] might sometimes give a different result than
    [sprintf ... |> Float.of_string] because it round-trips through an integer. For
    example, the decimal fraction 0.009375 is slightly under-represented as a float:

    {[
      sprintf "%.17g" 0.009375 = "0.0093749999999999997"
    ]}

    But:

    {[
      0.009375 *. 1e5 = 937.5
    ]}

    Therefore:

    {[
      round_significant 0.009375 ~significant_digits:3 = 0.00938
    ]}

    whereas:

    {[
      sprintf "%.3g" 0.009375 = "0.00937"
    ]}

    In general we believe (and have tested on numerous examples) that the following holds
    for all x:

    {[
      let s = sprintf "%.*g" significant_digits x |> Float.of_string in
      s = round_significant ~significant_digits x
      || s = round_significant ~significant_digits (one_ulp `Up x)
      || s = round_significant ~significant_digits (one_ulp `Down x)
    ]}

    Also, for float representations of decimal fractions (like 0.009375),
    [round_significant] is more likely to give the "desired" result than
    [sprintf ... |> of_string] (that is, the result of rounding the decimal fraction,
    rather than its float representation). But it's not guaranteed either--see the
    [4.36083208835] example above. *)
val round_significant : t -> significant_digits:int -> t

(** [round_decimal x ~decimal_digits:n] rounds [x] to the nearest [10**(-n)]. For positive
    [n] it is meant to be equivalent to [sprintf "%.*f" n x |> Float.of_string], but
    faster.

    All the considerations mentioned in [round_significant] apply (both functions use the
    same code path). *)
val round_decimal : t -> decimal_digits:int -> t

(** {2 Tests} *)

val is_nan : t -> bool [@@zero_alloc]

(** A float is infinite when it is either [infinity] or [neg_infinity]. *)
val is_inf : t -> bool
[@@zero_alloc]

(** A float is finite when neither [is_nan] nor [is_inf] is true. *)
val is_finite : t -> bool
[@@zero_alloc]

(** [is_integer x] is [true] if and only if [x] is an integer. *)
val is_integer : t -> bool
[@@zero_alloc]

(** [is_positive], [is_non_negative], [is_negative], and [is_non_positive] return [false]
    if [t] is [nan]. *)

val is_positive : t @ local -> bool [@@zero_alloc]
val is_non_negative : t @ local -> bool [@@zero_alloc]
val is_negative : t @ local -> bool [@@zero_alloc]
val is_non_positive : t @ local -> bool [@@zero_alloc]

(** {2 Arithmetic} *)

(** [min_inan] and [max_inan] return, respectively, the min and max of the two given
    values, except when one of the values is a [nan], in which case the other is returned.
    (Returns [nan] if both arguments are [nan].) *)

val min_inan : t -> t -> t
val max_inan : t -> t -> t

(** [mod_float x y] returns a result with the same sign as [x]. It returns [nan] if [y] is
    [0]. It is basically

    {[
      let mod_float x y = x -. (float (truncate (x /. y)) *. y)
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
[@@zero_alloc]

val sub : t -> t -> t [@@zero_alloc]
val scale : t -> t -> t [@@zero_alloc]

(** A sub-module designed to be opened to make working with floats more convenient. *)
module O : sig
  external unbox : (float[@local_opt]) -> float# = "%unbox_float"
  external box : float# -> (float[@local_opt]) = "%box_float"
  val ( + ) : t -> t -> t [@@zero_alloc strict]
  val ( - ) : t -> t -> t [@@zero_alloc strict]
  val ( * ) : t -> t -> t [@@zero_alloc strict]
  val ( / ) : t -> t -> t [@@zero_alloc strict]

  (** In analogy to Int.( % ), ( % ):
      - always produces non-negative (or NaN) result
      - raises when given a negative modulus.

      Like the other infix operators, NaNs in mean NaNs out.

      Other cases: (a % Infinity) = a when 0 <= a < Infinity, (a % Infinity) = Infinity
      when -Infinity < a < 0, (+/- Infinity % a) = NaN, (a % 0) = NaN. *)
  val ( % ) : t -> t -> t

  val ( ** ) : t -> t -> t [@@zero_alloc strict]
  val ( ~- ) : t -> t [@@zero_alloc strict]

  (* Comparisons.Infix *)
  val ( >= ) : t -> t -> bool [@@zero_alloc strict]
  val ( <= ) : t -> t -> bool [@@zero_alloc strict]
  val ( = ) : t -> t -> bool [@@zero_alloc strict]
  val ( > ) : t -> t -> bool [@@zero_alloc strict]
  val ( < ) : t -> t -> bool [@@zero_alloc strict]
  val ( <> ) : t -> t -> bool [@@zero_alloc strict]
  val abs : t -> t [@@zero_alloc strict]
  val neg : t -> t [@@zero_alloc strict]

  (** Note that this doesn't round trip in either direction. For example,
      [Float.to_int (Float.of_int max_int) <> max_int]. *)
  val of_int : int -> t
  [@@zero_alloc strict]
end

include module type of O (** @inline *)

(** Similar to [O], except that operators are suffixed with a dot, allowing one to have
    both int and float operators in scope simultaneously. *)
module O_dot : sig
  val ( +. ) : t -> t -> t [@@zero_alloc]
  val ( -. ) : t -> t -> t [@@zero_alloc]
  val ( *. ) : t -> t -> t [@@zero_alloc]
  val ( /. ) : t -> t -> t [@@zero_alloc]
  val ( %. ) : t -> t -> t
  val ( **. ) : t -> t -> t [@@zero_alloc]
  val ( ~-. ) : t -> t [@@zero_alloc]
end

(** {2 String conversions} *)

(** [to_string x] builds a string [s] representing the float [x] that guarantees the round
    trip, that is such that [Float_u.equal x (Float_u.of_string s)].

    It usually yields as few significant digits as possible. That is, it won't print
    [3.14] as [3.1400000000000001243]. The only exception is that occasionally it will
    output 17 significant digits when the number can be represented with just 16 (but not
    15 or less) of them. *)
val to_string : t -> string

(** [of_string] is inverse to [to_string]. *)
val of_string : string @ local -> t

(** Pretty print float, for example [to_string_hum ~decimals:3 1234.1999 = "1_234.200"]
    [to_string_hum ~decimals:3 ~strip_zero:true 1234.1999 = "1_234.2" ]. No delimiters are
    inserted to the right of the decimal. *)
val to_string_hum
  :  ?delimiter:char (** defaults to ['_'] *)
  -> ?decimals:int (** defaults to [3] *)
  -> ?strip_zero:bool (** defaults to [false] *)
  -> ?explicit_plus:bool
       (** Forces a + in front of non-negative values. Defaults to [false] *)
  -> t
  -> string

(** Produce a lossy compact string representation of the float. The float is scaled by an
    appropriate power of 1000 and rendered with one digit after the decimal point, except
    that the decimal point is written as '.', 'k', 'm', 'g', 't', or 'p' to indicate the
    scale factor. (However, if the digit after the "decimal" point is 0, it is
    suppressed.)

    The smallest scale factor that allows the number to be rendered with at most 3 digits
    to the left of the decimal is used. If the number is too large for this format (i.e.,
    the absolute value is at least 999.95e15), scientific notation is used instead. E.g.:

    - [to_padded_compact_string     (-0.01) =  "-0  "]
    - [to_padded_compact_string       1.89  =   "1.9"]
    - [to_padded_compact_string 999_949.99  = "999k9"]
    - [to_padded_compact_string 999_950.    =   "1m "]

    In the case where the digit after the "decimal", or the "decimal" itself is omitted,
    the numbers are padded on the right with spaces to ensure the last two columns of the
    string always correspond to the decimal and the digit afterward (except in the case of
    scientific notation, where the exponent is the right-most element in the string and
    could take up to four characters).

    - [to_padded_compact_string    1. =    "1  "]
    - [to_padded_compact_string  1.e6 =    "1m "]
    - [to_padded_compact_string 1.e16 = "1.e+16"]
    - [to_padded_compact_string max_finite_value = "1.8e+308"]

    Numbers in the range -.05 < x < .05 are rendered as "0 " or "-0 ".

    Other cases:

    - [to_padded_compact_string nan          =  "nan  "]
    - [to_padded_compact_string infinity     =  "inf  "]
    - [to_padded_compact_string neg_infinity = "-inf  "]

    Exact ties are resolved to even in the decimal:

    - [to_padded_compact_string      3.25 =  "3.2"]
    - [to_padded_compact_string      3.75 =  "3.8"]
    - [to_padded_compact_string 33_250.   = "33k2"]
    - [to_padded_compact_string 33_350.   = "33k4"]

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

(** [int_pow x n] computes [x ** float n] via repeated squaring. It is generally much
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
val int_pow : t -> int -> t

(** [square x] returns [x *. x]. *)
val square : t -> t [@@zero_alloc]

(** [ldexp x n] returns [x *. 2 ** n] *)
val ldexp : t -> int -> t [@@zero_alloc]

(** Base 10 logarithm. *)
val log10 : t -> t [@@zero_alloc]

(** Base 2 logarithm. *)
val log2 : t -> t [@@zero_alloc]

(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results even if [x] is
    close to [0.0]. *)
val expm1 : t -> t
[@@zero_alloc]

(** [log1p x] computes [log(1.0 +. x)] (natural logarithm), giving numerically-accurate
    results even if [x] is close to [0.0]. *)
val log1p : t -> t
[@@zero_alloc]

(** [copysign x y] returns a float whose absolute value is that of [x] and whose sign is
    that of [y]. If [x] is [nan], returns [nan]. If [y] is [nan], returns either [x] or
    [-. x], but it is not specified which. *)
val copysign : t -> t -> t
[@@zero_alloc]

(** Cosine. Argument is in radians. *)
val cos : t -> t [@@zero_alloc]

(** Sine. Argument is in radians. *)
val sin : t -> t [@@zero_alloc]

(** Tangent. Argument is in radians. *)
val tan : t -> t [@@zero_alloc]

(** Arc cosine. The argument must fall within the range [[-1.0, 1.0]]. Result is in
    radians and is between [0.0] and [pi]. *)
val acos : t -> t
[@@zero_alloc]

(** Arc sine. The argument must fall within the range [[-1.0, 1.0]]. Result is in radians
    and is between [-pi/2] and [pi/2]. *)
val asin : t -> t
[@@zero_alloc]

(** Arc tangent. Result is in radians and is between [-pi/2] and [pi/2]. *)
val atan : t -> t
[@@zero_alloc]

(** [atan2 y x] returns the arc tangent of [y /. x]. The signs of [x] and [y] are used to
    determine the quadrant of the result. Result is in radians and is between [-pi] and
    [pi]. *)
val atan2 : t -> t -> t
[@@zero_alloc]

(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length of the hypotenuse of
    a right-angled triangle with sides of length [x] and [y], or, equivalently, the
    distance of the point [(x,y)] to origin. *)
val hypot : t -> t -> t
[@@zero_alloc]

(** Hyperbolic cosine. Argument is in radians. *)
val cosh : t -> t [@@zero_alloc]

(** Hyperbolic sine. Argument is in radians. *)
val sinh : t -> t [@@zero_alloc]

(** Hyperbolic tangent. Argument is in radians. *)
val tanh : t -> t [@@zero_alloc]

(** Hyperbolic arc cosine. The argument must fall within the range [[1.0, inf]]. Result is
    in radians and is between [0.0] and [inf]. *)
val acosh : t -> t
[@@zero_alloc]

(** Hyperbolic arc sine. The argument and result range over the entire real line. Result
    is in radians. *)
val asinh : t -> t
[@@zero_alloc]

(** Hyperbolic arc tangent. The argument must fall within the range [[-1.0, 1.0]]. Result
    is in radians and ranges over the entire real line. *)
val atanh : t -> t
[@@zero_alloc]

(** Square root. *)
val sqrt : t -> t [@@zero_alloc]

(** Cube root. *)
val cbrt : t -> t

(** Exponential. *)
val exp : t -> t [@@zero_alloc]

(** Natural logarithm. *)
val log : t -> t [@@zero_alloc]

(** {2 Classification and representation} *)

(** Excluding nan the floating-point "number line" looks like:
    {v
             t                Class.t    example
           ^ neg_infinity     Infinite   neg_infinity
           | neg normals      Normal     -3.14
           | neg subnormals   Subnormal  -.2. ** -1023.
           | (-/+) zero       Zero       0.
           | pos subnormals   Subnormal  2. ** -1023.
           | pos normals      Normal     3.14
           v infinity         Infinite   infinity
    v} *)
module Class = Base.Float.Class

val classify : t -> Class.t [@@zero_alloc]

(*_ Caution: If we remove this sig item, [sign] will still be present from
    [Comparable.With_zero]. *)

val sign : t -> Sign.t
[@@deprecated "[since 2016-01] Replace [sign] with [robust_sign] or [sign_exn]"]

(** The sign of a float. Both [-0.] and [0.] map to [Zero]. Raises on nan. All other
    values map to [Neg] or [Pos]. *)
val sign_exn : t -> Sign.t
[@@zero_alloc]

(** The sign of a float, with support for NaN. Both [-0.] and [0.] map to [Zero]. All NaN
    values map to [Nan]. All other values map to [Neg] or [Pos]. *)
val sign_or_nan : t -> Sign_or_nan.t
[@@zero_alloc]

(** These functions construct and destruct 64-bit floating point numbers based on their
    IEEE representation with a sign bit, an 11-bit non-negative (biased) exponent, and a
    52-bit non-negative mantissa (or significand). See
    {{:http://en.wikipedia.org/wiki/Double-precision_floating-point_format} Wikipedia} for
    details of the encoding.

    In particular, if 1 <= exponent <= 2046, then:

    {[
      create_ieee_exn ~negative:false ~exponent ~mantissa
      = (2 ** (exponent - 1023)) * (1 + ((2 ** -52) * mantissa))
    ]} *)

val create_ieee_exn : negative:bool -> exponent:int -> mantissa:Int63.t -> t
val ieee_negative : t -> bool
val ieee_exponent : t -> int
val ieee_mantissa : t -> Int63.t

(** Branchless, as [Bool.select]. *)
val select : bool -> t -> t -> t [@@zero_alloc]

(** Branchless. *)
val first_non_nan : t -> t -> t [@@zero_alloc]

val to_bits : t -> int64#
val of_bits : int64# -> t

(** {2 Arrays} *)

module type Array_getters_and_setters := sig
  type t
  type elt : float64

  val get : local_ t -> int -> elt
  val set : local_ t -> int -> elt -> unit
  val unsafe_get : local_ t -> int -> elt
  val unsafe_set : local_ t -> int -> elt -> unit
end

module type Array = sig
  type t
  type elt : float64

  include Array_getters_and_setters with type t := t and type elt := elt

  val get : local_ t -> int -> elt [@@zero_alloc]
  val set : local_ t -> int -> elt -> unit [@@zero_alloc]
  val unsafe_get : local_ t -> int -> elt [@@zero_alloc]
  val unsafe_set : local_ t -> int -> elt -> unit [@@zero_alloc]
  val create : len:int -> elt -> t
  val length : local_ t -> int

  val unsafe_blit
    :  src:local_ t
    -> src_pos:int
    -> dst:local_ t
    -> dst_pos:int
    -> len:int
    -> unit

  val compare : t -> t -> int
  val globalize : local_ t -> t
  val copy : t -> t
  val t_of_sexp : Sexp.t -> t @@ portable
  val sexp_of_t : t -> Sexp.t @@ portable
  val custom_sexp_of_t : (elt -> Sexp.t) -> t -> Sexp.t
  val custom_t_of_sexp : (Sexp.t -> elt) -> Sexp.t -> t
  val init : int -> f:(int -> elt) -> t
  val iter : t -> f:(elt -> unit) -> unit
  val iteri : t -> f:(int -> elt -> unit) -> unit

  (** Identity no-op conversions between [t] and [elt array]. They are the same type at
      runtime. *)

  val%template to_float_u_array : t @ m -> elt array @ m [@@mode m = (local, global)]
  val%template of_float_u_array : elt array @ m -> t @ m [@@mode m = (local, global)]
end

(** The [Array] module provides some helpers that wrap common operations on [floatarray]s
    with appropriate boxing or unboxing to work with [float#].

    These functions are provided just for convenience. They are equivalent (in semantics
    and performance) to just using the same functions from the [Float_array] library and
    appropriate calls to [to_float] or [of_float]. The compiler should erase the extra
    boxing/unboxing steps either way. *)
module Array : sig
  include Array with type t = Float_array.t and type elt := float#

  include%template Bin_prot.Binable.S with type t := t

  module Permissioned : sig
    type -'perms t = 'perms Float_array.Permissioned.t

    val get : local_ [> read ] t -> int -> float# [@@zero_alloc]
    val set : local_ [> write ] t -> int -> float# -> unit [@@zero_alloc]
    val unsafe_get : local_ [> read ] t -> int -> float# [@@zero_alloc]
    val unsafe_set : local_ [> write ] t -> int -> float# -> unit [@@zero_alloc]
  end
end

(** The [Polymorphic_array_helpers] module provides some helpers that wrap common
    operations on [float array]s with appropriate boxing or unboxing to work with
    [float#].

    These functions are provided just for convenience. They are equivalent (in semantics
    and performance) to just using the same functions from [Base.Array] and appropriate
    calls to [to_float] or [of_float]. The compiler should erase the extra boxing/unboxing
    steps either way.

    The name is unwieldy on purpose. We encourage using [Float_u.Array] over
    [Float_u.Polymorphic_array_helpers] for new code. *)
module Polymorphic_array_helpers :
  Array_getters_and_setters with type t := float array and type elt := float#

module type Ref = Ref_intf.T

module Ref : sig
  type t = { mutable contents : float# }

  include Ref with type elt := float# and type t := t
end

(** The moral equivalent of [t option] implemented in-place with [is_none] as [is_nan].

    By design, we can do math on this type and propagate NaN (none) status. *)

module Option : sig
  type t : float64 mod everything
  [@@deriving globalize, quickcheck, sexp ~stackify, typerep, bin_io ~localize, hash]

  include Immediate_option.S_unboxed_float64 with type t := t with type value := float#

  val unsafe_value : t -> float#
  val to_string : t -> string
  val none : t
  val is_some : t -> bool [@@zero_alloc]
  val some : float# -> t [@@zero_alloc]
  val unchecked_some : float# -> t [@@zero_alloc]
  val of_float_nan_as_none : float# -> t [@@zero_alloc strict]
  val to_float_none_as_nan : t -> float# [@@zero_alloc]
  val zero : t
  val one : t
  val scale : t -> float# -> t [@@zero_alloc]
  val div : t -> float# -> t [@@zero_alloc]

  [%%template:
  [@@@mode.default m = (global, local)]

  val equal : t @ m -> t @ m -> bool [@@zero_alloc]
  val of_option : float option @ m -> t [@@zero_alloc]
  val to_option : t -> float option @ m]

  val%template of_or_null : float or_null @ m -> t
  [@@mode m = (global, local)] [@@zero_alloc]

  val%template to_or_null : t -> float or_null @ m
  [@@alloc a @ m = (heap @ global, stack @ local)] [@@zero_alloc_if_stack a]

  val select : bool -> t -> t -> t [@@zero_alloc]
  val some_if : bool -> float# -> t [@@zero_alloc]

  (** A workaround for the lack of unboxed float literals; strictly speaking one can use
      it for non constants but the name is a helpful reminder. *)
  val const : float -> t
  [@@zero_alloc]

  (** [first_some x y] returns x if x is not none, else returns y *)
  val first_some : t -> t -> t
  [@@zero_alloc]

  (** [some_or t ~default = first_some t (unchecked_some t)] *)
  val some_or : t -> default:float# -> t
  [@@zero_alloc]

  val merge : t -> t -> f:(float# -> float# -> float#) -> t

  (** Computes [numerator / denominator] when [denominator] is nonzero, otherwise returns
      [else_]. This is done without allocating or branching by using [Unboxed.select]. *)
  val divide_if_denominator_nonzero_else : numerator:t -> denominator:t -> else_:t -> t
  [@@zero_alloc]

  val value : t -> default:float# -> float# [@@zero_alloc]
  val value_exn : t -> float# [@@zero_alloc]
  val compare : t -> t -> int [@@zero_alloc]

  (** [clamp_exn t ~min ~max] clamps [t] to the range [[min, max]]. A [none] bound does
      not clamp on that side. Returns [none] if [t] is [none]. *)
  val clamp_exn : t -> min:t -> max:t -> t

  (** Returns [false] if [t] is [none] *)

  val is_finite : t -> bool [@@zero_alloc]
  val is_inf : t -> bool [@@zero_alloc]
  val is_positive : t -> bool [@@zero_alloc]
  val is_non_negative : t -> bool [@@zero_alloc]
  val is_negative : t -> bool [@@zero_alloc]
  val is_non_positive : t -> bool [@@zero_alloc]
  val is_integer : t -> bool [@@zero_alloc]

  module O : sig
    val ( + ) : t -> t -> t [@@zero_alloc]
    val ( - ) : t -> t -> t [@@zero_alloc]
    val ( * ) : t -> t -> t [@@zero_alloc]
    val ( / ) : t -> t -> t [@@zero_alloc]
    val ( ** ) : t -> t -> t [@@zero_alloc]
    val ( = ) : t -> t -> bool [@@zero_alloc]
    val ( <> ) : t -> t -> bool [@@zero_alloc]
    val abs : t -> t [@@zero_alloc]
    val neg : t -> t [@@zero_alloc]

    (** The result of [min] will be [none] if either operand is. *)
    val min : t -> t -> t
    [@@zero_alloc]

    (** The result of [max] will be [none] if either operand is. (Note that this disagrees
        with [compare], which thinks [none] is less than all other values!) *)
    val max : t -> t -> t
    [@@zero_alloc]
  end

  include module type of O

  (** Operations where we use NaN semantics even though they are inconsistent with the
      remaining operators, e.g., [>=] violates the equivalance [a >= b <-> a > b || a = b]
      because it returns [false] for two [none] inputs. *)
  module Ieee_nan : sig
    val ( < ) : t -> t -> bool [@@zero_alloc]
    val ( <= ) : t -> t -> bool [@@zero_alloc]
    val ( > ) : t -> t -> bool [@@zero_alloc]
    val ( >= ) : t -> t -> bool [@@zero_alloc]
  end

  module Ref : sig
    include Ref_intf.T with type elt := t

    val create_none : unit -> t
    val set_none : local_ t -> unit [@@zero_alloc]
    val set_float_nan_as_none : local_ t -> float# -> unit [@@zero_alloc]
  end

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      [@@deriving
        globalize
        , stable_witness
        , sexp ~stackify
        , bin_io ~localize
        , hash
        , typerep
        , equal
        , compare]
    end
  end
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving globalize, stable_witness]

    (** We derive [sexp], [bin_io], [hash], [typerep], [string], [equal], and [compare]. *)

    val%template sexp_of_t : t @ m -> Sexp.t @ m
    [@@alloc a @ m = (heap @ global, stack @ local)]

    val t_of_sexp : Sexp.t -> t

    include%template Bin_prot.Binable.S [@mode local] with type t := t

    include Ppx_hash_lib.Hashable.S with type t := t

    val typerep_of_t : t Typerep.t
    val typename_of_t : t Typerep_lib.Typename.t
    val of_string : string -> t
    val to_string : t -> string
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end
end
