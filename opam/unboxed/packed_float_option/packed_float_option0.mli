@@ portable

(** [Packed_float_option.t] is the moral equivalent of [float option] represented under
    the hood as a regular float where [Packed_float_option.is_none = Float.is_nan].

    This type is often used whenever we have code dealing with floating point numbers in
    code and we want to avoid allocating on the OCaml heap. *)

open! Core

type t = private float
[@@deriving bin_io ~localize, globalize, sexp ~stackify, sexp_grammar, typerep]

(** This interface includes [hash]. However, the evaluation of [hash t] requires a
    comparison, and also it allocates, so this should not be used in highly
    performance-sensitive applications. *)
include%template
  Immediate_option.S_without_immediate_zero_alloc
  [@mode local]
  with type t := t
   and type value := float

[%%template:
[@@@mode.default m = (global, local)]

val of_or_null : float or_null @ m -> t @ m
val to_or_null : t @ m -> float or_null @ m]

include Comparable.Map_and_set_binable with type t := t

module Array : sig
  type elt := t
  type t [@@deriving bin_io ~localize, globalize, sexp]

  include Float_array.S with type t := t and type float_elt := elt

  module Permissioned :
    Float_array.Permissioned with type permissionless := t and type float_elt := elt

  (** Like [of_float_nan_as_none], except it reinterprets the array. Note that this is a
      view into the array, and no copy is created. *)
  val view_of_float_array_nan_as_none : Float_array.t -> t

  (** Like [to_float_none_as_nan], except it reinterprets the array. Note that this is a
      view into the array, and no copy is created. *)
  val view_to_float_array_none_as_nan : t -> Float_array.t
end

val zero : t

(** Orders [none] before anything else *)
val compare : t -> t -> int

val between : t -> low:t -> high:t -> bool

(** Unlike polymorphic compare, [none=none]. *)
val%template equal : t @ m -> t @ m -> bool
[@@mode m = (global, local)]

val value_map : t -> f:(float -> 'a) -> default:'a -> 'a

(** In some applications, float arthimetic is done where [nan] is used to denote an
    invalid value. This function can be used to interpret such results as a [t] with no
    overhead (internally, both this and [unchecked_value] are just the identity). *)
val of_float_nan_as_none : float -> t
[@@zero_alloc strict]

val to_float_none_as_nan : t -> float

(** The result of arithmetic operations will be [none] if the operand is. In addition, if
    the float operation would give [nan], the result is [none]. *)
val abs : t -> t

val neg : t -> t
val log : t -> t
val log10 : t -> t
val log1p : t -> t
val sqrt : t -> t
val square : t -> t
val exp : t -> t

(** The result of [is_inf] will be: [true]: if operand is [some inf] or [some -inf];
    [false]: otherwise, including when operand is [none] *)
val is_inf : t -> bool

(* Returns: [true]: if operand is [some val], such that [Float.is_positive val] [false]:
   otherwise, including when operand is [none]
*)
val is_positive : t -> bool

(* Returns: [true]: if operand is [some val], such that [Float.is_non_positive val]
   [false]: otherwise, including when operand is [none]
*)
val is_non_positive : t -> bool

(* Returns: [true]: if operand is [some val], such that [Float.is_negative val] [false]:
   otherwise, including when operand is [none]
*)
val is_negative : t -> bool

(* Returns: [true]: if operand is [some val], such that [Float.is_non_negative val]
   [false]: otherwise, including when operand is [none]
*)
val is_non_negative : t -> bool

(* Returns: [true]: if operand is [some val], such that [Float.is_integer val] [false]:
   otherwise, including when operand is [none]
*)
val is_integer : t -> bool

(* Returns: [true]: if operand is [some val], such that [Float.is_finite val] [false]:
   otherwise, including when operand is [none]
*)
val is_finite : t -> bool

(** The result of [min] will be [none] if either operand is. *)
val min : t -> t -> t

val inv : t -> t
val scale : t -> float -> t

(** [first_some x y] returns x if x is not none, else returns y *)
val first_some : t -> t -> t

(** [merge x y ~f] merges together the values from [x] and [y] using [f]. If both [x] and
    [y] are [None], returns [none]. If only one is [Some], returns that one, and if both
    are [Some], returns the result of applying [f] to the contents of [x] and [y]. *)
val merge : t -> t -> f:(float -> float -> float) -> t

val validate : none:unit Validate.check -> some:float Validate.check -> t Validate.check

val validate_option_bound
  :  may_be_none:bool
  -> ?min:float Maybe_bound.t
  -> ?max:float Maybe_bound.t
  -> t Validate.check

val to_string : t -> string

(** Computes [numerator / denominator] when [denominator] is nonzero, otherwise returns
    [else_]. This is done without allocating or branching by unboxing the arguments and
    then using [Unboxed.select]. *)
val divide_if_denominator_nonzero_else : numerator:t -> denominator:t -> else_:t -> t

module Infix : sig
  (** The result of arithmetic operations will be [none] if either operand is. In
      addition, if the float operation would give [nan], the result is [none]. *)

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t

  (** Returns true if both operands are [none] or (not [none] and equal). *)

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

(** Operations where we use NaN semantics even though they are inconsistent with the
    remaining operators, e.g., [>=] violates the equivalance [a >= b <-> a > b || a = b]
    because it returns [false] for two [none] inputs. *)
module Ieee_nan : sig
  module Infix : sig
    (** Returns [false] if either operand is none *)

    val ( < ) : t -> t -> bool [@@zero_alloc]
    val ( <= ) : t -> t -> bool [@@zero_alloc]
    val ( > ) : t -> t -> bool [@@zero_alloc]
    val ( >= ) : t -> t -> bool [@@zero_alloc]
  end

  include module type of Infix

  (** The result of [max] will be [none] if either operand is. This is inconsistent with
      [compare], which considers [none] values "smaller" than everything else *)
  val max : t -> t -> t

  (** Returns [none] if any argument is [none]. *)
  val clamp_exn : t -> min:t -> max:t -> t
end

module Local : sig
  val globalize : local_ t -> t
  val of_float_nan_as_none : local_ float -> local_ t
  val to_float_none_as_nan : local_ t -> local_ float
  val is_none : local_ t -> bool
  val equal : local_ t -> local_ t -> bool

  module Infix : sig
    (** Exposes local_ versions of the infix operators. Comments of [Infix] apply here as
        well. *)

    val ( + ) : local_ t -> local_ t -> local_ t
    val ( - ) : local_ t -> local_ t -> local_ t
    val ( * ) : local_ t -> local_ t -> local_ t
    val ( / ) : local_ t -> local_ t -> local_ t
    val ( = ) : local_ t -> local_ t -> bool
    val ( <> ) : local_ t -> local_ t -> bool
  end

  include module type of Infix

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : local_ t -> bool [@@zero_alloc]
      val unsafe_value : local_ t -> local_ float [@@zero_alloc]
    end
  end
end

module Stable : sig
  module V1 : sig
    (** [none] compares equal to itself, and less than any [some] value. Comparison is not
        robust. *)
    type nonrec t = t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , sexp ~stackify
      , stable_witness]
  end
end

(** A variant of the above type using unboxed floats. The operations are largely the same
    (but we can't currently share the module type across layouts.)

    (This interface is somewhat sparser than what you see above; this is mostly not a
    choice and more we haven't implemented everything. Feel free to add!) *)
module Unboxed : sig
  type boxed := t

  include module type of struct
    include Float_u.Option
  end

  module O : sig
    include module type of O

    val%template box : t @ m -> boxed @ m
    [@@zero_alloc_if_local m] [@@mode m = (global, local)]

    val unbox : boxed @ local -> t [@@zero_alloc]
  end

  include module type of O
end
