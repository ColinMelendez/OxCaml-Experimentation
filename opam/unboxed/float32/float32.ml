open! Base
open Printf

let raise_s = Error.raise_s

external float32_of_string
  :  local_ string
  -> float32
  @@ portable
  = "caml_float32_of_string"

external of_bits
  :  (int32[@local_opt])
  -> float32
  @@ portable
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
[@@unboxed] [@@noalloc] [@@builtin]

external to_bits
  :  (float32[@local_opt])
  -> int32
  @@ portable
  = "caml_float32_to_bits_bytecode" "caml_float32_to_bits"
[@@unboxed] [@@noalloc] [@@builtin]

module Util = struct
  external format_float32
    :  string
    -> local_ float32
    -> string
    @@ portable
    = "caml_format_float32"

  external globalize_float32 : local_ float32 -> float32 @@ portable = "%obj_dup"

  let hash_float32 = Hashtbl.hash
  let hash_fold_float32 state t = hash_fold_int32 state (to_bits t)
  let exn_to_string e = Sexp.to_string_hum (sexp_of_exn e)
  let of_sexp_error what sexp = raise (Sexp.Of_sexp_error (Failure what, sexp))

  let float32_of_sexp (sexp : Sexp.t) =
    match sexp with
    | Atom str ->
      (try float32_of_string str with
       | exc -> of_sexp_error ("float32_of_sexp: " ^ exn_to_string exc) sexp)
    | List _ -> of_sexp_error "float32_of_sexp: atom needed" sexp
  ;;

  let%template[@alloc a = (heap, stack)] sexp_of_float32 n =
    let atom = format_float32 "%.9G" n in
    Sexp.Atom atom [@exclave_if_stack a]
  ;;

  module Float32_replace_polymorphic_compare = struct
    external ( < )
      :  (float32[@local_opt])
      -> (float32[@local_opt])
      -> bool
      @@ portable
      = "%lessthan"

    external ( <= )
      :  (float32[@local_opt])
      -> (float32[@local_opt])
      -> bool
      @@ portable
      = "%lessequal"

    external ( <> )
      :  (float32[@local_opt])
      -> (float32[@local_opt])
      -> bool
      @@ portable
      = "%notequal"

    external ( = )
      :  (float32[@local_opt])
      -> (float32[@local_opt])
      -> bool
      @@ portable
      = "%equal"

    external ( > )
      :  (float32[@local_opt])
      -> (float32[@local_opt])
      -> bool
      @@ portable
      = "%greaterthan"

    external ( >= )
      :  (float32[@local_opt])
      -> (float32[@local_opt])
      -> bool
      @@ portable
      = "%greaterequal"

    [%%template
    [@@@mode.default m = (global, local)]

    external equal
      :  (float32[@local_opt])
      -> (float32[@local_opt])
      -> bool
      @@ portable
      = "%equal"

    external compare
      :  (float32[@local_opt])
      -> (float32[@local_opt])
      -> int
      @@ portable
      = "%compare"

    let max (x : float32) y =
      let geq = x >= y in
      Bool.select geq x y [@exclave_if_local m]
    ;;

    let min (x : float32) y =
      let leq = x <= y in
      Bool.select leq x y [@exclave_if_local m]
    ;;]

    let ascending (x : float32) y = Poly.ascending x y
    let descending (x : float32) y = Poly.descending x y
  end

  [%%template
  [@@@mode.default m = (local, global)]

  let equal_float32 = (Float32_replace_polymorphic_compare.equal [@mode m])
  let compare_float32 = (Float32_replace_polymorphic_compare.compare [@mode m])]

  include struct
    open Bin_prot

    let bin_shape_float32 = Shape.basetype (Shape.Uuid.of_string "float32") []

    let%template[@mode m = (global, local)] bin_size_float32 f =
      (Size.bin_size_int32_bits [@mode m]) (to_bits f)
    ;;

    let%template[@mode m = (global, local)] bin_write_float32 buf ~pos f =
      (Write.bin_write_int32_bits [@mode m]) buf ~pos (to_bits f)
    ;;

    let bin_read_float32 buf ~pos_ref = Read.bin_read_int32_bits buf ~pos_ref |> of_bits

    let __bin_read_float32__ _buf ~pos_ref _vint =
      Common.raise_variant_wrong_type "float32" !pos_ref
    ;;

    let bin_writer_float32 = [%bin_writer: float32]
    let bin_reader_float32 = [%bin_reader: float32]
    let bin_float32 = [%bin_type_class: float32]
  end

  (* Stolen from [pervasives.ml]. Adds a "." at the end if needed. It is in
     [pervasives.mli], but it also says not to use it directly, so we copy and paste the
     code. It makes the assumption on the string passed in argument that it was returned
     by [format_float]. *)
  let valid_float_lexem s =
    let l = String.length s in
    let rec loop i =
      if Int.( >= ) i l
      then s ^ "."
      else (
        match s.[i] with
        | '0' .. '9' | '-' -> loop (i + 1)
        | _ -> s)
    in
    loop 0
  ;;
end

module Stable = struct
  module V1 = struct
    open Util

    type t = float32
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp ~stackify]

    let to_string x = valid_float_lexem (format_float32 "%.9g" x)

    let of_string s =
      try float32_of_string s with
      | _ -> invalid_argf "Float32.of_string %s" (globalize_string s) ()
    ;;

    let stable_witness = Ppx_stable_witness_runtime.Stable_witness.assert_stable
  end
end

module T = struct
  include Stable.V1
end

include T

let hashable : t Hashable.t = { hash; compare; sexp_of_t }

include%template Comparator.Make [@mode local] [@modality portable] (T)

(* We include type-specific [Replace_polymorphic_compare] at the end, after including
   functor application that could shadow its definitions. This is here so that efficient
   versions of the comparison functions are exported by this module. *)

open Util.Float32_replace_polymorphic_compare

let nan = Stdlib_stable.Float32.nan
let infinity = Stdlib_stable.Float32.infinity
let neg_infinity = Stdlib_stable.Float32.neg_infinity
let max_finite_value = Stdlib_stable.Float32.max_float
let epsilon_float = Stdlib_stable.Float32.epsilon

external add : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%addfloat32"
external sub : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%subfloat32"
external mul : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%mulfloat32"
external div : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%divfloat32"
external neg : local_ t -> (t[@local_opt]) @@ portable = "%negfloat32"
external abs : local_ t -> (t[@local_opt]) @@ portable = "%absfloat32"

let%template[@mode m = (global, local)] square x = mul x x
let is_inf x = div 1.s x = 0.s
let is_finite x = sub x x = 0.s
let is_nan x = x <> x

external pow
  :  local_ t
  -> local_ t
  -> t
  @@ portable
  = "caml_power_float32_bytecode" "powf"
[@@unboxed] [@@noalloc]

external trunc : local_ t -> t @@ portable = "caml_trunc_float32_bytecode" "truncf"
[@@unboxed] [@@noalloc]

let is_integer x = x = trunc x && is_finite x

let to_int32_preserve_order t =
  if is_nan t
  then None
  else if t = 0.s
  then (* also includes -0. *)
    Some 0l
  else if t > 0.s
  then Some (to_bits t)
  else Some (Int32.neg (to_bits (neg t)))
;;

let to_int32_preserve_order_exn x = Option.value_exn (to_int32_preserve_order x)

let of_int32_preserve_order x =
  if Stdlib.( >= ) x 0l then of_bits x else neg (of_bits (Stdlib.Int32.neg x))
;;

let one_ulp dir t =
  match to_int32_preserve_order t with
  | None -> nan
  | Some x ->
    of_int32_preserve_order
      (Int32.( + )
         x
         (match dir with
          | `Up -> 1l
          | `Down -> -1l))
;;

external mod_float
  :  local_ t
  -> local_ t
  -> t
  @@ portable
  = "caml_fmod_float32_bytecode" "fmodf"
[@@unboxed] [@@noalloc]

external frexp : local_ t -> t * int @@ portable = "caml_frexp_float32"

external ldexp
  :  (t[@unboxed] [@local_opt])
  -> (int[@untagged])
  -> (t[@unboxed])
  @@ portable
  = "caml_ldexp_float32_bytecode" "caml_ldexp_float32"
[@@noalloc]

external log10 : local_ t -> t @@ portable = "caml_log10_float32_bytecode" "log10f"
[@@unboxed] [@@noalloc]

external log2 : local_ t -> t @@ portable = "caml_log2_float32_bytecode" "log2f"
[@@unboxed] [@@noalloc]

external expm1 : local_ t -> t @@ portable = "caml_expm1_float32_bytecode" "expm1f"
[@@unboxed] [@@noalloc]

external log1p : local_ t -> t @@ portable = "caml_log1p_float32_bytecode" "log1pf"
[@@unboxed] [@@noalloc]

external cos : local_ t -> t @@ portable = "caml_cos_float32_bytecode" "cosf"
[@@unboxed] [@@noalloc]

external sin : local_ t -> t @@ portable = "caml_sin_float32_bytecode" "sinf"
[@@unboxed] [@@noalloc]

external tan : local_ t -> t @@ portable = "caml_tan_float32_bytecode" "tanf"
[@@unboxed] [@@noalloc]

external acos : local_ t -> t @@ portable = "caml_acos_float32_bytecode" "acosf"
[@@unboxed] [@@noalloc]

external asin : local_ t -> t @@ portable = "caml_asin_float32_bytecode" "asinf"
[@@unboxed] [@@noalloc]

external atan : local_ t -> t @@ portable = "caml_atan_float32_bytecode" "atanf"
[@@unboxed] [@@noalloc]

external atan2
  :  local_ t
  -> local_ t
  -> t
  @@ portable
  = "caml_atan2_float32_bytecode" "atan2f"
[@@unboxed] [@@noalloc]

external hypot
  :  local_ t
  -> local_ t
  -> t
  @@ portable
  = "caml_hypot_float32_bytecode" "hypotf"
[@@unboxed] [@@noalloc]

external cosh : local_ t -> t @@ portable = "caml_cosh_float32_bytecode" "coshf"
[@@unboxed] [@@noalloc]

external sinh : local_ t -> t @@ portable = "caml_sinh_float32_bytecode" "sinhf"
[@@unboxed] [@@noalloc]

external tanh : local_ t -> t @@ portable = "caml_tanh_float32_bytecode" "tanhf"
[@@unboxed] [@@noalloc]

external acosh : local_ t -> t @@ portable = "caml_acosh_float32_bytecode" "acoshf"
[@@unboxed] [@@noalloc]

external asinh : local_ t -> t @@ portable = "caml_asinh_float32_bytecode" "asinhf"
[@@unboxed] [@@noalloc]

external atanh : local_ t -> t @@ portable = "caml_atanh_float32_bytecode" "atanhf"
[@@unboxed] [@@noalloc]

external sqrt : local_ t -> t @@ portable = "caml_sqrt_float32_bytecode" "sqrtf"
[@@unboxed] [@@noalloc] [@@builtin]

external cbrt : local_ t -> t @@ portable = "caml_cbrt_float32_bytecode" "cbrtf"
[@@unboxed] [@@noalloc]

external exp : local_ t -> t @@ portable = "caml_exp_float32_bytecode" "expf"
[@@unboxed] [@@noalloc]

external log : local_ t -> t @@ portable = "caml_log_float32_bytecode" "logf"
[@@unboxed] [@@noalloc]

external copysign
  :  local_ t
  -> local_ t
  -> t
  @@ portable
  = "caml_copysign_float32_bytecode" "copysignf"
[@@unboxed] [@@noalloc]

let is_x_minus_one_exact x =
  (* [x = x -. 1.] does not work with x87 floating point arithmetic backend (which is used
     on 32-bit ocaml) because of 80-bit register precision of intermediate computations.

     An alternative way of computing this: [x -. one_ulp `Down x <= 1.] is also prone to
     the same precision issues: you need to make sure [x] is 64-bit.
  *)
  not (Int32.equal (to_bits x) (to_bits (sub x 1.s)))
;;

let lower_bound_for_int bound_bits =
  let min_int_as_float32 = ldexp (-1.s) (bound_bits - 1) in
  let open Int in
  if bound_bits - 1 < 24 (* 24 = #bits in the float's mantissa with sign included *)
  then (
    (* The smallest float that rounds towards zero to [min_int] is [min_int - 1 + epsilon] *)
    assert (is_x_minus_one_exact min_int_as_float32);
    one_ulp `Up (sub min_int_as_float32 1.s))
  else (
    (* [min_int_as_float32] is already the smallest float [f] satisfying
       [f > min_int - 1]. *)
    assert (not (is_x_minus_one_exact min_int_as_float32));
    min_int_as_float32)
;;

(* [upper_bound_for_int] and [lower_bound_for_int] are for calculating the max/min float
   that fits in a given-size integer when rounded towards 0 (using [int_of_float]).

   max_int/min_int depend on [num_bits], e.g. +/- 2^30, +/- 2^62 if 31-bit, 63-bit
   (respectively) while float is IEEE standard for double (52 significant bits).

   In all cases, we want to guarantee that
   [lower_bound_for_int <= x <= upper_bound_for_int] iff [int_of_float x] fits in an int
   with [num_bits] bits.

   [2 ** (num_bits - 1)] is the first float greater that max_int, we use the preceding
   float as upper bound.

   [- (2 ** (num_bits - 1))] is equal to min_int. For lower bound we look for the smallest
   float [f] satisfying [f > min_int - 1] so that [f] rounds toward zero to [min_int]

   So in particular we will have: [lower_bound_for_int x <= - (2 ** (1-x))]
   [upper_bound_for_int x  <    2 ** (1-x) ]
*)
let upper_bound_for_int num_bits = one_ulp `Down (ldexp 1.s (num_bits - 1))

let box =
  (* Prevent potential constant folding of [+ (-0.s)] in the near ocamlopt future. The
     reason we add [-0.s] rather than [0.s] is that [x + (-0.s)] is always the same as
     [x], whereas [x + 0.s] is not, in that it sends [-0.s] to [0.s]. *)
  let x = Sys.opaque_identity (-0.s) in
  fun (local_ f) -> add (globalize f) x
;;

let invariant (_ : t) = ()

let of_string_opt s =
  try Some (float32_of_string s) with
  | Failure _ -> None
;;

let max_value = infinity
let min_value = neg_infinity
let min_positive_subnormal_value = pow 2.s (-149.s)
let min_positive_normal_value = Stdlib_stable.Float32.min_float
let zero = Stdlib_stable.Float32.zero
let one = Stdlib_stable.Float32.one
let minus_one = Stdlib_stable.Float32.minus_one
let pi = Stdlib_stable.Float32.pi
let sqrt_pi = 0x1.c5bf8ap+0s
let sqrt_2pi = 0x1.40d932p+1s
let euler_gamma_constant = 0x1.2788dp-1s

external of_int64
  :  (int64[@local_opt])
  -> t
  @@ portable
  = "caml_float32_of_int64_bytecode" "caml_float32_of_int64"
[@@unboxed] [@@noalloc] [@@builtin]

external to_int64
  :  (t[@local_opt])
  -> int64
  @@ portable
  = "caml_float32_to_int64_bytecode" "caml_float32_to_int64"
[@@unboxed] [@@noalloc] [@@builtin]

let iround_lbound = lower_bound_for_int Int.num_bits
let iround_ubound = upper_bound_for_int Int.num_bits

external of_float : local_ float -> (t[@local_opt]) @@ portable = "%float32offloat"
external to_float : local_ t -> (float[@local_opt]) @@ portable = "%floatoffloat32"
external of_int : int -> (t[@local_opt]) @@ portable = "%float32ofint"
external to_int_unchecked : local_ t -> int @@ portable = "%intoffloat32"

let to_int f =
  if f >= iround_lbound && f <= iround_ubound
  then to_int_unchecked f
  else
    Printf.invalid_argf
      "Float32.to_int: argument (%f) is out of range or NaN"
      (to_float f)
      ()
;;

(* Outside of the range (round_nearest_lb..round_nearest_ub), all representable doubles
   are integers in the mathematical sense, and [round_nearest] should be identity.

   However, for odd numbers with the absolute value between 2**23 and 2**24, the formula
   [round_nearest x = floor (x + 0.5)] does not hold:

   {v
     # let naive_round_nearest x = floor (x +. 0.5s);;
     # let x = 2.s ** 23.s +. 1.s;;
     val x : float32 = 4503599627370497.s
     # naive_round_nearest x;;
     - :     float32 = 4503599627370498.s
   v}
*)

let round_nearest_lb = neg (pow 2.s 23.s)
let round_nearest_ub = pow 2.s 23.s

(* For [x = one_ulp `Down 0.5s], the formula [floor (x +. 0.5s)] for rounding to nearest
   does not work, because the exact result is halfway between [one_ulp `Down 1.s] and
   [1.s], and it gets rounded up to [1.s] due to the round-ties-to-even rule. *)
let one_ulp_less_than_half = one_ulp `Down 0.5s

let[@ocaml.inline always] add_half_for_round_nearest t = exclave_
  add
    t
    (if t = one_ulp_less_than_half
     then one_ulp_less_than_half (* since t < 0.5s, make sure the result is < 1.0s *)
     else 0.5s)
;;

module Rounding_intrinsics = struct
  external iround_current
    :  t @ local
    -> int64
    @@ portable
    = "caml_simd_cast_float32_int64_bytecode" "caml_simd_cast_float32_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

  external round_current
    :  t @ local
    -> t
    @@ portable
    = "caml_simd_float32_round_current_bytecode" "caml_simd_float32_round_current"
  [@@noalloc] [@@unboxed] [@@builtin]

  external round_down
    :  t @ local
    -> t
    @@ portable
    = "caml_simd_float32_round_neg_inf_bytecode" "caml_simd_float32_round_neg_inf"
  [@@noalloc] [@@unboxed] [@@builtin]

  external round_up
    :  t @ local
    -> t
    @@ portable
    = "caml_simd_float32_round_pos_inf_bytecode" "caml_simd_float32_round_pos_inf"
  [@@noalloc] [@@unboxed] [@@builtin]

  external round_towards_zero
    :  t @ local
    -> t
    @@ portable
    = "caml_simd_float32_round_towards_zero_bytecode"
      "caml_simd_float32_round_towards_zero"
  [@@noalloc] [@@unboxed] [@@builtin]
end

let round_down = Rounding_intrinsics.round_down
let round_up = Rounding_intrinsics.round_up
let round_towards_zero = Rounding_intrinsics.round_towards_zero

(* see the comment above [round_nearest_lb] and [round_nearest_ub] for an explanation *)
let[@ocaml.inline] round_nearest_inline t =
  if t > round_nearest_lb && t < round_nearest_ub
  then round_down (add_half_for_round_nearest t) [@nontail]
  else box t
;;

let round_nearest t = (round_nearest_inline [@ocaml.inlined always]) t

(* The performance of the "exn" rounding functions is important, so they are written out
   separately, and tuned individually. (We could have the option versions call the "exn"
   versions, but that imposes arguably gratuitous overhead---especially in the case where
   the capture of backtraces is enabled upon "with"---and that seems not worth it when
   compared to the relatively small amount of code duplication.) *)

let iround_up t =
  if t > 0.0s
  then (
    let t' = round_up t in
    if t' <= iround_ubound then Some (to_int_unchecked t') else None)
  else if t >= iround_lbound
  then Some (to_int_unchecked t)
  else None
;;

let[@ocaml.inline always] iround_up_exn t =
  if t > 0.0s
  then (
    let t' = round_up t in
    if t' <= iround_ubound
    then to_int_unchecked t'
    else invalid_argf "Float32.iround_up_exn: argument (%f) is too large" (to_float t) ())
  else if t >= iround_lbound
  then to_int_unchecked t
  else
    invalid_argf
      "Float32.iround_up_exn: argument (%f) is too small or NaN"
      (to_float t)
      ()
;;

let iround_down t =
  if t >= 0.0s
  then if t <= iround_ubound then Some (to_int_unchecked t) else None
  else (
    let t' = round_down t in
    if t' >= iround_lbound then Some (to_int_unchecked t') else None)
;;

let[@ocaml.inline always] iround_down_exn t =
  if t >= 0.0s
  then
    if t <= iround_ubound
    then to_int_unchecked t
    else
      invalid_argf "Float32.iround_down_exn: argument (%f) is too large" (to_float t) ()
  else (
    let t' = round_down t in
    if t' >= iround_lbound
    then to_int_unchecked t'
    else
      invalid_argf
        "Float32.iround_down_exn: argument (%f) is too small or NaN"
        (to_float t)
        ())
;;

let iround_towards_zero t =
  if t >= iround_lbound && t <= iround_ubound then Some (to_int_unchecked t) else None
;;

let[@ocaml.inline always] iround_towards_zero_exn t =
  if t >= iround_lbound && t <= iround_ubound
  then to_int_unchecked t
  else
    invalid_argf
      "Float32.iround_towards_zero_exn: argument (%f) is out of range or NaN"
      (to_float t)
      ()
;;

let iround_nearest t =
  if t >= 0.s
  then
    if t < round_nearest_ub
    then Some (to_int_unchecked (add_half_for_round_nearest t))
    else if t <= iround_ubound
    then Some (to_int_unchecked t)
    else None
  else if t > round_nearest_lb
  then Some (to_int_unchecked (round_down (add t 0.5s)))
  else if t >= iround_lbound
  then Some (to_int_unchecked t)
  else None
;;

let[@ocaml.inline always] iround_nearest_exn t =
  if t >= 0.s
  then
    if t < round_nearest_ub
    then to_int_unchecked (add_half_for_round_nearest t)
    else if t <= iround_ubound
    then to_int_unchecked t
    else
      invalid_argf
        "Float32.iround_nearest_exn: argument (%f) is too large"
        (to_float t)
        ()
  else if t > round_nearest_lb
  then to_int_unchecked (round_down (add t 0.5s))
  else if t >= iround_lbound
  then to_int_unchecked t
  else
    invalid_argf
      "Float32.iround_nearest_exn: argument (%f) is too small or NaN"
      (to_float t)
      ()
;;

(* The following [iround_exn] and [iround] functions are slower than the ones above. Their
   equivalence to those functions is tested in the unit tests below. *)

let[@inline] iround_exn ?(dir = `Nearest) t =
  match dir with
  | `Zero -> iround_towards_zero_exn t
  | `Nearest -> iround_nearest_exn t
  | `Up -> iround_up_exn t
  | `Down -> iround_down_exn t
;;

let iround ?(dir = `Nearest) t =
  try Some (iround_exn ~dir t) with
  | _ -> None
;;

[%%template
[@@@mode.default m = (global, local)]

let min_inan x y = if is_nan y then x else if is_nan x then y else if x > y then x else y
let max_inan x y = if is_nan y then x else if is_nan x then y else if x > y then x else y]

module Parts : sig
  type t

  val fractional : t -> float32 @@ portable
  val integral : t -> float32 @@ portable
end = struct
  type t = float32 * float32

  let fractional t = fst t
  let integral t = snd t
end

external modf : local_ t -> Parts.t @@ portable = "caml_modf_float32"

let round_nearest_half_to_even' t =
  if t <= round_nearest_lb || t >= round_nearest_ub
  then box t
  else (
    let floor = round_down t in
    (* [ceil_or_succ = if t is an integer then t +. 1.s else ceil t]. Faster than [ceil]. *)
    let ceil_or_succ = add floor 1.s in
    let diff_floor = sub t floor in
    let diff_ceil = sub ceil_or_succ t in
    if diff_floor < diff_ceil
    then floor
    else if diff_floor > diff_ceil
    then ceil_or_succ
    else if (* exact tie, pick the even *)
            mod_float floor 2.s = 0.s
    then floor
    else ceil_or_succ)
;;

let iround_nearest_half_to_even' t =
  (* The result is unspecified if t is out of range *)
  round_nearest_half_to_even' t |> to_int64
;;

let round_nearest_half_to_even =
  (* We only assume the current rounding mode is half-to-even in native code. *)
  match Sys.backend_type with
  | Native -> Rounding_intrinsics.round_current
  | _ -> round_nearest_half_to_even'
;;

let iround_nearest_half_to_even =
  (* We only assume the current rounding mode is half-to-even in native code. *)
  match Sys.backend_type with
  | Native -> Rounding_intrinsics.iround_current
  | _ -> iround_nearest_half_to_even'
;;

let round ?(dir = `Nearest) t =
  match dir with
  | `Nearest -> round_nearest t
  | `Down -> round_down t
  | `Up -> round_up t
  | `Zero -> round_towards_zero t
;;

external classify_float
  :  (t[@unboxed] [@local_opt])
  -> Stdlib.fpclass
  @@ portable
  = "caml_classify_float32_bytecode" "caml_classify_float32"
[@@noalloc]

let classify t =
  let module C = Float.Class in
  match classify_float t with
  | FP_normal -> C.Normal
  | FP_subnormal -> C.Subnormal
  | FP_zero -> C.Zero
  | FP_infinite -> C.Infinite
  | FP_nan -> C.Nan
;;

let insert_underscores ?(delimiter = '_') ?(strip_zero = false) string =
  match String.lsplit2 string ~on:'.' with
  | None -> Base.Int_conversions.insert_delimiter string ~delimiter
  | Some (left, right) ->
    let left = Base.Int_conversions.insert_delimiter left ~delimiter in
    let right =
      if strip_zero then String.rstrip right ~drop:(fun c -> Char.( = ) c '0') else right
    in
    (match right with
     | "" -> left
     | _ -> left ^ "." ^ right)
;;

let to_string_hum ?delimiter ?(decimals = 3) ?strip_zero ?(explicit_plus = false) f =
  if Int.( < ) decimals 0
  then invalid_argf "to_string_hum: invalid argument ~decimals=%d" decimals ();
  match classify f with
  | Infinite -> if f > 0.s then "inf" else "-inf"
  | Nan -> "nan"
  | Normal | Subnormal | Zero ->
    let s =
      if explicit_plus
      then sprintf "%+.*f" decimals (to_float f)
      else sprintf "%.*f" decimals (to_float f)
    in
    insert_underscores s ?delimiter ?strip_zero
;;

let sexp_of_t t =
  let sexp = sexp_of_t t in
  match Dynamic.get Sexp.of_float_style with
  | `No_underscores -> sexp
  | `Underscores ->
    (match sexp with
     | List _ ->
       raise_s
         (Sexp.message
            "[sexp_of_float32] produced strange sexp"
            [ "sexp", Sexp.sexp_of_t sexp ])
     | Atom string ->
       if String.contains string 'E' then sexp else Atom (insert_underscores string))
;;

let to_padded_compact_string_custom t ?(prefix = "") ~kilo ~mega ~giga ~tera ?peta () =
  (* Round a ratio toward the nearest integer, resolving ties toward the nearest even
     number. For sane inputs (in particular, when [denominator] is an integer and
     [abs numerator < 2e52]) this should be accurate. Otherwise, the result might be a
     little bit off, but we don't really use that case. *)
  let iround_ratio_exn ~numerator ~denominator =
    let k = round_down (div numerator denominator) in
    (* if [abs k < 2e53], then both [k] and [k +. 1.] are accurately represented, and in
       particular [k +. 1. > k]. If [denominator] is also an integer, and
       [abs (denominator *. (k +. 1)) < 2e53] (and in some other cases, too), then [lower]
       and [higher] are actually both accurate. Since (roughly)
       [numerator = denominator *. k] then for [abs numerator < 2e52] we should be fine. *)
    let lower = mul denominator k in
    let higher = mul denominator (add k 1.s) in
    (* Subtracting numbers within a factor of two from each other is accurate. So either
       the two subtractions below are accurate, or k = 0, or k = -1. In case of a tie,
       round to even. *)
    let diff_right = sub higher numerator in
    let diff_left = sub numerator lower in
    let k = iround_nearest_exn k in
    if diff_right < diff_left
    then k + 1
    else if diff_right > diff_left
    then k
    else if (* a tie *) Int.O.(k land 1 = 0)
    then k
    else k + 1
  in
  match classify t with
  | Infinite -> if t < 0.0s then "-inf  " else "inf  "
  | Nan -> "nan  "
  | Subnormal | Normal | Zero ->
    let go t =
      let conv_one t =
        assert (0.s <= t && t < 999.95s);
        let x = prefix ^ Util.format_float32 "%.1f" t in
        (* Fix the ".0" suffix *)
        if String.is_suffix x ~suffix:".0"
        then (
          let x = Bytes.of_string x in
          let n = Bytes.length x in
          Bytes.set x (n - 1) ' ';
          Bytes.set x (n - 2) ' ';
          Bytes.unsafe_to_string ~no_mutation_while_string_reachable:x)
        else x
      in
      let conv mag t denominator =
        assert (
          (denominator = 100.s && t >= 999.95s)
          || (denominator >= 100_000.s && t >= round_nearest (mul denominator 9.999_5s)));
        assert (t < round_nearest (mul denominator 9_999.5s));
        let i, d =
          let k = iround_ratio_exn ~numerator:t ~denominator in
          (* [mod] is okay here because we know i >= 0. *)
          k / 10, Stdlib.( mod ) k 10
        in
        let open Int in
        assert (0 <= i && i < 1000);
        assert (0 <= d && d < 10);
        if d = 0
        then sprintf "%s%d%s " prefix i mag
        else sprintf "%s%d%s%d" prefix i mag d
      in
      (* While the standard metric prefixes (e.g. capital "M" rather than "m", [1]) are
         nominally more correct, this hinders readability in our case. E.g., 10G6 and 1066
         look too similar. That's an extreme example, but in general k,m,g,t,p probably
         stand out better than K,M,G,T,P when interspersed with digits.

         [1] http://en.wikipedia.org/wiki/Metric_prefix *)
      (* The trick here is that:
         - the first boundary (999.95) as a float is slightly over-represented (so it is
           better approximated as "1k" than as "999.9"),
         - the other boundaries are accurately represented, because they are integers.
           That's why the strict equalities below do exactly what we want. *)
      if t < 999.95E0s
      then conv_one t
      else if t < 999.95E3s
      then conv kilo t 100.s
      else if t < 999.95E6s
      then conv mega t 100_000.s
      else if t < 999.95E9s
      then conv giga t 100_000_000.s
      else if t < 999.95E12s
      then conv tera t 100_000_000_000.s
      else (
        match peta with
        | None -> sprintf "%s%.1e" prefix (to_float t)
        | Some peta ->
          if t < 999.95E15s
          then conv peta t 100_000_000_000_000.s
          else sprintf "%s%.1e" prefix (to_float t))
    in
    if t >= 0.s then go t else "-" ^ go (neg t)
;;

let to_padded_compact_string t =
  to_padded_compact_string_custom t ~kilo:"k" ~mega:"m" ~giga:"g" ~tera:"t" ~peta:"p" ()
;;

(* Performance note: Initializing the accumulator to 1 results in one extra multiply;
   e.g., to compute x ** 4, we in principle only need 2 multiplies, but this function will
   have 3 multiplies. However, attempts to avoid this (like decrementing n and
   initializing accum to be x, or handling small exponents as a special case) have not
   yielded anything that is a net improvement.
*)
let int_pow x n =
  let open Int in
  if n = 0
  then 1.s
  else (
    (* Using [box x] on the following line convinces the compiler to avoid a certain
       boxing (that would result in allocation in each iteration). Soon, the compiler
       shouldn't need this "hint" to avoid the boxing. *)
    let x = ref (box x) in
    let n = ref n in
    let accum = ref 1.s in
    if !n < 0
    then (
      (* x ** n = (1/x) ** -n *)
      x := div 1.s !x;
      n := ~- (!n);
      if !n < 0
      then (
        (* n must have been min_int, so it is now so big that it has wrapped around. We
           decrement it so that it looks positive again, but accordingly have to put an
           extra factor of x in the accumulator.
        *)
        accum := !x;
        decr n));
    (* Letting [a] denote (the original value of) [x ** n], we maintain the invariant that
       [(x ** n) *. accum = a]. *)
    while !n > 1 do
      if !n land 1 <> 0 then accum := mul !x !accum;
      x := mul !x !x;
      n := !n lsr 1
    done;
    (* n is necessarily 1 at this point, so there is one additional multiplication by x. *)
    mul !x !accum)
;;

let between t ~low ~high = low <= t && t <= high

let clamp_unchecked ~to_clamp_maybe_nan ~min_which_is_not_nan ~max_which_is_not_nan =
  (* We want to propagate nans; this means we have to use them as the _second_ argument. *)
  let t_maybe_nan = max min_which_is_not_nan to_clamp_maybe_nan in
  min max_which_is_not_nan t_maybe_nan
;;

let clamp_exn t ~min ~max =
  (* Also fails if [min] or [max] is nan *)
  assert (min <= max);
  clamp_unchecked
    ~to_clamp_maybe_nan:t
    ~min_which_is_not_nan:min
    ~max_which_is_not_nan:max
;;

let clamp t ~min ~max =
  (* Also fails if [min] or [max] is nan *)
  if min <= max
  then
    Ok
      (clamp_unchecked
         ~to_clamp_maybe_nan:t
         ~min_which_is_not_nan:min
         ~max_which_is_not_nan:max)
  else
    Or_error.error_s
      (Sexp.message
         "clamp requires [min <= max]"
         [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
;;

external ( + ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%addfloat32"
external ( - ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%subfloat32"
external ( * ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%mulfloat32"
external ( / ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%divfloat32"
external ( ~- ) : local_ t -> (t[@local_opt]) @@ portable = "%negfloat32"

let ( % ) x y =
  if y < 0.s
  then
    invalid_argf
      "%.9g %% %.9g in float32.ml: modulus should be positive"
      (to_float x)
      (to_float y)
      ()
  else (
    let m = mod_float x y in
    if m < 0.s then m + y else m)
;;

let ( ** ) = pow

let[@inline] sign_exn t : Sign.t =
  if t > 0.s
  then Pos
  else if t < 0.s
  then Neg
  else if t = 0.s
  then Zero
  else
    Error.raise_s (Sexp.message "Float32.sign_exn of NAN" [ "", sexp_of_t (globalize t) ])
;;

let sign_or_nan t : Sign_or_nan.t =
  if t > 0.s then Pos else if t < 0.s then Neg else if t = 0.s then Zero else Nan
;;

let exponent_bits = 8
let mantissa_bits = 23
let exponent_mask32 = Int32.(shift_left one exponent_bits - one)
let exponent_mask = Int.of_int32_trunc exponent_mask32
let mantissa_mask32 = Int32.(shift_left one mantissa_bits - one)
let mantissa_mask = Int.of_int32_trunc mantissa_mask32

let ieee_negative t =
  let bits = to_bits t in
  Int32.O.(bits < zero)
;;

let ieee_exponent t =
  let bits = to_bits t in
  Int32.to_int_trunc Int32.O.((bits lsr mantissa_bits) land exponent_mask32)
;;

let ieee_mantissa t =
  let bits = to_bits t in
  (* This is safe because mantissa_mask32 < Int.max_value, even on 32-bit platforms. *)
  Int.of_int32_trunc Int32.O.(bits land mantissa_mask32)
;;

let create_ieee_exn ~negative ~exponent ~mantissa =
  if Int.(bit_and exponent exponent_mask <> exponent)
  then failwithf "exponent %d out of range [0, %d]" exponent exponent_mask ()
  else if Int.(bit_and mantissa mantissa_mask <> mantissa)
  then
    failwithf
      "mantissa %s out of range [0, %s]"
      (Int.to_string mantissa)
      (Int.to_string mantissa_mask)
      ()
  else (
    let sign_bits = if negative then Int32.min_value else Int32.zero in
    let expt_bits = Int32.shift_left (Int32.of_int_trunc exponent) mantissa_bits in
    let mant_bits = Int.to_int32_trunc mantissa in
    let bits = Int32.(bit_or sign_bits (bit_or expt_bits mant_bits)) in
    of_bits bits)
;;

let create_ieee ~negative ~exponent ~mantissa =
  Or_error.try_with (fun () -> create_ieee_exn ~negative ~exponent ~mantissa)
;;

module Terse = struct
  type nonrec t = t

  let t_of_sexp = t_of_sexp
  let sexp_of_t x = Sexp.Atom (to_string x)
  let to_string = to_string
  let of_string = of_string
end

include%template Comparable.With_zero [@modality portable] (struct
    include T

    let zero = zero
  end)

(* These are partly here as a performance hack to avoid some boxing we're getting with the
   versions we get from [With_zero]. They also make [Float32.is_negative nan] and
   [Float32.is_non_positive nan] return [false]; the versions we get from [With_zero]
   return [true]. *)
let is_positive t = t > 0.s
let is_non_negative t = t >= 0.s
let is_negative t = t < 0.s
let is_non_positive t = t <= 0.s

include%template Pretty_printer.Register [@modality portable] (struct
    include T

    let module_name = "Float32"
    let to_string = to_string
  end)

module O = struct
  external ( + ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%addfloat32"
  external ( - ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%subfloat32"
  external ( * ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%mulfloat32"
  external ( / ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%divfloat32"
  external ( ~- ) : local_ t -> (t[@local_opt]) @@ portable = "%negfloat32"

  let ( % ) = ( % )
  let ( ** ) = ( ** )

  include Util.Float32_replace_polymorphic_compare

  external abs : local_ t -> (t[@local_opt]) @@ portable = "%absfloat32"
  external neg : local_ t -> (t[@local_opt]) @@ portable = "%negfloat32"

  let zero = zero

  external of_int : int -> (t[@local_opt]) @@ portable = "%float32ofint"
  external of_float : local_ float -> (t[@local_opt]) @@ portable = "%float32offloat"
end

module O_dot = struct
  external ( +. ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%addfloat32"
  external ( -. ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%subfloat32"
  external ( *. ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%mulfloat32"
  external ( /. ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%divfloat32"
  external ( ~-. ) : local_ t -> (t[@local_opt]) @@ portable = "%negfloat32"

  let ( %. ) = ( % )
  let ( **. ) = ( ** )
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after including functor
   application that could shadow its definitions. This is here so that efficient versions
   of the comparison functions are exported by this module. *)
include Util.Float32_replace_polymorphic_compare

(* These functions specifically replace defaults in replace_polymorphic_compare.

   The desired behavior here is to propagate a nan if either argument is nan. Because the
   first comparison will always return false if either argument is nan, it suffices to
   check if x is nan. Then, when x is nan or both x and y are nan, we return x = nan; and
   when y is nan but not x, we return y = nan.
*)
let min (x : t) y = if x < y || is_nan x then x else y
let max (x : t) y = if x > y || is_nan x then x else y

include%template struct
  open Base_quickcheck

  let quickcheck_generator = (Generator.map [@mode portable]) Generator.float ~f:of_float
  let quickcheck_observer = (Observer.unmap [@mode portable]) Observer.float ~f:to_float

  let quickcheck_shrinker =
    (Shrinker.map [@mode portable]) Shrinker.float ~f:of_float ~f_inverse:to_float
  ;;
end

module Private = struct
  let box = box
  let lower_bound_for_int = lower_bound_for_int
  let upper_bound_for_int = upper_bound_for_int
  let specialized_hash = Util.hash_float32
  let one_ulp_less_than_half = one_ulp_less_than_half
  let exponent_bits = exponent_bits
  let mantissa_bits = mantissa_bits
  let exponent_mask32 = exponent_mask32
  let exponent_mask = exponent_mask
  let mantissa_mask32 = mantissa_mask32
  let mantissa_mask = mantissa_mask
end

module Bytes = struct
  external get : bytes -> pos:int -> float32 @@ portable = "%caml_bytes_getf32"
  external unsafe_get : bytes -> pos:int -> float32 @@ portable = "%caml_bytes_getf32u"
  external set : bytes -> pos:int -> float32 -> unit @@ portable = "%caml_bytes_setf32"

  external unsafe_set
    :  bytes
    -> pos:int
    -> float32
    -> unit
    @@ portable
    = "%caml_bytes_setf32u"
end

module String = struct
  external get : string -> pos:int -> float32 @@ portable = "%caml_string_getf32"
  external unsafe_get : string -> pos:int -> float32 @@ portable = "%caml_string_getf32u"
end

module Bigstring = struct
  open Bigarray

  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  external get : t @ shared -> pos:int -> float32 @@ portable = "%caml_bigstring_getf32"

  external unsafe_get
    :  t @ shared
    -> pos:int
    -> float32
    @@ portable
    = "%caml_bigstring_getf32u"

  external set : t -> pos:int -> float32 -> unit @@ portable = "%caml_bigstring_setf32"

  external unsafe_set
    :  t
    -> pos:int
    -> float32
    -> unit
    @@ portable
    = "%caml_bigstring_setf32u"
end

module Bigarray = struct
  open Bigarray

  module Array1 = struct
    external get
      :  ('a, float32_elt, 'c) Array1.t
      -> int
      -> float32
      @@ portable
      = "%caml_ba_float32_ref_1"

    external set
      :  ('a, float32_elt, 'c) Array1.t
      -> int
      -> float32
      -> unit
      @@ portable
      = "%caml_ba_float32_set_1"

    external unsafe_get
      :  ('a, float32_elt, 'c) Array1.t
      -> int
      -> float32
      @@ portable
      = "%caml_ba_float32_unsafe_ref_1"

    external unsafe_set
      :  ('a, float32_elt, 'c) Array1.t
      -> int
      -> float32
      -> unit
      @@ portable
      = "%caml_ba_float32_unsafe_set_1"
  end

  module Array2 = struct
    external get
      :  ('a, float32_elt, 'c) Array2.t
      -> int
      -> int
      -> float32
      @@ portable
      = "%caml_ba_float32_ref_2"

    external set
      :  ('a, float32_elt, 'c) Array2.t
      -> int
      -> int
      -> float32
      -> unit
      @@ portable
      = "%caml_ba_float32_set_2"

    external unsafe_get
      :  ('a, float32_elt, 'c) Array2.t
      -> int
      -> int
      -> float32
      @@ portable
      = "%caml_ba_float32_unsafe_ref_2"

    external unsafe_set
      :  ('a, float32_elt, 'c) Array2.t
      -> int
      -> int
      -> float32
      -> unit
      @@ portable
      = "%caml_ba_float32_unsafe_set_2"
  end

  module Array3 = struct
    external get
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32
      @@ portable
      = "%caml_ba_float32_ref_3"

    external set
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32
      -> unit
      @@ portable
      = "%caml_ba_float32_set_3"

    external unsafe_get
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32
      @@ portable
      = "%caml_ba_float32_unsafe_ref_3"

    external unsafe_set
      :  ('a, float32_elt, 'c) Array3.t
      -> int
      -> int
      -> int
      -> float32
      -> unit
      @@ portable
      = "%caml_ba_float32_unsafe_set_3"
  end
end
