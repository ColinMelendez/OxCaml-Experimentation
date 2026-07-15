open Core
open! Int.Replace_polymorphic_compare

module Stable = struct
  open Core.Core_stable

  module V1 = struct
    (* [Float.compare] compares different nans as equal. We have tests to ensure this. *)
    type t = float
    [@@deriving
      bin_io ~localize, compare ~localize, globalize, sexp, stable_witness, typerep]

    (* We use [Float.abs (0. /. 0.)] for [none] instead of [Float.nan] because the
       compiler currently does not treat Float.nan as a known constant at compile time. In
       some cases, this can lead to unneccessary allocations. The [Float.abs] call ensures
       that when printing the value of [none], we get [nan] and not [-nan] *)
    let none = Core.Float.abs (0. /. 0.)
    let[@zero_alloc] is_none t = Core.Float.is_nan t
    let[@zero_alloc] is_some t = not (is_none t)
    let[@zero_alloc] some_is_representable t = is_some t

    [%%template
    [@@@mode.default m = (global, local)]

    let[@zero_alloc] some (v @ m) =
      assert (is_some v);
      v
    ;;

    let[@zero_alloc] unchecked_value (x : t @ m) = x
    let to_option (t @ m) = if is_none t then None else Some t [@exclave_if_local m]

    let[@zero_alloc] of_option = function
      | None -> none
      | Some v -> (some [@mode m]) v [@exclave_if_local m]
    ;;

    let to_or_null (t @ m) = if is_none t then Null else This t [@exclave_if_local m]

    let[@zero_alloc] of_or_null = function
      | Null -> none
      | This v -> (some [@mode m]) v [@exclave_if_local m]
    ;;]

    let%template[@alloc a @ m = (heap_global, stack_local)] sexp_of_t (t @ m) =
      ((to_or_null [@mode m]) t |> ([%sexp_of: float or_null] [@alloc a]))
      [@exclave_if_stack a]
    ;;

    let t_of_sexp s = [%of_sexp: float option] s |> of_option
    let t_sexp_grammar = [%sexp_grammar: float option] |> Sexplib.Sexp_grammar.coerce
    let equal = [%compare.equal: float]
    let%template equal = ([%compare.equal: float] [@mode local]) [@@mode m = local]
  end
end

include Stable.V1

include%template Comparable.Make_binable [@mode local] [@modality portable] (Stable.V1)

module Optional_syntax = struct
  module Optional_syntax = struct
    let[@zero_alloc] is_none t = is_none t

    let%template[@zero_alloc] unsafe_value t =
      (unchecked_value [@mode m]) t [@exclave_if_local m]
    [@@mode m = (global, local)]
    ;;
  end
end

module Array = struct
  include Float_array

  let view_to_float_array_none_as_nan t = t
  let view_of_float_array_nan_as_none t = t
end

open Optional_syntax

[%%template
[@@@alloc a @ m = (heap_global, stack_local)]

let[@zero_alloc] value (t @ m) ~default =
  (match%optional (t : _ @ m) with
   | None -> default
   | Some t -> t)
  [@exclave_if_stack a]
[@@mode m]
;;

let[@zero_alloc] value_exn (t @ m) =
  (match%optional (t : _ @ m) with
   | None -> raise_s [%message "None"]
   | Some t -> t)
  [@exclave_if_stack a]
[@@mode m]
;;]

let value_map t ~f ~default =
  match%optional t with
  | None -> default
  | Some t -> f t
;;

let zero = Float.zero

[%%template
[@@@mode.default m = (global, local)]

let[@inline] of_float_nan_as_none (x : float @ m) = x [@exclave_if_local m]
let[@inline] to_float_none_as_nan (x : t @ m) = x [@exclave_if_local m]]

module Infix = struct
  (* [nan] behaves essentially correctly for these functions. Note that eg
     [Float.is_nan (0. /. 0.)] *)
  let ( + ) = Float.( + )
  let ( - ) = Float.( - )
  let ( * ) = Float.( * )
  let ( / ) = Float.( / )

  (* We need to check both operands, because: Float.nan ** 0. = 1.
     1. ** Float.nan = 1. *)
  let ( ** ) t1 t2 = if is_none t1 || is_none t2 then none else Float.( ** ) t1 t2

  (* Although [Float.(nan <> nan)] is true, we define [Packed_float_option.(none = none)]
     to be true (per [equal] above). *)
  let ( = ) = equal
  let ( <> ) t1 t2 = not (t1 = t2)
end

module Ieee_nan = struct
  module Infix = struct
    (* These functions return false if either operand is [nan]. *)
    let ( < ) = [%eta2 Float.( < )]
    let ( <= ) = [%eta2 Float.( <= )]
    let ( > ) = [%eta2 Float.( > )]
    let ( >= ) = [%eta2 Float.( >= )]
  end

  include Infix

  let max = Float.max
  let clamp_exn = Float.clamp_exn
end

module Local = struct
  let globalize = globalize_float
  let of_float_nan_as_none = (of_float_nan_as_none [@mode local])
  let to_float_none_as_nan = (to_float_none_as_nan [@mode local])

  (* Evaluatues [none = none] to true *)
  let equal = ([%compare.equal: float] [@mode local])

  module Infix = struct
    let ( + ) = Stdlib.( +. )
    let ( - ) = Stdlib.( -. )
    let ( * ) = Stdlib.( *. )
    let ( / ) = Stdlib.( /. )
    let ( = ) = equal
    let ( <> ) t1 t2 = not (t1 = t2)
  end

  include Infix

  module Optional_syntax = struct
    module S = Optional_syntax

    module Optional_syntax = struct
      let[@zero_alloc] is_none (local_ (x : t)) = Stdlib.( <> ) x x
      let[@zero_alloc] unsafe_value (local_ (x : t)) = x
    end

    module%test _ = struct
      module L = Optional_syntax

      let%test_unit "is_none" =
        Quickcheck.test
          ~trials:1_000
          ~examples:[ Float.infinity; Float.nan; Float.neg_infinity ]
          ~sexp_of:Float.sexp_of_t
          Float.quickcheck_generator
          ~f:(fun f -> assert (Bool.equal (L.is_none f) (S.is_none f)))
      ;;

      let%test_unit "unsafe_value not nan" =
        (* by definition nan doesn't equal itself so we cannot test it here *)
        Quickcheck.test
          ~trials:1_000
          ~examples:[ Float.infinity; Float.neg_infinity ]
          ~sexp_of:Float.sexp_of_t
          (Quickcheck.Generator.filter
             Float.quickcheck_generator
             ~f:(Fn.non Float.is_nan))
          ~f:(fun f -> assert (Stdlib.( = ) (L.unsafe_value f) (S.unsafe_value f)))
      ;;

      let%test ("unsafe_value nan" [@tags "no-js"]) =
        L.unsafe_value none |> globalize |> Float.is_nan
      ;;
    end
  end

  let is_none = Optional_syntax.Optional_syntax.is_none
end

(* Due to the arithmetic operations provided above, it's possible to get a different
   representation of [None]. For hashing, we make sure to use a canonical [None], although
   it is not necessary at the time, as [Float.hash] returns the same hash for all
   representations of [NaN].

   The performance of keeping vs. removing this branching stays the same, proved by
   benchmarking. *)
let none_hash = Float.hash none

let hash t =
  match%optional t with
  | None -> none_hash
  | Some t -> Float.hash t
;;

let hash_fold_t h t = hash_fold_int h (hash t)
let abs = Float.abs
let neg = Float.neg
let log = Float.log
let log10 = Float.log10
let log1p = Float.log1p
let sqrt = Float.sqrt
let square = Float.square
let exp = Float.exp
let is_inf = Float.is_inf
let is_positive = Float.is_positive
let is_non_positive = Float.is_non_positive
let is_negative = Float.is_negative
let is_non_negative = Float.is_non_negative
let is_integer = Float.is_integer
let is_finite = Float.is_finite
let min = Float.min
let inv t = Infix.( / ) (of_float_nan_as_none 1.) t
let scale t flt = Infix.( * ) t (of_float_nan_as_none flt)

let first_some x y =
  match%optional x with
  | Some _ -> x
  | None -> y
;;

let merge x y ~f =
  match%optional x, y with
  | None, None -> none
  | Some x, None -> x
  | None, Some y -> y
  | Some x, Some y -> f x y
;;

let validate ~none:none_check ~some:some_check t =
  match%optional t with
  | None -> none_check ()
  | Some value -> some_check value
;;

let validate_option_bound ~may_be_none ?min:(lower = Unbounded) ?max:(upper = Unbounded) t
  =
  match%optional t with
  | None ->
    if may_be_none then Validate.get_pass () else Validate.fail "value may not be none"
  | Some value ->
    Validate.bounded ~lower ~upper ~compare:Float.compare ~name:Float.to_string value
;;

let to_string t = Sexp.to_string (sexp_of_t t)

include Infix

module Unboxed = struct
  include Float_u.Option

  module O = struct
    include O

    let unbox x = Float_u.of_float x |> of_float_nan_as_none

    let%template[@mode m = (global, local)] box (t @ m) =
      (t |> to_float_none_as_nan |> Float_u.to_float)
      [@exclave_if_local m ~reasons:[ May_return_local ]]
    ;;
  end

  include O
end

let[@inline] divide_if_denominator_nonzero_else
  ~(numerator : t)
  ~(denominator : t)
  ~(else_ : t)
  =
  Unboxed.divide_if_denominator_nonzero_else
    ~numerator:(Unboxed.unbox numerator)
    ~denominator:(Unboxed.unbox denominator)
    ~else_:(Unboxed.unbox else_)
  |> Unboxed.box
;;
