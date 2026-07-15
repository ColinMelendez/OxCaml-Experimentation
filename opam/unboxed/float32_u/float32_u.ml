(*_ Create an alias to the built-in [array], whose parameter has layout [any]. We need
    this alias to define the [Array] submodule below. [Base.Array.t] and [Stdlib.Array.t]
    are unsuitable as they re-export [array] with a parameter of layout [value].
*)
type ('a : any mod separable) builtin_array = 'a array

open Base
open Ocaml_intrinsics_kernel.Conditional
module F = Float32
module Boxed = Float32

external to_float32 : float32# -> (float32[@local_opt]) @@ portable = "%box_float32"
external of_float32 : (float32[@local_opt]) -> float32# @@ portable = "%unbox_float32"

module Stable = struct
  module V1 = struct
    open struct
      module F = F.Stable.V1
    end

    type t = float32#

    let globalize (local_ t) : t = t

    let%template[@alloc a = (heap, stack)] [@inline] sexp_of_t t : Base.Sexp.t =
      (F.sexp_of_t [@alloc a] [@inlined hint]) (to_float32 t) [@exclave_if_stack a]
    ;;

    let[@inline] t_of_sexp sexp : t = of_float32 ((F.t_of_sexp [@inlined hint]) sexp)
    let bin_shape_t = Bin_prot_unboxed_numbers.Float32_u.bin_shape_t

    [%%template
    [@@@mode.default m = (global, local)]

    let[@inline always] bin_size_t t =
      (Bin_prot_unboxed_numbers.Float32_u.bin_size_t [@mode m] [@inlined]) t
    ;;

    let[@inline always] bin_write_t buf ~pos t =
      (Bin_prot_unboxed_numbers.Float32_u.bin_write_t [@mode m] [@inlined]) buf ~pos t
    ;;]

    let[@inline always] bin_read_t buf ~pos_ref =
      (Bin_prot_unboxed_numbers.Float32_u.bin_read_t [@inlined]) buf ~pos_ref
    ;;

    let __bin_read_t__ = Bin_prot_unboxed_numbers.Float32_u.__bin_read_t__
    let bin_writer_t = Bin_prot_unboxed_numbers.Float32_u.bin_writer_t
    let bin_reader_t = Bin_prot_unboxed_numbers.Float32_u.bin_reader_t
    let bin_t = Bin_prot_unboxed_numbers.Float32_u.bin_t
    let[@inline] to_string t : string = (F.to_string [@inlined hint]) (to_float32 t)
    let[@inline] of_string s : t = of_float32 ((F.of_string [@inlined hint]) s)

    [%%template
    [@@@mode.default m = (global, local)]

    let[@inline] [@zero_alloc] equal t1 t2 : bool =
      (F.equal [@mode m]) (to_float32 t1) (to_float32 t2)
    ;;

    let[@inline] [@zero_alloc] compare t1 t2 : int =
      (F.compare [@mode m]) (to_float32 t1) (to_float32 t2)
    ;;]

    let stable_witness = Ppx_stable_witness_runtime.Stable_witness.assert_stable
  end
end

include Stable.V1

let[@inline] ascending t1 t2 : int =
  (F.ascending [@inlined hint]) (to_float32 t1) (to_float32 t2)
;;

let[@inline] descending t1 t2 : int =
  (F.descending [@inlined hint]) (to_float32 t1) (to_float32 t2)
;;

let[@inline] between t ~low ~high : bool =
  (F.between [@inlined hint]) (to_float32 t) ~low:(to_float32 low) ~high:(to_float32 high)
;;

let[@inline] clamp_exn t ~min ~max : t =
  of_float32
    ((F.clamp_exn [@inlined hint])
       (to_float32 t)
       ~min:(to_float32 min)
       ~max:(to_float32 max))
;;

let[@inline] pp ppf t : unit = (F.pp [@inlined hint]) ppf (to_float32 t)
let[@inline] invariant t : unit = (F.invariant [@inlined hint]) (to_float32 t)
let nan : t = of_float32 F.nan
let infinity : t = of_float32 F.infinity
let neg_infinity : t = of_float32 F.neg_infinity
let max_value : t = of_float32 F.max_value
let min_value : t = of_float32 F.min_value
let zero : t = of_float32 F.zero
let one : t = of_float32 F.one
let minus_one : t = of_float32 F.minus_one
let pi : t = of_float32 F.pi
let sqrt_pi : t = of_float32 F.sqrt_pi
let sqrt_2pi : t = of_float32 F.sqrt_2pi
let euler_gamma_constant : t = of_float32 F.euler_gamma_constant
let epsilon_float : t = of_float32 F.epsilon_float
let max_finite_value : t = of_float32 F.max_finite_value
let min_positive_subnormal_value : t = of_float32 F.min_positive_subnormal_value
let min_positive_normal_value : t = of_float32 F.min_positive_normal_value

let[@inline] to_int32_preserve_order t : int32 option =
  (F.to_int32_preserve_order [@inlined hint]) (to_float32 t)
;;

let[@inline] to_int32_preserve_order_exn t : int32 =
  (F.to_int32_preserve_order_exn [@inlined hint]) (to_float32 t)
;;

let[@inline] of_int32_preserve_order i : t =
  of_float32 ((F.of_int32_preserve_order [@inlined hint]) i)
;;

external box_int64 : int64# -> (int64[@local_opt]) @@ portable = "%box_int64"
external unbox_int64 : (int64[@local_opt]) -> int64# @@ portable = "%unbox_int64"

let[@inline] one_ulp ud t : t = of_float32 ((F.one_ulp [@inlined hint]) ud (to_float32 t))
let[@inline] of_int i : t = of_float32 (F.of_int i)
let[@inline] to_int t : int = (F.to_int [@inlined hint]) (to_float32 t)
let[@inline] of_int64 i : t = of_float32 ((F.of_int64 [@inlined hint]) i)
let[@inline] to_int64 t : int64 = (F.to_int64 [@inlined hint]) (to_float32 t)
let[@inline] of_int64_u x : t = of_float32 ((F.of_int64 [@inlined hint]) (box_int64 x))

let[@inline] to_int64_u x : int64# =
  unbox_int64 ((F.to_int64 [@inlined hint]) (to_float32 x))
;;

let[@inline] of_float i : t = of_float32 (F.of_float i)
let[@inline] to_float t : float = F.to_float (to_float32 t)

external unbox_f64 : (float[@local_opt]) -> float# @@ portable = "%unbox_float"
external box_f64 : float# -> (float[@local_opt]) @@ portable = "%box_float"

let[@inline] [@zero_alloc opt] to_float_u t : float# =
  unbox_f64 ((to_float [@inlined hint]) t)
;;

let[@inline] of_float_u f : t = (of_float [@inlined hint]) (box_f64 f)
let[@inline] round ?dir t : t = of_float32 ((F.round [@inlined hint]) ?dir (to_float32 t))
let[@inline] iround ?dir t : int option = (F.iround [@inlined hint]) ?dir (to_float32 t)
let[@inline] iround_exn ?dir t : int = (F.iround_exn [@inlined hint]) ?dir (to_float32 t)

let[@inline] round_towards_zero t : t =
  of_float32 ((F.round_towards_zero [@inlined hint]) (to_float32 t))
;;

let[@inline] round_down t : t = of_float32 ((F.round_down [@inlined hint]) (to_float32 t))
let[@inline] round_up t : t = of_float32 ((F.round_up [@inlined hint]) (to_float32 t))

(** Rounds half integers up. *)
let[@inline] round_nearest t : t =
  of_float32 ((F.round_nearest [@inlined hint]) (to_float32 t))
;;

(** Rounds half integers to the even integer. *)
let[@inline] round_nearest_half_to_even t : t =
  of_float32 ((F.round_nearest_half_to_even [@inlined hint]) (to_float32 t))
;;

let[@inline] iround_towards_zero t : int option =
  (F.iround_towards_zero [@inlined hint]) (to_float32 t)
;;

let[@inline] iround_down t : int option = (F.iround_down [@inlined hint]) (to_float32 t)
let[@inline] iround_up t : int option = (F.iround_up [@inlined hint]) (to_float32 t)

let[@inline] iround_nearest t : int option =
  (F.iround_nearest [@inlined hint]) (to_float32 t)
;;

let[@inline] iround_towards_zero_exn t : int =
  (F.iround_towards_zero_exn [@inlined hint]) (to_float32 t)
;;

let[@inline] iround_down_exn t : int = (F.iround_down_exn [@inlined hint]) (to_float32 t)
let[@inline] iround_up_exn t : int = (F.iround_up_exn [@inlined hint]) (to_float32 t)

let[@inline] iround_nearest_exn t : int =
  (F.iround_nearest_exn [@inlined hint]) (to_float32 t)
;;

let[@inline] iround_nearest_half_to_even t : int64 =
  (F.iround_nearest_half_to_even [@inlined hint]) (to_float32 t)
;;

let iround_lbound = of_float32 F.iround_lbound
let iround_ubound = of_float32 F.iround_ubound
let[@inline] is_nan t : bool = (F.is_nan [@inlined hint]) (to_float32 t)
let[@inline] is_inf t : bool = (F.is_inf [@inlined hint]) (to_float32 t)
let[@inline] is_finite t : bool = (F.is_finite [@inlined hint]) (to_float32 t)
let[@inline] is_integer t : bool = (F.is_integer [@inlined hint]) (to_float32 t)

let[@inline] min_inan t1 t2 : t =
  of_float32 ((F.min_inan [@inlined hint]) (to_float32 t1) (to_float32 t2))
;;

let[@inline] max_inan t1 t2 : t =
  of_float32 ((F.max_inan [@inlined hint]) (to_float32 t1) (to_float32 t2))
;;

let[@inline] mod_float t1 t2 : t =
  of_float32 ((F.mod_float [@inlined hint]) (to_float32 t1) (to_float32 t2))
;;

let[@inline] add t1 t2 : t = of_float32 (F.add (to_float32 t1) (to_float32 t2))
let[@inline] sub t1 t2 : t = of_float32 (F.sub (to_float32 t1) (to_float32 t2))
let[@inline] mul t1 t2 : t = of_float32 (F.mul (to_float32 t1) (to_float32 t2))
let[@inline] div t1 t2 : t = of_float32 (F.div (to_float32 t1) (to_float32 t2))

module O = struct
  external box : float32# -> (float32[@local_opt]) @@ portable = "%box_float32"
  external unbox : (float32[@local_opt]) -> float32# @@ portable = "%unbox_float32"

  let[@inline] ( + ) t1 t2 : t = of_float32 (F.O.( + ) (to_float32 t1) (to_float32 t2))
  let[@inline] ( - ) t1 t2 : t = of_float32 (F.O.( - ) (to_float32 t1) (to_float32 t2))
  let[@inline] ( * ) t1 t2 : t = of_float32 (F.O.( * ) (to_float32 t1) (to_float32 t2))
  let[@inline] ( / ) t1 t2 : t = of_float32 (F.O.( / ) (to_float32 t1) (to_float32 t2))
  let[@inline] ( % ) t1 t2 : t = of_float32 (F.O.( % ) (to_float32 t1) (to_float32 t2))
  let[@inline] ( ** ) t1 t2 : t = of_float32 (F.O.( ** ) (to_float32 t1) (to_float32 t2))
  let[@inline] ( ~- ) t : t = of_float32 (F.O.( ~- ) (to_float32 t))
  let[@inline] ( >= ) t1 t2 : bool = F.O.( >= ) (to_float32 t1) (to_float32 t2)
  let[@inline] ( <= ) t1 t2 : bool = F.O.( <= ) (to_float32 t1) (to_float32 t2)
  let[@inline] ( = ) t1 t2 : bool = F.O.( = ) (to_float32 t1) (to_float32 t2)
  let[@inline] ( > ) t1 t2 : bool = F.O.( > ) (to_float32 t1) (to_float32 t2)
  let[@inline] ( < ) t1 t2 : bool = F.O.( < ) (to_float32 t1) (to_float32 t2)
  let[@inline] ( <> ) t1 t2 : bool = F.O.( <> ) (to_float32 t1) (to_float32 t2)
  let[@inline] abs t : t = of_float32 (F.O.abs (to_float32 t))
  let[@inline] neg t : t = of_float32 (F.O.neg (to_float32 t))
end

include O

module O_dot = struct
  let[@inline] ( +. ) t1 t2 : t =
    of_float32 (F.O_dot.( +. ) (to_float32 t1) (to_float32 t2))
  ;;

  let[@inline] ( -. ) t1 t2 : t =
    of_float32 (F.O_dot.( -. ) (to_float32 t1) (to_float32 t2))
  ;;

  let[@inline] ( *. ) t1 t2 : t =
    of_float32 (F.O_dot.( *. ) (to_float32 t1) (to_float32 t2))
  ;;

  let[@inline] ( /. ) t1 t2 : t =
    of_float32 (F.O_dot.( /. ) (to_float32 t1) (to_float32 t2))
  ;;

  let[@inline] ( %. ) t1 t2 : t =
    of_float32 (F.O_dot.( %. ) (to_float32 t1) (to_float32 t2))
  ;;

  let[@inline] ( **. ) t1 t2 : t =
    of_float32 (F.O_dot.( **. ) (to_float32 t1) (to_float32 t2))
  ;;

  let[@inline] ( ~-. ) t : t = of_float32 (F.O_dot.( ~-. ) (to_float32 t))
end

let[@inline] to_string_hum ?delimiter ?decimals ?strip_zero ?explicit_plus t : string =
  (F.to_string_hum [@inlined hint])
    ?delimiter
    ?decimals
    ?strip_zero
    ?explicit_plus
    (to_float32 t)
;;

let[@inline] to_padded_compact_string t : string =
  (F.to_padded_compact_string [@inlined hint]) (to_float32 t)
;;

let[@inline] to_padded_compact_string_custom t ?prefix ~kilo ~mega ~giga ~tera ?peta ()
  : string
  =
  (F.to_padded_compact_string_custom [@inlined hint])
    (to_float32 t)
    ?prefix
    ~kilo
    ~mega
    ~giga
    ~tera
    ?peta
    ()
;;

let[@inline] int_pow t i : t = of_float32 ((F.int_pow [@inlined hint]) (to_float32 t) i)
let[@inline] square t : t = of_float32 ((F.square [@inlined hint]) (to_float32 t))
let[@inline] ldexp t i : t = of_float32 (F.ldexp (to_float32 t) i)
let[@inline] log10 t : t = of_float32 (F.log10 (to_float32 t))
let[@inline] log2 t : t = of_float32 (F.log2 (to_float32 t))
let[@inline] expm1 t : t = of_float32 (F.expm1 (to_float32 t))
let[@inline] log1p t : t = of_float32 (F.log1p (to_float32 t))
let[@inline] copysign t1 t2 : t = of_float32 (F.copysign (to_float32 t1) (to_float32 t2))
let[@inline] cos t = of_float32 (F.cos (to_float32 t))
let[@inline] sin t : t = of_float32 (F.sin (to_float32 t))
let[@inline] tan t : t = of_float32 (F.tan (to_float32 t))
let[@inline] acos t : t = of_float32 (F.acos (to_float32 t))
let[@inline] asin t : t = of_float32 (F.asin (to_float32 t))
let[@inline] atan t : t = of_float32 (F.atan (to_float32 t))
let[@inline] atan2 t1 t2 : t = of_float32 (F.atan2 (to_float32 t1) (to_float32 t2))
let[@inline] hypot t1 t2 : t = of_float32 (F.hypot (to_float32 t1) (to_float32 t2))
let[@inline] cosh t : t = of_float32 (F.cosh (to_float32 t))
let[@inline] sinh t : t = of_float32 (F.sinh (to_float32 t))
let[@inline] tanh t : t = of_float32 (F.tanh (to_float32 t))
let[@inline] acosh t : t = of_float32 (F.acosh (to_float32 t))
let[@inline] asinh t : t = of_float32 (F.asinh (to_float32 t))
let[@inline] atanh t : t = of_float32 (F.atanh (to_float32 t))
let[@inline] sqrt t : t = of_float32 (F.sqrt (to_float32 t))
let[@inline] cbrt t : t = of_float32 (F.cbrt (to_float32 t))
let[@inline] exp t : t = of_float32 (F.exp (to_float32 t))
let[@inline] log t : t = of_float32 (F.log (to_float32 t))

module Class = Base.Float.Class

let[@inline] classify t : Class.t = (F.classify [@inlined hint]) (to_float32 t)

let[@inline] sign t : Base.Sign.t =
  (F.sign [@inlined hint]) (to_float32 t) [@alert "-deprecated"]
;;

let[@inline] sign_exn t : Base.Sign.t = (F.sign_exn [@inlined hint]) (to_float32 t)

let[@inline] sign_or_nan t : Base.Sign_or_nan.t =
  (F.sign_or_nan [@inlined hint]) (to_float32 t)
;;

let[@inline] create_ieee_exn ~negative ~exponent ~mantissa : t =
  of_float32 ((F.create_ieee_exn [@inlined hint]) ~negative ~exponent ~mantissa)
;;

let[@inline] ieee_negative t : bool = (F.ieee_negative [@inlined hint]) (to_float32 t)
let[@inline] ieee_exponent t : int = (F.ieee_exponent [@inlined hint]) (to_float32 t)
let[@inline] ieee_mantissa t : int = (F.ieee_mantissa [@inlined hint]) (to_float32 t)

external box_int32 : int32# -> (int32[@local_opt]) @@ portable = "%box_int32"
external unbox_int32 : (int32[@local_opt]) -> int32# @@ portable = "%unbox_int32"

let[@inline] to_bits x = F.to_bits (to_float32 x) |> unbox_int32
let[@inline] of_bits x = F.of_bits (box_int32 x) |> of_float32

let[@inline] select b (ifso : t) (ifnot : t) : t =
  Unboxed.select_int32 b (to_bits ifso) (to_bits ifnot) |> of_bits
;;

let[@inline] first_non_nan (x : t) (y : t) : t = select (is_nan x) y x

(* Both of these implementations are the same up to direction.

   [if (x < y) then x else y] is the correct min, except that if either is nan we get y
   (because comparisons with nan are false). If y is nan, that's correct. If x is nan, we
   need to pick it, so explicitly test that.

   Implement that branchlessly with [select]. The only weird thing here is that we want to
   do two selects, but not pass back and forth to [int32] twice, so we write out the
   selects in int32-space.
*)

let[@inline] min (x : t) (y : t) : t =
  let xb = to_bits x in
  let yb = to_bits y in
  let minb = Unboxed.select_int32 (x < y) xb yb in
  Unboxed.select_int32 (is_nan x) xb minb |> of_bits
;;

let[@inline] max (x : t) (y : t) : t =
  let xb = to_bits x in
  let yb = to_bits y in
  let maxb = Unboxed.select_int32 (x > y) xb yb in
  Unboxed.select_int32 (is_nan x) xb maxb |> of_bits
;;

include struct
  open Base_quickcheck

  let%template quickcheck_generator =
    (Generator.Via_thunk.map [@mode portable]) Generator.float ~f:(fun f () ->
      of_float (f ()))
  ;;

  let%template quickcheck_observer =
    (Observer.Via_thunk.unmap [@mode portable]) Observer.float ~f:(fun f () ->
      to_float (f ()))
  ;;

  let quickcheck_shrinker = Shrinker.atomic
end

module Ref = struct
  type nonrec t = { mutable contents : t }

  let[@inline] get t = t.contents
  let[@inline] set t x = t.contents <- x
  let[@inline] add t x = set t (get t + x)
  let[@inline] create contents = { contents }
  let[@inline] create_zero () = create #0.0s

  module O = struct
    let[@inline] ( ! ) t = get t
    let[@inline] ( := ) t x = set t x
  end
end

module Array = struct
  type ('a : float32) t = 'a builtin_array

  external length : local_ 'a t -> int @@ portable = "%array_length"
  external get : local_ 'a t -> int -> 'a @@ portable = "%array_safe_get"
  external set : local_ 'a t -> int -> 'a -> unit @@ portable = "%array_safe_set"
  external unsafe_get : local_ 'a t -> int -> 'a @@ portable = "%array_unsafe_get"
  external unsafe_set : local_ 'a t -> int -> 'a -> unit @@ portable = "%array_unsafe_set"

  (* Unsafe because this function may be used to create values of type ['a], which may
     have user-defined invariants beyond the float32 layout. *)
  external unsafe_create_uninitialized
    :  len:int
    -> 'a t
    @@ portable
    = "caml_make_unboxed_float32_vect_bytecode" "caml_make_unboxed_float32_vect"

  external create_uninitialized
    :  len:int
    -> float32# t
    @@ portable
    = "caml_make_unboxed_float32_vect_bytecode" "caml_make_unboxed_float32_vect"

  external create_local_uninitialized
    :  len:int
    -> float32# t @ local
    @@ portable
    = "caml_make_unboxed_float32_vect_bytecode" "caml_make_local_unboxed_float32_vect"

  external unsafe_blit
    :  src:local_ 'a t
    -> src_pos:int
    -> dst:local_ 'a t
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit" "caml_unboxed_float32_vect_blit"

  external globalize' : local_ 'a t -> 'a t @@ portable = "%obj_dup"

  let globalize _ t = globalize' t

  [%%template
  (* Copied from lib/base/src/ppx_compare_lib.ml *)
  let[@mode local] [@zero_alloc] compare comp arr1 arr2 =
    if phys_equal arr1 arr2
    then 0
    else (
      let len_a = length arr1 in
      let len_b = length arr2 in
      let ret = Int.compare len_a len_b in
      if Int.(ret <> 0)
      then ret
      else (
        let[@zero_alloc] rec loop i =
          if Int.(i = len_a)
          then 0
          else (
            let l = unsafe_get arr1 i
            and r = unsafe_get arr2 i in
            let res = (comp [@zero_alloc assume]) l r in
            if Int.(res <> 0) then res else loop Int.(i + 1))
        in
        loop 0 [@nontail]))
  ;;

  let[@zero_alloc] compare = [%eta3 compare [@mode local]]]

  (* The impl was copied from [lib/base/src/array.ml]. *)
  let[@zero_alloc] for_all2_local_exn t1 t2 ~f =
    let i = ref Int.O.(length t1 - 1) in
    let result = ref true in
    while Int.O.(!i >= 0) && !result do
      if not ((f [@zero_alloc assume]) (unsafe_get t1 !i) (unsafe_get t2 !i))
      then result := false
      else Int.decr i
    done;
    !result
  ;;

  [%%template
  let[@mode local] [@zero_alloc] equal element_equal t1 t2 =
    Int.O.(length t1 = length t2) && for_all2_local_exn t1 t2 ~f:element_equal
  ;;

  let[@zero_alloc] equal = [%eta3 equal [@mode local]]]

  (* Copied with minor changes from [sexp_of_array] in [lib/sexplib0/src/sexp_conv.ml] *)
  let sexp_of_t sexp_of__a ar =
    let lst_ref = ref [] in
    for i = Int.O.(length ar - 1) downto 0 do
      lst_ref := sexp_of__a (get ar i) :: !lst_ref
    done;
    Sexp.List !lst_ref
  ;;

  (* Custom implementation using unsafe uninitialized creation *)
  let t_of_sexp a__of_sexp sexp =
    match (sexp : Sexp.t) with
    | List [] -> [||]
    | List t ->
      let len = List.length t in
      let res = unsafe_create_uninitialized ~len in
      List.iteri t ~f:(fun i el -> unsafe_set res i (a__of_sexp el));
      res
    | Atom _ -> raise_s [%sexp "array_of_sexp: list needed: {sexp#Sexp}"]
  ;;

  let init len ~f =
    let r = unsafe_create_uninitialized ~len in
    for i = 0 to Base.Int.O.(len - 1) do
      unsafe_set r i (f i)
    done;
    r
  ;;

  let copy t = init (length t) ~f:(fun i -> unsafe_get t i) [@nontail]
end

module Bigarray = Stdlib_stable.Float32_u.Bigarray
