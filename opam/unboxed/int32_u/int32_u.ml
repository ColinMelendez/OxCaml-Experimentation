module I = Base.Int32
module Boxed = Core.Int32

type t = int32#

let[@inline] globalize (local_ (t : t)) = t

external of_int32 : (int32[@local_opt]) -> t @@ portable = "%unbox_int32"
external to_int32 : t -> (int32[@local_opt]) @@ portable = "%box_int32"

(* Note about the implementation strategy:

   Most functions in this file are implemented by boxing the int, calling the equivalent
   function on boxed ints, and then unboxing the result. This may seem surprising: isn't
   the point of unboxed types to avoid boxes? But it's fine; the compiler's middle-end
   will reliably eliminate these boxing and unboxing steps, and the testsuite checks there
   are no allocations here. If you add new functions, you should add similar tests.

   Why is it done this way? Just because it was less work than adding many new primitives
   to the compiler. But it is likely we will do that work, one day.
*)

let[@inline] select b t1 t2 =
  of_int32
    (Ocaml_intrinsics_kernel.Conditional.select_int32 b (to_int32 t1) (to_int32 t2))
;;

let[@inline] to_int x = (I.to_int [@inlined hint]) (to_int32 x)
let[@inline] of_float x = of_int32 ((I.of_float [@inlined hint]) x)
let[@inline] to_float t = (I.to_float [@inlined hint]) (to_int32 t)
let[@inline] of_int_exn x = of_int32 ((I.of_int_exn [@inlined hint]) x)
let[@inline] to_int_exn t = (I.to_int_exn [@inlined hint]) (to_int32 t)

module Shared_derived = struct
  let[@inline] t_of_sexp x = of_int32 ((I.t_of_sexp [@inlined hint]) x)
  let[@inline] sexp_of_t t = (I.sexp_of_t [@inlined hint]) (to_int32 t)

  let%template[@alloc stack] [@inline] sexp_of_t t = exclave_
    (I.sexp_of_t [@alloc stack] [@inlined hint]) (to_int32 t)
  ;;

  let bin_shape_t = Bin_prot_unboxed_numbers.Int32_u.bin_shape_t

  [%%template
  [@@@mode.default m = (global, local)]

  let[@inline always] bin_size_t t =
    (Bin_prot_unboxed_numbers.Int32_u.bin_size_t [@mode m] [@inlined]) t
  ;;

  let[@inline always] bin_write_t buf ~pos t =
    (Bin_prot_unboxed_numbers.Int32_u.bin_write_t [@mode m] [@inlined]) buf ~pos t
  ;;]

  let[@inline always] bin_read_t buf ~pos_ref =
    (Bin_prot_unboxed_numbers.Int32_u.bin_read_t [@inlined]) buf ~pos_ref
  ;;

  let __bin_read_t__ = Bin_prot_unboxed_numbers.Int32_u.__bin_read_t__
  let bin_writer_t = Bin_prot_unboxed_numbers.Int32_u.bin_writer_t
  let bin_reader_t = Bin_prot_unboxed_numbers.Int32_u.bin_reader_t
  let bin_t = Bin_prot_unboxed_numbers.Int32_u.bin_t
  let[@inline] hash_fold_t state t = (I.hash_fold_t [@inlined hint]) state (to_int32 t)
  let[@inline] hash t = (I.hash [@inlined hint]) (to_int32 t)
  let typerep_of_t = Typerep_lib.Std.Typerep.Int32_u
  let[@inline] of_string x = of_int32 ((I.of_string [@inlined hint]) x)

  let%template[@alloc a = (heap, stack)] [@inline] to_string t =
    (I.to_string [@alloc a] [@inlined hint]) (to_int32 t) [@exclave_if_stack a]
  ;;

  let%template[@mode m = (global, local)] [@inline] [@zero_alloc] equal t1 t2 : bool =
    (I.equal [@mode m]) (to_int32 t1) (to_int32 t2)
  ;;

  let%template[@mode m = (global, local)] [@inline] [@zero_alloc] compare t1 t2 : int =
    (I.compare [@mode m]) (to_int32 t1) (to_int32 t2)
  ;;
end

include Shared_derived

let[@inline] ( >= ) t1 t2 = I.( >= ) (to_int32 t1) (to_int32 t2)
let[@inline] ( <= ) t1 t2 = I.( <= ) (to_int32 t1) (to_int32 t2)

(* [min] can't be implemented in terms of [Int32.min] because that function uses
   [caml_csel_value] which forces the integers to be boxed leading to unwanted
   allocations. Instead, we have to copy the function definition and use a different
   [select] function that knows how to handle unboxed types. *)
let[@inline] min t1 t2 : t = select (t1 <= t2) t1 t2

(* Can't be implemented using [Int32.max] for the same reason as [min] *)
let[@inline] max t1 t2 : t = select (t1 >= t2) t1 t2

let[@inline] ascending t1 t2 : int =
  (I.ascending [@inlined hint]) (to_int32 t1) (to_int32 t2)
;;

let[@inline] descending t1 t2 : int =
  (I.descending [@inlined hint]) (to_int32 t1) (to_int32 t2)
;;

let[@inline] between t ~low ~high : bool =
  (I.between [@inlined hint]) (to_int32 t) ~low:(to_int32 low) ~high:(to_int32 high)
;;

let[@inline] clamp_unchecked t ~min:min_ ~max:max_ =
  (min [@inlined hint]) t max_ |> (max [@inlined hint]) min_
;;

(* Can't be implemented using [Int32.clamp_exn] for the same reason as [min] *)
let[@inline] clamp_exn t ~min ~max =
  assert (min <= max);
  (clamp_unchecked [@inlined hint]) t ~min ~max
;;

let[@inline] pp ppf t : unit = (I.pp [@inlined hint]) ppf (to_int32 t)
let[@inline] invariant t : unit = (I.invariant [@inlined hint]) (to_int32 t)
let[@inline] is_positive t = (I.is_positive [@inlined hint]) (to_int32 t)
let[@inline] is_non_negative t = (I.is_non_negative [@inlined hint]) (to_int32 t)
let[@inline] is_negative t = (I.is_negative [@inlined hint]) (to_int32 t)
let[@inline] is_non_positive t = (I.is_non_positive [@inlined hint]) (to_int32 t)
let[@inline] sign t = (I.sign [@inlined hint]) (to_int32 t)

let[@inline] to_string_hum ?delimiter t =
  (I.to_string_hum [@inlined hint]) ?delimiter (to_int32 t)
;;

let one = of_int32 I.one
let minus_one = of_int32 I.minus_one

let[@inline] round ?dir t ~to_multiple_of:t2 =
  of_int32 ((I.round [@inlined hint]) ?dir (to_int32 t) ~to_multiple_of:(to_int32 t2))
;;

(* let[@inline] round_towards_zero t ~to_multiple_of:t2 =
 *   of_int32
 *     ((I.round_towards_zero [@inlined hint]) (to_int32 t) ~to_multiple_of:(to_int32 t2))
 * ;; *)

let[@inline] round_down t ~to_multiple_of:t2 =
  of_int32 ((I.round_down [@inlined hint]) (to_int32 t) ~to_multiple_of:(to_int32 t2))
;;

let[@inline] round_up t ~to_multiple_of:t2 =
  of_int32 ((I.round_up [@inlined hint]) (to_int32 t) ~to_multiple_of:(to_int32 t2))
;;

let[@inline] round_nearest t ~to_multiple_of:t2 =
  of_int32 ((I.round_nearest [@inlined hint]) (to_int32 t) ~to_multiple_of:(to_int32 t2))
;;

let[@inline] succ t = of_int32 ((I.succ [@inlined hint]) (to_int32 t))
let[@inline] pred t = of_int32 ((I.pred [@inlined hint]) (to_int32 t))
let[@inline] pow t1 t2 = of_int32 ((I.pow [@inlined hint]) (to_int32 t1) (to_int32 t2))

let[@inline] bit_and t1 t2 =
  of_int32 ((I.bit_and [@inlined hint]) (to_int32 t1) (to_int32 t2))
;;

let[@inline] bit_or t1 t2 =
  of_int32 ((I.bit_or [@inlined hint]) (to_int32 t1) (to_int32 t2))
;;

let[@inline] bit_xor t1 t2 =
  of_int32 ((I.bit_xor [@inlined hint]) (to_int32 t1) (to_int32 t2))
;;

let[@inline] bit_not t = of_int32 ((I.bit_not [@inlined hint]) (to_int32 t))
let[@inline] popcount t = (I.popcount [@inlined hint]) (to_int32 t) |> of_int32
let[@inline] shift_left t x = of_int32 ((I.shift_left [@inlined hint]) (to_int32 t) x)
let[@inline] shift_right t x = of_int32 ((I.shift_right [@inlined hint]) (to_int32 t) x)
let of_int32_exn = of_int32
let to_int32_exn = to_int32
let[@inline] of_int64_exn x = of_int32 (I.of_local_int64_exn x) [@@zero_alloc]
let[@inline] to_int64 t = (I.to_int64 [@inlined hint]) (to_int32 t)
let[@inline] of_nativeint_exn x = of_int32 ((I.of_nativeint_exn [@inlined hint]) x)
let[@inline] to_nativeint_exn t = (I.to_nativeint_exn [@inlined hint]) (to_int32 t)
let[@inline] of_float_unchecked f = of_int32 ((I.of_float_unchecked [@inlined hint]) f)

module O = struct
  external box : int32# -> (int32[@local_opt]) @@ portable = "%box_int32"
  external unbox : (int32[@local_opt]) -> int32# @@ portable = "%unbox_int32"

  let[@inline] ( + ) t1 t2 =
    of_int32 ((I.O.( + ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( - ) t1 t2 =
    of_int32 ((I.O.( - ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( * ) t1 t2 =
    of_int32 ((I.O.( * ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( ** ) t1 t2 =
    of_int32 ((I.O.( ** ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( >= ) t1 t2 = (I.O.( >= ) [@inlined hint]) (to_int32 t1) (to_int32 t2)
  let[@inline] ( <= ) t1 t2 = (I.O.( <= ) [@inlined hint]) (to_int32 t1) (to_int32 t2)
  let[@inline] ( = ) t1 t2 = (I.O.( = ) [@inlined hint]) (to_int32 t1) (to_int32 t2)
  let[@inline] ( > ) t1 t2 = (I.O.( > ) [@inlined hint]) (to_int32 t1) (to_int32 t2)
  let[@inline] ( < ) t1 t2 = (I.O.( < ) [@inlined hint]) (to_int32 t1) (to_int32 t2)
  let[@inline] ( <> ) t1 t2 = (I.O.( <> ) [@inlined hint]) (to_int32 t1) (to_int32 t2)
  let[@inline] neg t = of_int32 ((I.O.neg [@inlined hint]) (to_int32 t))
  let[@inline] ( ~- ) t = of_int32 ((I.O.( ~- ) [@inlined hint]) (to_int32 t))

  let[@inline] ( /% ) t1 t2 =
    of_int32 ((I.O.( /% ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( % ) t1 t2 =
    of_int32 ((I.O.( % ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( / ) t1 t2 =
    of_int32 ((I.O.( / ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( // ) t1 t2 = (I.O.( // ) [@inlined hint]) (to_int32 t1) (to_int32 t2)
  let[@inline] rem t1 t2 = of_int32 ((I.rem [@inlined hint]) (to_int32 t1) (to_int32 t2))

  let[@inline] ( land ) t1 t2 =
    of_int32 ((I.O.( land ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( lor ) t1 t2 =
    of_int32 ((I.O.( lor ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] ( lxor ) t1 t2 =
    of_int32 ((I.O.( lxor ) [@inlined hint]) (to_int32 t1) (to_int32 t2))
  ;;

  let[@inline] lnot t = of_int32 ((I.O.lnot [@inlined hint]) (to_int32 t))
  let[@inline] ( lsl ) t x = of_int32 ((I.O.( lsl ) [@inlined hint]) (to_int32 t) x)
  let[@inline] ( asr ) t x = of_int32 ((I.O.( asr ) [@inlined hint]) (to_int32 t) x)
  let[@inline] abs t = of_int32 ((I.O.abs [@inlined hint]) (to_int32 t))
  let[@inline] ( lsr ) t x = of_int32 ((I.O.(( lsr )) [@inlined hint]) (to_int32 t) x)
  let zero = of_int32 I.zero
end

include O

let num_bits = I.num_bits
let max_value = of_int32 I.max_value
let min_value = of_int32 I.min_value
let[@inline] ( lsr ) t x = of_int32 ((I.(( lsr )) [@inlined hint]) (to_int32 t) x)
let shift_right_logical = ( lsr )
let[@inline] ceil_pow2 t = of_int32 ((I.ceil_pow2 [@inlined hint]) (to_int32 t))
let[@inline] floor_pow2 t = of_int32 ((I.floor_pow2 [@inlined hint]) (to_int32 t))
let[@inline] ceil_log2 t = (I.ceil_log2 [@inlined hint]) (to_int32 t) |> of_int32
let[@inline] floor_log2 t = (I.floor_log2 [@inlined hint]) (to_int32 t) |> of_int32
let[@inline] is_pow2 t = (I.is_pow2 [@inlined hint]) (to_int32 t)
let[@inline] clz t = (I.clz [@inlined hint]) (to_int32 t) |> of_int32
let[@inline] ctz t = (I.ctz [@inlined hint]) (to_int32 t) |> of_int32
let[@inline] to_nativeint t = I.to_nativeint (to_int32 t)
let[@inline] of_int_trunc x = of_int32 (I.of_int_trunc x)
let[@inline] to_int_trunc t = (I.to_int_trunc [@inlined hint]) (to_int32 t)
let[@inline] of_nativeint_trunc x = of_int32 (I.of_nativeint_trunc x)
let[@inline] of_int64_trunc x = of_int32 (I.of_int64_trunc x)
let[@inline] to_int64_u t = Int64_u.of_int64 (to_int64 t) [@@zero_alloc]
let[@inline] of_int64_u_trunc t = of_int64_trunc (Int64_u.to_int64 t) [@@zero_alloc]

let[@inline] of_int64_u_exn t = of_int64_exn (local_ Int64_u.to_int64 t) [@nontail]
[@@zero_alloc]
;;

let[@inline] bits_of_float f = of_int32 (I.bits_of_float f)
let[@inline] float_of_bits t = I.float_of_bits (to_int32 t)
let[@inline] bswap16 t = of_int32 ((I.bswap16 [@inlined hint]) (to_int32 t))
let[@inline] bswap32 t = of_int32 (I.bswap32 (to_int32 t))

include struct
  open Base_quickcheck

  let%template quickcheck_generator =
    (Generator.Via_thunk.map [@mode portable]) Generator.int32 ~f:(fun f () ->
      of_int32 (f ()))
  ;;

  let%template quickcheck_observer =
    (Observer.Via_thunk.unmap [@mode portable]) Observer.int32 ~f:(fun f () ->
      to_int32 (f ()))
  ;;

  let quickcheck_shrinker = Shrinker.atomic
end

module Array_index = struct
  external get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    @@ portable
    = "%array_safe_get_indexed_by_int32#"
  [@@layout_poly]

  external set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_safe_set_indexed_by_int32#"
  [@@layout_poly]

  external unsafe_get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    @@ portable
    = "%array_unsafe_get_indexed_by_int32#"
  [@@layout_poly]

  external unsafe_set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_unsafe_set_indexed_by_int32#"
  [@@layout_poly]
end

module Array = struct
  type ('a : bits32) t = 'a array

  external length : ('a t[@local_opt]) -> int @@ portable = "%array_length"
  external get : ('a t[@local_opt]) -> int -> 'a @@ portable = "%array_safe_get"
  external set : ('a t[@local_opt]) -> int -> 'a -> unit @@ portable = "%array_safe_set"
  external unsafe_get : ('a t[@local_opt]) -> int -> 'a @@ portable = "%array_unsafe_get"

  external unsafe_set
    :  ('a t[@local_opt])
    -> int
    -> 'a
    -> unit
    @@ portable
    = "%array_unsafe_set"

  external create_uninitialized
    :  len:int
    -> int32# t
    @@ portable
    = "caml_make_unboxed_int32_vect_bytecode" "caml_make_unboxed_int32_vect"

  external unsafe_blit
    :  src:('a t[@local_opt])
    -> src_pos:int
    -> dst:('a t[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit" "caml_unboxed_int32_vect_blit"

  let sexp_of_t sexp_of__a ar =
    let open Base in
    let lst_ref = ref [] in
    for i = length ar - 1 downto 0 do
      lst_ref := sexp_of__a (get ar i) :: !lst_ref
    done;
    Sexp.List !lst_ref
  ;;
end

module Stable = struct
  module V1 = struct
    type nonrec t = t

    include Shared_derived

    let stable_witness = Ppx_stable_witness_runtime.Stable_witness.assert_stable
  end
end

module Hex_unsigned = struct
  module Bytes = Base.Bytes
  module Nothing = Base.Nothing
  module To_string_config = Int64_u.Hex_unsigned.To_string_config
  module Private = Int64_u.Hex_unsigned.Private
  module Sexp = Base.Sexp

  let max_digits = #8L

  type nonrec t = t

  let compare = Shared_derived.compare
  let hash = Shared_derived.hash
  let hash_fold_t = Shared_derived.hash_fold_t

  let[@inline] of_string s =
    let i64 = Int64_u.Hex_unsigned.Private.of_string s ~max_digits in
    let i32 = Int64_u.(select (i64 >= #0x8000_0000L) (i64 - #0x1_0000_0000L) i64) in
    of_int64_u_exn i32
  ;;

  let[@inline] t_of_sexp (sexp : Sexp.t @ local) =
    match sexp with
    | Atom s -> of_string s
    | List _ ->
      (match Private.t_of_sexp_failed sexp with
       | (_ : Nothing.t) -> .)
  ;;

  [%%template
  [@@@alloc.default a = (heap, stack)]

  let[@inline never] to_string_custom t ~config =
    let t = to_int64_u t in
    let digits_to_process = Private.digits_to_process t ~max_digits in
    (let bytes =
       (Bytes.create [@alloc a])
         (Private.to_string_required_length ~config ~digits_to_process)
     in
     Private.to_string_into t (borrow_ bytes) ~config ~digits_to_process;
     Bytes.unique_to_string bytes [@nontail])
    [@exclave_if_stack a]
  ;;

  let[@inline] to_string t : string =
    (to_string_custom [@alloc a])
      t
      ~config:To_string_config.(unbox default) [@exclave_if_stack a]
  ;;

  let[@inline] sexp_of_t t : Sexp.t =
    Atom ((to_string [@alloc a]) t) [@exclave_if_stack a]
  ;;]
end
