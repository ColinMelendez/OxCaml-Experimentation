module I = Base.Nativeint

type t = nativeint#

let[@inline] globalize (local_ (t : t)) = t

module Boxed = Core.Nativeint

external of_nativeint : (nativeint[@local_opt]) -> t @@ portable = "%unbox_nativeint"
external to_nativeint : t -> (nativeint[@local_opt]) @@ portable = "%box_nativeint"

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
  of_nativeint
    (Ocaml_intrinsics_kernel.Conditional.select_nativeint
       b
       (to_nativeint t1)
       (to_nativeint t2))
;;

let[@inline] to_int x = (I.to_int [@inlined hint]) (to_nativeint x)
let[@inline] of_float x = of_nativeint ((I.of_float [@inlined hint]) x)
let[@inline] to_float t = (I.to_float [@inlined hint]) (to_nativeint t)
let[@inline] of_int_exn x = of_nativeint ((I.of_int_exn [@inlined hint]) x)

module Shared_derived = struct
  let[@inline] t_of_sexp x = of_nativeint ((I.t_of_sexp [@inlined hint]) x)

  let%template[@alloc a = (heap, stack)] [@inline] sexp_of_t t =
    (I.sexp_of_t [@alloc a] [@inlined hint]) (to_nativeint t) [@exclave_if_stack a]
  ;;

  let bin_shape_t = Bin_prot_unboxed_numbers.Nativeint_u.bin_shape_t

  [%%template
  [@@@mode.default m = (global, local)]

  let[@inline always] bin_size_t t =
    (Bin_prot_unboxed_numbers.Nativeint_u.bin_size_t [@mode m] [@inlined]) t
  ;;

  let[@inline always] bin_write_t buf ~pos t =
    (Bin_prot_unboxed_numbers.Nativeint_u.bin_write_t [@mode m] [@inlined]) buf ~pos t
  ;;]

  let[@inline always] bin_read_t buf ~pos_ref =
    (Bin_prot_unboxed_numbers.Nativeint_u.bin_read_t [@inlined]) buf ~pos_ref
  ;;

  let __bin_read_t__ = Bin_prot_unboxed_numbers.Nativeint_u.__bin_read_t__
  let bin_writer_t = Bin_prot_unboxed_numbers.Nativeint_u.bin_writer_t
  let bin_reader_t = Bin_prot_unboxed_numbers.Nativeint_u.bin_reader_t
  let bin_t = Bin_prot_unboxed_numbers.Nativeint_u.bin_t

  let[@inline] hash_fold_t state t =
    (I.hash_fold_t [@inlined hint]) state (to_nativeint t)
  ;;

  let[@inline] hash t = (I.hash [@inlined hint]) (to_nativeint t)
  let typerep_of_t = Typerep_lib.Std.Typerep.Nativeint_u
  let[@inline] of_string x = of_nativeint ((I.of_string [@inlined hint]) x)

  let%template[@alloc a = (heap, stack)] [@inline] to_string t =
    (I.to_string [@alloc a] [@inlined hint]) (to_nativeint t) [@exclave_if_stack a]
  ;;

  let%template[@mode m = (global, local)] [@inline] [@zero_alloc] equal t1 t2 : bool =
    (I.equal [@mode m]) (to_nativeint t1) (to_nativeint t2)
  ;;

  let%template[@mode m = (global, local)] [@inline] [@zero_alloc] compare t1 t2 : int =
    (I.compare [@mode m]) (to_nativeint t1) (to_nativeint t2)
  ;;
end

include Shared_derived

(* Inlined from [Base.Int_conversions], and modified to operate over unboxed things. *)
let is_representable_as_int =
  if I.num_bits <= (Sys.int_size |> I.of_int)
  then fun _ -> true
  else (
    let min = of_int_exn Base.Int.min_value in
    let max = of_int_exn Base.Int.max_value in
    fun [@zero_alloc] x -> compare min x <= 0 && compare x max <= 0)
;;

let[@zero_alloc] [@inline] is_representable_as_int x =
  (is_representable_as_int [@zero_alloc assume]) x
;;

(* Inlined from [Base.Int_conversions], and modified to operate over unboxed things. *)
let[@cold] to_int_exn_failure x to_string =
  Base.Printf.failwithf
    "conversion from nativeint to int failed: %s is out of range"
    (to_string x)
    ()
;;

(* We don't implement this as box-then-call-Nativeint-function because this allocates. *)
let[@inline] to_int_exn t =
  if is_representable_as_int t
  then I.to_int_trunc (to_nativeint t)
  else to_int_exn_failure t to_string
;;

let[@inline] ( >= ) t1 t2 = I.( >= ) (to_nativeint t1) (to_nativeint t2)
let[@inline] ( <= ) t1 t2 = I.( <= ) (to_nativeint t1) (to_nativeint t2)

(* [min] can't be implemented in terms of [Nativeint.min] because that function uses
   [caml_csel_value] which forces the integers to be boxed leading to unwanted
   allocations. Instead, we have to copy the function definition and use a different
   [select] function that knows how to handle unboxed types. *)
let[@inline] min t1 t2 : t = select (t1 <= t2) t1 t2

(* Can't be implemented using [Nativeint.max] for the same reason as [min] *)
let[@inline] max t1 t2 : t = select (t1 >= t2) t1 t2

let[@inline] ascending t1 t2 : int =
  (I.ascending [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
;;

let[@inline] descending t1 t2 : int =
  (I.descending [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
;;

let[@inline] between t ~low ~high : bool =
  (I.between [@inlined hint])
    (to_nativeint t)
    ~low:(to_nativeint low)
    ~high:(to_nativeint high)
;;

let[@inline] clamp_unchecked t ~min:min_ ~max:max_ =
  (min [@inlined hint]) t max_ |> (max [@inlined hint]) min_
;;

(* Can't be implemented using [Nativeint.clamp_exn] for the same reason as [min] *)
let[@inline] clamp_exn t ~min ~max =
  assert (min <= max);
  (clamp_unchecked [@inlined hint]) t ~min ~max
;;

let[@inline] pp ppf t : unit = (I.pp [@inlined hint]) ppf (to_nativeint t)
let[@inline] invariant t : unit = (I.invariant [@inlined hint]) (to_nativeint t)
let[@inline] is_positive t = (I.is_positive [@inlined hint]) (to_nativeint t)
let[@inline] is_non_negative t = (I.is_non_negative [@inlined hint]) (to_nativeint t)
let[@inline] is_negative t = (I.is_negative [@inlined hint]) (to_nativeint t)
let[@inline] is_non_positive t = (I.is_non_positive [@inlined hint]) (to_nativeint t)
let[@inline] sign t = (I.sign [@inlined hint]) (to_nativeint t)

let[@inline] to_string_hum ?delimiter t =
  (I.to_string_hum [@inlined hint]) ?delimiter (to_nativeint t)
;;

let one = of_nativeint I.one
let minus_one = of_nativeint I.minus_one

let[@inline] round ?dir t ~to_multiple_of:t2 =
  of_nativeint
    ((I.round [@inlined hint]) ?dir (to_nativeint t) ~to_multiple_of:(to_nativeint t2))
;;

(* let[@inline] round_towards_zero t ~to_multiple_of:t2 =
 *   of_nativeint
 *     ((I.round_towards_zero [@inlined hint]) (to_nativeint t) ~to_multiple_of:(to_nativeint t2))
 * ;; *)

let[@inline] round_down t ~to_multiple_of:t2 =
  of_nativeint
    ((I.round_down [@inlined hint]) (to_nativeint t) ~to_multiple_of:(to_nativeint t2))
;;

let[@inline] round_up t ~to_multiple_of:t2 =
  of_nativeint
    ((I.round_up [@inlined hint]) (to_nativeint t) ~to_multiple_of:(to_nativeint t2))
;;

let[@inline] round_nearest t ~to_multiple_of:t2 =
  of_nativeint
    ((I.round_nearest [@inlined hint]) (to_nativeint t) ~to_multiple_of:(to_nativeint t2))
;;

let[@inline] succ t = of_nativeint ((I.succ [@inlined hint]) (to_nativeint t))
let[@inline] pred t = of_nativeint ((I.pred [@inlined hint]) (to_nativeint t))

let[@inline] pow t1 t2 =
  of_nativeint ((I.pow [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
;;

let[@inline] bit_and t1 t2 =
  of_nativeint ((I.bit_and [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
;;

let[@inline] bit_or t1 t2 =
  of_nativeint ((I.bit_or [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
;;

let[@inline] bit_xor t1 t2 =
  of_nativeint ((I.bit_xor [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
;;

let[@inline] bit_not t = of_nativeint ((I.bit_not [@inlined hint]) (to_nativeint t))
let[@inline] popcount t = (I.popcount [@inlined hint]) (to_nativeint t) |> of_nativeint

let[@inline] shift_left t x =
  of_nativeint ((I.shift_left [@inlined hint]) (to_nativeint t) x)
;;

let[@inline] shift_right t x =
  of_nativeint ((I.shift_right [@inlined hint]) (to_nativeint t) x)
;;

let[@inline] of_int32_exn x = of_nativeint ((I.of_int32_exn [@inlined hint]) x)
let[@inline] to_int32_exn t = (I.to_int32_exn [@inlined hint]) (to_nativeint t)
let[@inline] of_int64_exn x = of_nativeint ((I.of_int64_exn [@inlined hint]) x)
let[@inline] to_int64 t = (I.to_int64 [@inlined hint]) (to_nativeint t)
let of_nativeint_exn = of_nativeint
let to_nativeint_exn = to_nativeint

let[@inline] of_float_unchecked f =
  of_nativeint ((I.of_float_unchecked [@inlined hint]) f)
;;

module O = struct
  external box : nativeint# -> (nativeint[@local_opt]) @@ portable = "%box_nativeint"
  external unbox : (nativeint[@local_opt]) -> nativeint# @@ portable = "%unbox_nativeint"

  let zero = of_nativeint I.O.zero

  let[@inline] ( + ) t1 t2 =
    of_nativeint ((I.O.( + ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( - ) t1 t2 =
    of_nativeint ((I.O.( - ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( * ) t1 t2 =
    of_nativeint ((I.O.( * ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( ** ) t1 t2 =
    of_nativeint ((I.O.( ** ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( >= ) t1 t2 =
    (I.O.( >= ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
  ;;

  let[@inline] ( <= ) t1 t2 =
    (I.O.( <= ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
  ;;

  let[@inline] ( = ) t1 t2 =
    (I.O.( = ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
  ;;

  let[@inline] ( > ) t1 t2 =
    (I.O.( > ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
  ;;

  let[@inline] ( < ) t1 t2 =
    (I.O.( < ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
  ;;

  let[@inline] ( <> ) t1 t2 =
    (I.O.( <> ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
  ;;

  let[@inline] neg t = of_nativeint ((I.O.neg [@inlined hint]) (to_nativeint t))
  let[@inline] ( ~- ) t = of_nativeint ((I.O.( ~- ) [@inlined hint]) (to_nativeint t))

  let[@inline] rem t1 t2 =
    of_nativeint ((I.rem [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( /% ) t1 t2 =
    of_nativeint ((I.O.( /% ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( % ) t1 t2 =
    of_nativeint ((I.O.( % ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( / ) t1 t2 =
    of_nativeint ((I.O.( / ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( // ) t1 t2 =
    (I.O.( // ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2)
  ;;

  let[@inline] ( land ) t1 t2 =
    of_nativeint ((I.O.( land ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( lor ) t1 t2 =
    of_nativeint ((I.O.( lor ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] ( lxor ) t1 t2 =
    of_nativeint ((I.O.( lxor ) [@inlined hint]) (to_nativeint t1) (to_nativeint t2))
  ;;

  let[@inline] lnot t = of_nativeint ((I.O.lnot [@inlined hint]) (to_nativeint t))

  let[@inline] ( lsl ) t x =
    of_nativeint ((I.O.( lsl ) [@inlined hint]) (to_nativeint t) x)
  ;;

  let[@inline] ( asr ) t x =
    of_nativeint ((I.O.( asr ) [@inlined hint]) (to_nativeint t) x)
  ;;

  let[@inline] ( lsr ) t x =
    of_nativeint ((I.O.( lsr ) [@inlined hint]) (to_nativeint t) x)
  ;;

  let[@inline] abs t = of_nativeint ((I.O.abs [@inlined hint]) (to_nativeint t))
end

include O

let num_bits = I.num_bits
let max_value = of_nativeint I.max_value
let min_value = of_nativeint I.min_value
let[@inline] ( lsr ) t x = of_nativeint ((I.(( lsr )) [@inlined hint]) (to_nativeint t) x)
let shift_right_logical = ( lsr )
let[@inline] ceil_pow2 t = of_nativeint ((I.ceil_pow2 [@inlined hint]) (to_nativeint t))
let[@inline] floor_pow2 t = of_nativeint ((I.floor_pow2 [@inlined hint]) (to_nativeint t))
let[@inline] ceil_log2 t = (I.ceil_log2 [@inlined hint]) (to_nativeint t) |> of_nativeint

let[@inline] floor_log2 t =
  (I.floor_log2 [@inlined hint]) (to_nativeint t) |> of_nativeint
;;

let[@inline] is_pow2 t = (I.is_pow2 [@inlined hint]) (to_nativeint t)
let[@inline] clz t = (I.clz [@inlined hint]) (to_nativeint t) |> of_nativeint
let[@inline] ctz t = (I.ctz [@inlined hint]) (to_nativeint t) |> of_nativeint
let[@inline] of_int x = of_nativeint (I.of_int x)
let[@inline] of_int32 x = of_nativeint (I.of_int32 x)
let[@inline] to_int32 t = (I.to_int32 [@inlined hint]) (to_nativeint t)
let[@inline] to_int_trunc t = (I.to_int_trunc [@inlined hint]) (to_nativeint t)
let[@inline] to_int32_trunc t = I.to_int32_trunc (to_nativeint t)
let[@inline] of_int64_trunc x = of_nativeint (I.of_int64_trunc x)
let[@inline] bswap t = of_nativeint (I.bswap (to_nativeint t))

include struct
  open Base_quickcheck

  let%template quickcheck_generator =
    (Generator.Via_thunk.map [@mode portable]) Generator.nativeint ~f:(fun f () ->
      of_nativeint (f ()))
  ;;

  let%template quickcheck_observer =
    (Observer.Via_thunk.unmap [@mode portable]) Observer.nativeint ~f:(fun f () ->
      to_nativeint (f ()))
  ;;

  let quickcheck_shrinker = Shrinker.atomic
end

module Array_index = struct
  external get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    @@ portable
    = "%array_safe_get_indexed_by_nativeint#"
  [@@layout_poly]

  external set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_safe_set_indexed_by_nativeint#"
  [@@layout_poly]

  external unsafe_get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    @@ portable
    = "%array_unsafe_get_indexed_by_nativeint#"
  [@@layout_poly]

  external unsafe_set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_unsafe_set_indexed_by_nativeint#"
  [@@layout_poly]
end

module Array = struct
  type ('a : word) t = 'a array

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
    -> nativeint# t
    @@ portable
    = "caml_make_unboxed_nativeint_vect_bytecode" "caml_make_unboxed_nativeint_vect"

  external unsafe_blit
    :  src:('a t[@local_opt])
    -> src_pos:int
    -> dst:('a t[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit" "caml_unboxed_nativeint_vect_blit"

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
  type word_size =
    | I64
    | I32

  let word_size () =
    match Sys.word_size with
    | 64 -> I64
    | 32 -> I32
    | word_size -> Base.Printf.failwithf "unsupported word size %d" word_size ()
  ;;

  module Local = struct
    type nonrec t = t

    let compare = Shared_derived.compare
    let hash = Shared_derived.hash
    let hash_fold_t = Shared_derived.hash_fold_t

    let of_string s =
      match word_size () with
      | I64 -> of_int64_trunc (Int64_u.box (Int64_u.Hex_unsigned.Local.of_string s))
      | I32 -> of_int32 (Int32_u.box (Int32_u.Hex_unsigned.of_string s))
    ;;

    let t_of_sexp sexp =
      match word_size () with
      | I64 -> of_int64_trunc (Int64_u.box (Int64_u.Hex_unsigned.Local.t_of_sexp sexp))
      | I32 -> of_int32 (Int32_u.box (Int32_u.Hex_unsigned.t_of_sexp sexp))
    ;;

    let to_string t = exclave_
      match word_size () with
      | I64 -> Int64_u.Hex_unsigned.Local.to_string (Int64_u.unbox (to_int64 t))
      | I32 ->
        (Int32_u.Hex_unsigned.to_string [@alloc stack])
          (Int32_u.unbox (I.to_int32_trunc (to_nativeint t)))
    ;;

    let sexp_of_t t = exclave_
      match word_size () with
      | I64 -> Int64_u.Hex_unsigned.Local.sexp_of_t (Int64_u.unbox (to_int64 t))
      | I32 ->
        (Int32_u.Hex_unsigned.sexp_of_t [@alloc stack])
          (Int32_u.unbox (I.to_int32_trunc (to_nativeint t)))
    ;;
  end

  type nonrec t = t

  let compare = Local.compare
  let hash = Shared_derived.hash
  let hash_fold_t = Shared_derived.hash_fold_t
  let of_string = [%eta1 Local.of_string]
  let t_of_sexp = [%eta1 Local.t_of_sexp]

  let to_string t =
    match word_size () with
    | I64 -> Int64_u.Hex_unsigned.to_string (Int64_u.unbox (to_int64 t))
    | I32 ->
      Int32_u.Hex_unsigned.to_string (Int32_u.unbox (I.to_int32_trunc (to_nativeint t)))
  ;;

  let sexp_of_t t =
    match word_size () with
    | I64 -> Int64_u.Hex_unsigned.sexp_of_t (Int64_u.unbox (to_int64 t))
    | I32 ->
      Int32_u.Hex_unsigned.sexp_of_t (Int32_u.unbox (I.to_int32_trunc (to_nativeint t)))
  ;;
end
