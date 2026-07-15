open! Base
module I = Base.Int64

type t = int64#

module Boxed = Base.Int64

let[@inline] globalize (local_ (t : t)) = t

external of_int64 : (int64[@local_opt]) -> t @@ portable = "%unbox_int64"
external to_int64 : t -> (int64[@local_opt]) @@ portable = "%box_int64"
external box_float : float# -> (float[@local_opt]) @@ portable = "%box_float"
external unbox_float : (float[@local_opt]) -> float# @@ portable = "%unbox_float"

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
  of_int64
    (Ocaml_intrinsics_kernel.Conditional.select_int64 b (to_int64 t1) (to_int64 t2))
;;

let[@inline] to_int x = (I.to_int [@inlined hint]) (to_int64 x)
let[@inline] of_float x = of_int64 ((I.of_float [@inlined hint]) (box_float x))
let[@inline] to_float t = (I.to_float [@inlined hint]) (to_int64 t) |> unbox_float
let[@inline] of_int_exn x = of_int64 ((I.of_int_exn [@inlined hint]) x)

module Shared_derived = struct
  let[@inline] t_of_sexp x = of_int64 ((I.t_of_sexp [@inlined hint]) x)

  let%template[@alloc a = (heap, stack)] [@inline] sexp_of_t t =
    (I.sexp_of_t [@alloc a] [@inlined hint]) (to_int64 t) [@exclave_if_stack a]
  ;;

  let t_sexp_grammar = Sexplib0.Sexp_grammar.coerce I.t_sexp_grammar
  let bin_shape_t = Bin_prot_unboxed_numbers.Int64_u.bin_shape_t

  [%%template
  [@@@mode.default m = (global, local)]

  let[@inline always] bin_size_t t =
    (Bin_prot_unboxed_numbers.Int64_u.bin_size_t [@mode m] [@inlined]) t
  ;;

  let[@inline always] bin_write_t buf ~pos t =
    (Bin_prot_unboxed_numbers.Int64_u.bin_write_t [@mode m] [@inlined]) buf ~pos t
  ;;]

  let[@inline always] bin_read_t buf ~pos_ref =
    (Bin_prot_unboxed_numbers.Int64_u.bin_read_t [@inlined]) buf ~pos_ref
  ;;

  let __bin_read_t__ = Bin_prot_unboxed_numbers.Int64_u.__bin_read_t__
  let bin_writer_t = Bin_prot_unboxed_numbers.Int64_u.bin_writer_t
  let bin_reader_t = Bin_prot_unboxed_numbers.Int64_u.bin_reader_t
  let bin_t = Bin_prot_unboxed_numbers.Int64_u.bin_t
  let[@inline] hash_fold_t state t = (I.hash_fold_t [@inlined hint]) state (to_int64 t)
  let[@inline] hash t = (I.hash [@inlined hint]) (to_int64 t)
  let typerep_of_t = Typerep_lib.Std.Typerep.Int64_u
  let[@inline] of_string x = of_int64 ((I.of_string [@inlined hint]) x)

  let%template[@mode m = (global, local)] [@inline] [@zero_alloc] equal t1 t2 : bool =
    (I.equal [@mode m]) (to_int64 t1) (to_int64 t2)
  ;;

  let%template[@mode m = (global, local)] [@inline] [@zero_alloc] compare t1 t2 : int =
    (I.compare [@mode m]) (to_int64 t1) (to_int64 t2)
  ;;
end

include Shared_derived

module O = struct
  let zero = of_int64 I.O.zero
  let[@inline] ( + ) t1 t2 = of_int64 (I.O.( + ) (to_int64 t1) (to_int64 t2))
  let[@inline] ( - ) t1 t2 = of_int64 (I.O.( - ) (to_int64 t1) (to_int64 t2))
  let[@inline] ( * ) t1 t2 = of_int64 (I.O.( * ) (to_int64 t1) (to_int64 t2))

  let[@inline] ( ** ) t1 t2 =
    of_int64 ((I.O.( ** ) [@inlined hint]) (to_int64 t1) (to_int64 t2))
  ;;

  let[@inline] ( >= ) t1 t2 = I.O.( >= ) (to_int64 t1) (to_int64 t2)
  let[@inline] ( <= ) t1 t2 = I.O.( <= ) (to_int64 t1) (to_int64 t2)
  let[@inline] ( = ) t1 t2 = I.O.( = ) (to_int64 t1) (to_int64 t2)
  let[@inline] ( > ) t1 t2 = I.O.( > ) (to_int64 t1) (to_int64 t2)
  let[@inline] ( < ) t1 t2 = I.O.( < ) (to_int64 t1) (to_int64 t2)
  let[@inline] ( <> ) t1 t2 = I.O.( <> ) (to_int64 t1) (to_int64 t2)
  let[@inline] neg t = of_int64 (I.O.neg (to_int64 t))
  let[@inline] ( ~- ) t = of_int64 (I.O.( ~- ) (to_int64 t))

  let[@inline] ( /% ) t1 t2 =
    of_int64 ((I.O.( /% ) [@inlined hint]) (to_int64 t1) (to_int64 t2))
  ;;

  let[@inline] ( % ) t1 t2 =
    of_int64 ((I.O.( % ) [@inlined hint]) (to_int64 t1) (to_int64 t2))
  ;;

  let[@inline] ( / ) t1 t2 = of_int64 (I.O.( / ) (to_int64 t1) (to_int64 t2))

  let[@inline] ( // ) t1 t2 =
    (I.O.( // ) [@inlined hint]) (to_int64 t1) (to_int64 t2) |> unbox_float
  ;;

  let[@inline] ( land ) t1 t2 = of_int64 (I.O.( land ) (to_int64 t1) (to_int64 t2))
  let[@inline] ( lor ) t1 t2 = of_int64 (I.O.( lor ) (to_int64 t1) (to_int64 t2))
  let[@inline] ( lxor ) t1 t2 = of_int64 (I.O.( lxor ) (to_int64 t1) (to_int64 t2))
  let[@inline] lnot t = of_int64 ((I.O.lnot [@inlined hint]) (to_int64 t))
  let[@inline] ( lsl ) t x = of_int64 (I.O.( lsl ) (to_int64 t) x)
  let[@inline] ( asr ) t x = of_int64 (I.O.( asr ) (to_int64 t) x)
  let[@inline] abs t = of_int64 ((I.O.abs [@inlined hint]) (to_int64 t))
  let[@inline] ( lsr ) t x = of_int64 ((I.O.(( lsr )) [@inlined hint]) (to_int64 t) x)

  external box : int64# -> (int64[@local_opt]) @@ portable = "%box_int64"
  external unbox : (int64[@local_opt]) -> int64# @@ portable = "%unbox_int64"
end

include O

(* [min] can't be implemented in terms of [Int64.min] because that function uses
   [caml_csel_value] which forces the integers to be boxed leading to unwanted
   allocations. Instead, we have to copy the function definition and use a different
   [select] function that knows how to handle unboxed types. *)
let[@inline] min t1 t2 : t = select (t1 <= t2) t1 t2

(* Can't be implemented using [Int64.max] for the same reason as [min] *)
let[@inline] max t1 t2 : t = select (t1 >= t2) t1 t2

let[@inline] ascending t1 t2 : int =
  (I.ascending [@inlined hint]) (to_int64 t1) (to_int64 t2)
;;

let[@inline] descending t1 t2 : int =
  (I.descending [@inlined hint]) (to_int64 t1) (to_int64 t2)
;;

let[@inline] between t ~low ~high : bool =
  (I.between [@inlined hint]) (to_int64 t) ~low:(to_int64 low) ~high:(to_int64 high)
;;

let[@inline] clamp_unchecked t ~min:min_ ~max:max_ =
  (min [@inlined hint]) t max_ |> (max [@inlined hint]) min_
;;

(* Can't be implemented using [Int64.clamp_exn] for the same reason as [min] *)
let[@inline] clamp_exn t ~min ~max =
  assert (min <= max);
  (clamp_unchecked [@inlined hint]) t ~min ~max
;;

let[@inline] pp ppf t : unit = (I.pp [@inlined hint]) ppf (to_int64 t)
let[@inline] invariant t : unit = (I.invariant [@inlined hint]) (to_int64 t)
let[@inline] is_positive t = (I.is_positive [@inlined hint]) (to_int64 t)
let[@inline] is_non_negative t = (I.is_non_negative [@inlined hint]) (to_int64 t)
let[@inline] is_negative t = (I.is_negative [@inlined hint]) (to_int64 t)
let[@inline] is_non_positive t = (I.is_non_positive [@inlined hint]) (to_int64 t)
let[@inline] sign t = (I.sign [@inlined hint]) (to_int64 t)

let[@inline] to_string_hum ?delimiter t =
  (I.to_string_hum [@inlined hint]) ?delimiter (to_int64 t)
;;

let zero = of_int64 I.zero
let one = of_int64 I.one
let minus_one = of_int64 I.minus_one
let[@inline] rem t1 t2 = of_int64 ((I.rem [@inlined hint]) (to_int64 t1) (to_int64 t2))

let[@inline] round ?dir t ~to_multiple_of:t2 =
  of_int64 ((I.round [@inlined hint]) ?dir (to_int64 t) ~to_multiple_of:(to_int64 t2))
;;

(* let[@inline] round_towards_zero t ~to_multiple_of:t2 =
 *   of_int64
 *     ((I.round_towards_zero [@inlined hint]) (to_int64 t) ~to_multiple_of:(to_int64 t2))
 * ;; *)

let[@inline] round_down t ~to_multiple_of:t2 =
  of_int64 ((I.round_down [@inlined hint]) (to_int64 t) ~to_multiple_of:(to_int64 t2))
;;

let[@inline] round_up t ~to_multiple_of:t2 =
  of_int64 ((I.round_up [@inlined hint]) (to_int64 t) ~to_multiple_of:(to_int64 t2))
;;

let[@inline] round_nearest t ~to_multiple_of:t2 =
  of_int64 ((I.round_nearest [@inlined hint]) (to_int64 t) ~to_multiple_of:(to_int64 t2))
;;

let[@inline] abs t = of_int64 ((I.abs [@inlined hint]) (to_int64 t))
let[@inline] succ t = of_int64 ((I.succ [@inlined hint]) (to_int64 t))
let[@inline] pred t = of_int64 ((I.pred [@inlined hint]) (to_int64 t))
let[@inline] pow t1 t2 = of_int64 ((I.pow [@inlined hint]) (to_int64 t1) (to_int64 t2))

let[@inline] bit_and t1 t2 =
  of_int64 ((I.bit_and [@inlined hint]) (to_int64 t1) (to_int64 t2))
;;

let[@inline] bit_or t1 t2 =
  of_int64 ((I.bit_or [@inlined hint]) (to_int64 t1) (to_int64 t2))
;;

let[@inline] bit_xor t1 t2 =
  of_int64 ((I.bit_xor [@inlined hint]) (to_int64 t1) (to_int64 t2))
;;

let[@inline] bit_not t = of_int64 ((I.bit_not [@inlined hint]) (to_int64 t))
let[@inline] popcount t = of_int64 ((I.popcount [@inlined hint]) (to_int64 t))
let[@inline] shift_left t x = of_int64 ((I.shift_left [@inlined hint]) (to_int64 t) x)
let[@inline] shift_right t x = of_int64 ((I.shift_right [@inlined hint]) (to_int64 t) x)
let[@inline] of_int32_exn x = of_int64 (I.of_int32_exn x)
let[@inline] to_int32_exn t = (I.to_int32_exn [@inlined hint]) (to_int64 t)
let[@inline] of_int64_exn t = of_int64 t
let[@inline] of_nativeint_exn x = of_int64 ((I.of_nativeint_exn [@inlined hint]) x)
let[@inline] to_nativeint_exn t = (I.to_nativeint_exn [@inlined hint]) (to_int64 t)

let[@inline] of_float_unchecked f =
  of_int64 ((I.of_float_unchecked [@inlined hint]) (box_float f))
;;

let num_bits = I.num_bits
let max_value = of_int64 I.max_value
let min_value = of_int64 I.min_value
let[@inline] ( lsr ) t x = of_int64 ((I.(( lsr )) [@inlined hint]) (to_int64 t) x)
let[@inline] shift_right_logical t i = t lsr i
let[@inline] ceil_pow2 t = of_int64 ((I.ceil_pow2 [@inlined hint]) (to_int64 t))
let[@inline] floor_pow2 t = of_int64 ((I.floor_pow2 [@inlined hint]) (to_int64 t))
let[@inline] ceil_log2 t = (I.ceil_log2 [@inlined hint]) (to_int64 t) |> of_int64
let[@inline] floor_log2 t = (I.floor_log2 [@inlined hint]) (to_int64 t) |> of_int64
let[@inline] is_pow2 t = (I.is_pow2 [@inlined hint]) (to_int64 t)
let[@inline] clz t = (I.clz [@inlined hint]) (to_int64 t) |> of_int64
let[@inline] ctz t = (I.ctz [@inlined hint]) (to_int64 t) |> of_int64
let[@inline] to_nativeint t = (I.to_nativeint [@inlined hint]) (to_int64 t)
let[@inline] of_int x = of_int64 (I.of_int x)
let[@inline] of_int32 x = of_int64 (I.of_int32 x)
let[@inline] to_int32 t = (I.to_int32 [@inlined hint]) (to_int64 t)
let[@inline] to_int32_trunc t = I.to_int32_trunc (to_int64 t)
let[@inline] to_nativeint_trunc t = I.to_nativeint_trunc (to_int64 t)
let[@inline] to_int_trunc t = I.to_int_trunc (to_int64 t)
let[@inline] of_nativeint x = of_int64 (I.of_nativeint x)
let[@inline] of_bool x = of_int (Bool.to_int x)
let[@inline] bits_of_float f = of_int64 (I.bits_of_float (box_float f))
let[@inline] float_of_bits t = I.float_of_bits (to_int64 t) |> unbox_float
let[@inline] bswap16 t = of_int64 ((I.bswap16 [@inlined hint]) (to_int64 t))
let[@inline] bswap32 t = of_int64 ((I.bswap32 [@inlined hint]) (to_int64 t))
let[@inline] bswap48 t = of_int64 ((I.bswap48 [@inlined hint]) (to_int64 t))
let[@inline] bswap64 t = of_int64 (I.bswap64 (to_int64 t))

(* Inlined from [Base.Int_conversions], and modified to operate over unboxed things. *)
let[@cold] to_int_exn_failure x =
  Base.Printf.failwithf
    "conversion from int64 to int failed: %s is out of range"
    (I.to_string (to_int64 x))
    ()
;;

let[@inline] to_int_exn t =
  let r = to_int_trunc t in
  if t <> of_int r then to_int_exn_failure t;
  r
;;

(* From https://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel *)
let[@inline] [@zero_alloc] rev_bits t =
  let[@inline] swap t ~shift ~mask =
    let hi = mask land (t lsr shift) in
    let lo = (mask land t) lsl shift in
    hi lor lo
  in
  t
  |> swap ~shift:1 ~mask:#0x5555555555555555L
  |> swap ~shift:2 ~mask:#0x3333333333333333L
  |> swap ~shift:4 ~mask:#0x0F0F0F0F0F0F0F0FL
  |> bswap64
;;

module Array_index = struct
  external get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    @@ portable
    = "%array_safe_get_indexed_by_int64#"
  [@@layout_poly]

  external set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_safe_set_indexed_by_int64#"
  [@@layout_poly]

  external unsafe_get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a
    @@ portable
    = "%array_unsafe_get_indexed_by_int64#"
  [@@layout_poly]

  external unsafe_set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (t[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_unsafe_set_indexed_by_int64#"
  [@@layout_poly]
end

module Iarray_index = struct
  external get
    : ('a : any mod separable).
    ('a iarray[@local_opt]) -> (t[@local_opt]) -> 'a
    @@ portable
    = "%array_safe_get_indexed_by_int64#"
  [@@layout_poly]

  external unsafe_get
    : ('a : any mod separable).
    ('a iarray[@local_opt]) -> (t[@local_opt]) -> 'a
    @@ portable
    = "%array_unsafe_get_indexed_by_int64#"
  [@@layout_poly]
end

module Array = struct
  type ('a : bits64) t = 'a array

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
    -> int64# t
    @@ portable
    = "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"

  external unsafe_create_uninitialized
    :  len:int
    -> ('a : bits64) t
    @@ portable
    = "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"

  external unsafe_blit
    :  src:('a t[@local_opt])
    -> src_pos:int
    -> dst:('a t[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit" "caml_unboxed_int64_vect_blit"

  let sexp_of_t sexp_of__a ar =
    let open Base in
    let lst_ref = ref [] in
    for i = length ar - 1 downto 0 do
      lst_ref := sexp_of__a (get ar i) :: !lst_ref
    done;
    Sexp.List !lst_ref
  ;;

  let map (type (a : bits64) (b : bits64)) (t : a t) ~(f : a -> b) : b t =
    let len = length t in
    let r = unsafe_create_uninitialized ~len in
    for i = 0 to Base.Int.O.(len - 1) do
      let x = unsafe_get t i in
      let y = f x in
      unsafe_set r i y
    done;
    r
  ;;

  let init len ~f =
    let r = unsafe_create_uninitialized ~len in
    for i = 0 to Base.Int.O.(len - 1) do
      unsafe_set r i (f i)
    done;
    r
  ;;

  let equal equal_a (local_ s) (local_ t) =
    let l = length s in
    Int.equal l (length t)
    &&
    let rec loop i =
      match Base.Int.is_negative i with
      | true -> true
      | false -> equal_a (unsafe_get s i) (unsafe_get t i) && loop Base.Int.O.(i - 1)
    in
    loop Base.Int.O.(l - 1) [@nontail]
  ;;

  let copy t = init (length t) ~f:(fun i -> unsafe_get t i) [@nontail]
end

let%template[@alloc a = (heap, stack)] to_string n =
  (Base.Exported_for_specific_uses.Integer_to_string.int64_u_to_string [@alloc a])
    n [@exclave_if_stack a]
;;

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving globalize]

    include Shared_derived

    let to_string = to_string
    let stable_witness = Ppx_stable_witness_runtime.Stable_witness.assert_stable
  end
end

module Ref = struct
  type nonrec ('elt : bits64) t = { mutable contents : 'elt }

  let[@inline] [@zero_alloc] get t = t.contents
  let[@inline] [@zero_alloc] set t x = t.contents <- x
  let[@inline] [@zero_alloc] add t x = set t (get t + x)
  let[@inline] create contents = { contents }
  let[@inline] [@zero_alloc] create_local contents = exclave_ { contents }
  let[@inline] [@zero_alloc] incr t = add t #1L
  let[@inline] [@zero_alloc] decr t = add t (-#1L)

  module O = struct
    let ref = create_local
    let[@inline] [@zero_alloc] ( ! ) t = get t
    let[@inline] [@zero_alloc] ( := ) t x = set t x
    let[@inline] [@zero_alloc] ( += ) t x = add t x
    let[@inline] [@zero_alloc] ( |= ) t x : unit = set t (get t lor x)
    let[@inline] [@zero_alloc] incr t : unit = incr t
    let[@inline] [@zero_alloc] decr t : unit = decr t
  end
end

let[@inline] for_loop_known_to_run_at_least_once ~start_incl ~end_excl ~(local_ f) =
  let rec loop i = exclave_
    (f [@inlined hint]) i;
    let i = i + #1L in
    (* important: <>, NOT <. In particular, for inclusive for, [end_excl] might wrap (and
       in fact be less than all i.) The point is that [end_excl] is the exact number we
       don't want to loop on. *)
    if i <> end_excl then loop i
  in
  loop start_incl [@nontail]
;;

let[@inline] for_loop ~start_incl ~end_excl ~(local_ f) =
  if start_incl < end_excl
  then for_loop_known_to_run_at_least_once ~start_incl ~end_excl ~f
;;

let[@inline] for_loop_incl ~start_incl ~end_incl ~(local_ f) =
  if start_incl <= end_incl
  then (
    let end_excl = end_incl + #1L in
    for_loop_known_to_run_at_least_once ~start_incl ~end_excl ~f)
;;

let[@inline] range n ~(local_ f) = for_loop ~start_incl:#0L ~end_excl:n ~f

let[@inline] rec range_rev_loop n ~f =
  let n = n - #1L in
  (f [@inlined hint]) n;
  if n > #0L then range_rev_loop n ~f
;;

let[@inline] range_rev n ~f = if n > #0L then range_rev_loop n ~f

include struct
  open Base_quickcheck

  let%template quickcheck_generator =
    (Generator.Via_thunk.map [@mode portable]) Generator.int64 ~f:(fun f () ->
      of_int64 (f ()))
  ;;

  let%template quickcheck_observer =
    (Observer.Via_thunk.unmap [@mode portable]) Observer.int64 ~f:(fun f () ->
      to_int64 (f ()))
  ;;

  let quickcheck_shrinker = Shrinker.atomic
end

module Hex_unsigned = struct
  module Bool = Base.Bool
  module Bytes = Base.Bytes
  module Char = Base.Char
  module Int = Base.Int
  module Nothing = Base.Nothing
  module Sexp = Base.Sexp
  module String = Base.String

  module To_string_config = struct
    module Case = struct
      type t =
        | Lowercase
        | Uppercase
      [@@deriving equal ~localize, sexp_of]
    end

    type t =
      { prefix_with_0x : bool
      ; render_letters : Case.t
      }
    [@@deriving box, equal ~localize, sexp_of]

    let default = { prefix_with_0x = true; render_letters = Lowercase }
    let[@inline] starting_offset t = of_int (Bool.to_int t.#prefix_with_0x) * #2L

    let[@inline] ascii_a t =
      let a =
        match t.#render_letters with
        | Lowercase -> 'a'
        | Uppercase -> 'A'
      in
      of_int (Char.to_int a)
    ;;
  end

  module Private = struct
    type digits_to_process = t

    let[@inline] ceil_div a b =
      let open O in
      (a + (b - #1L)) / b
    ;;

    (* [max 1] because there is always at least one digit printed, even if the input is 0. *)
    let[@inline] digits_to_process t ~max_digits =
      let open O in
      clamp_unchecked ~min:#1L ~max:max_digits (ceil_div (#64L - clz t) #4L)
    ;;

    (* 2 bytes for the "0x" prefix. plus one byte per 4-bit digit. *)
    let[@inline] to_string_required_length ~config ~digits_to_process =
      let open O in
      to_int_trunc (To_string_config.starting_offset config + digits_to_process)
    ;;

    let[@inline never] to_string_into
      t
      out
      ~(config : To_string_config.t#)
      ~digits_to_process
      =
      let open O in
      let set = Bytes.unsafe_set in
      if config.#prefix_with_0x
      then (
        set out 0 '0';
        set out 1 'x');
      let starting_offset = To_string_config.starting_offset config in
      let ascii_a = To_string_config.ascii_a config in
      range_rev digits_to_process ~f:(fun i ->
        let digit = (t lsr to_int_trunc (i * #4L)) land #0xFL in
        let output_index = starting_offset + digits_to_process - i - #1L in
        let ascii_zero = of_int (Char.to_int '0') in
        let char_as_int =
          select (digit >= #10L) (digit + ascii_a - #10L) (digit + ascii_zero)
        in
        set
          out
          (to_int_trunc output_index)
          (Char.unsafe_of_int (to_int_trunc char_as_int)))
      [@nontail]
    ;;

    let[@cold] malformed_hex_string s =
      match
        Base.raise_s
          [%message
            "malformed unsigned hex string" ~_:([%globalize: String.t] s : String.t)]
      with
      | (_ : Nothing.t) -> .
    ;;

    let[@inline never] of_string (s : string @ local) ~max_digits =
      let open O in
      let open Ref.O in
      let get = String.unsafe_get in
      let length = String.length s in
      if Int.O.(length < 3) || Char.( <> ) (get s 0) '0' || Char.( <> ) (get s 1) 'x'
      then malformed_hex_string s;
      let r = Ref.create_local #0L in
      (* [valid] would be better off as a bool ref (where 1=true and 0=false), but it's
         annoying to work with bits64 and value refs in the same scope. *)
      let valid = Ref.create_local #1L in
      let num_digits = of_int length - #2L in
      range num_digits ~f:(fun i ->
        let index = i + #2L in
        let char = get s (to_int_trunc index) in
        let int = of_int (Char.to_int char) in
        let ascii_lowercase_a = of_int (Char.to_int 'a') in
        let ascii_uppercase_a = of_int (Char.to_int 'A') in
        let ascii_zero = of_int (Char.to_int '0') in
        let open Bool.Non_short_circuiting in
        let lowercase_ascii = int >= ascii_lowercase_a && int < ascii_lowercase_a + #6L in
        let uppercase_ascii = int >= ascii_uppercase_a && int < ascii_uppercase_a + #6L in
        let decimal_digit = int >= ascii_zero && int < ascii_zero + #10L in
        let digit_valid = lowercase_ascii || uppercase_ascii || decimal_digit in
        let digit =
          int - ascii_zero
          |> select lowercase_ascii (int - ascii_lowercase_a + #10L)
          |> select uppercase_ascii (int - ascii_uppercase_a + #10L)
        in
        let output_bit_offset = (num_digits - #1L - i) * #4L in
        r := !r lor (digit lsl to_int_trunc output_bit_offset);
        valid := !valid land of_bool digit_valid);
      let malformed = !valid = #0L lor of_bool (num_digits > max_digits) in
      if malformed then malformed_hex_string s;
      !r [@nontail]
    ;;

    let[@cold] t_of_sexp_failed sexp =
      Base.raise_s
        [%message "expected Atom, got List" ~_:([%globalize: Sexp.t] sexp : Sexp.t)]
    ;;

    let[@inline] t_of_sexp (sexp : Sexp.t @ local) ~max_digits =
      match sexp with
      | Atom s -> of_string s ~max_digits
      | List _ ->
        (match t_of_sexp_failed sexp with
         | (_ : Nothing.t) -> .)
    ;;
  end

  let[@inline] max_digits () = #16L

  module Local = struct
    type nonrec t = t

    let compare = Shared_derived.compare
    let hash = Shared_derived.hash
    let hash_fold_t = Shared_derived.hash_fold_t
    let[@inline] of_string s = Private.of_string s ~max_digits:(max_digits ())
    let[@inline] t_of_sexp sexp = Private.t_of_sexp sexp ~max_digits:(max_digits ())

    let[@inline never] to_string_custom t ~config = exclave_
      let digits_to_process = Private.digits_to_process t ~max_digits:#16L in
      let bytes =
        Bytes.create_local (Private.to_string_required_length ~config ~digits_to_process)
      in
      Private.to_string_into t bytes ~config ~digits_to_process;
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
    ;;

    let[@inline] to_string t = exclave_
      to_string_custom t ~config:To_string_config.(unbox default)
    ;;

    let[@inline] sexp_of_t t : Sexp.t = exclave_ Atom (to_string t)
  end

  type nonrec t = t

  let compare = Local.compare
  let hash = Shared_derived.hash
  let hash_fold_t = Shared_derived.hash_fold_t
  let[@inline] of_string s = Private.of_string s ~max_digits:(max_digits ())
  let[@inline] t_of_sexp sexp = Private.t_of_sexp sexp ~max_digits:(max_digits ())

  let[@inline never] to_string_custom t ~config =
    let digits_to_process = Private.digits_to_process t ~max_digits:#16L in
    let bytes =
      Bytes.create (Private.to_string_required_length ~config ~digits_to_process)
    in
    Private.to_string_into t bytes ~config ~digits_to_process;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes [@nontail]
  ;;

  let[@inline] to_string t : string =
    to_string_custom t ~config:To_string_config.(unbox default)
  ;;

  let[@inline] sexp_of_t t : Sexp.t = Atom (to_string t)
end

module Option = struct
  type nonrec t = t
  type value = t

  let none = min_value
  let[@inline] is_none t = t = none
  let[@inline] is_some t = not (is_none t)

  let[@inline] some t =
    assert (is_some t);
    t
  ;;

  let[@inline] unchecked_some t = t
  let[@inline] some_is_representable t = is_some t
  let[@inline] value t ~default = select (is_some t) t default
  let[@inline] unchecked_value t = t
  let[@inline] to_option t = if is_some t then Some (box t) else None

  let[@inline] of_option t =
    match t with
    | None -> none
    | Some t -> some (unbox t)
  ;;

  let[@inline] to_local_option t = exclave_ if is_some t then Some (box t) else None
  let[@inline] of_local_option t = of_option t

  module Optional_syntax = struct
    module Optional_syntax = struct
      let[@inline] is_none t = is_none t
      let[@inline] unsafe_value t = unchecked_value t
    end
  end
end
