[@@@ocaml.flambda_o3]

open! Core
open Unboxed
include Bitset_intf

(* This bitset stores the number of valid bits (aka [bit_capacity]) in the first word of
   the [Bytes.t], and the bits themselves in the rest of the space.

   [t.bytes] is [8 + 8 * complete_words + 8] bytes long. There's

   - 8 bytes at the start for the bit capacity,
   - [complete_words] worth of 64-bit bitset elements, and
   - one word at the end of which a (possibly empty) subset is part of the bitset.

   [last_word_bitmask] is a bitmask selecting the valid bits of that last word.

   It is invariant that every bit in the final word of [t.bytes], not selected by
   [last_word_bitmask], is 0. *)

module Stable = struct
  module V1 = struct
    type t = Bytes.Stable.V1.t [@@deriving bin_io, globalize, stable_witness]

    let%expect_test _ =
      Core.print_endline [%bin_digest: t];
      [%expect {| 06c5811b990697b0a0c71e285a10e7d4 |}]
    ;;
  end
end

include Stable.V1

open struct
  module Bytes = struct
    include Bytes

    let blit ~(src @ local read) ~src_pos ~(dst @ local) ~dst_pos ~len =
      let src : Bytes.t @ local read_write = Obj.magic_read_write_uncontended src in
      Bytes.unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len [@nontail]
    ;;
  end
end

(* The byte offset of where the bitset data itself lives in the [Bytes.t] *)
let start_of_data = 8

(* Direct access to the words of the underlying [Bytes.t], without doing anything to
   handle the odd bits at the end. *)
module Direct : sig @@ portable
  val unsafe_get_64 : local_ Bytes.t @ read -> byte_index:int -> I64.t
  val unsafe_set_64 : local_ Bytes.t -> byte_index:int -> I64.t -> unit
  val unsafe_get_32 : local_ Bytes.t @ read -> byte_index:int -> I32.t
end = struct
  external unsafe_get_64
    :  local_ Bytes.t @ read
    -> int
    -> int64
    @@ portable
    = "%caml_bytes_get64u"

  external unsafe_set_64
    :  local_ Bytes.t
    -> int
    -> int64
    -> unit
    @@ portable
    = "%caml_bytes_set64u"

  external unsafe_get_32
    :  local_ Bytes.t @ read
    -> int
    -> int32
    @@ portable
    = "%caml_bytes_get32u"

  (* These look like they allocate, but thanks to some compiler magic they actually don't. *)

  let[@inline] unsafe_get_64 bytes ~byte_index =
    I64.of_int64 (unsafe_get_64 bytes (byte_index + start_of_data))
  ;;

  let[@inline] unsafe_set_64 bytes ~byte_index x =
    unsafe_set_64 bytes (byte_index + start_of_data) (I64.box x)
  ;;

  let[@inline] unsafe_get_32 bytes ~byte_index =
    I32.unbox (unsafe_get_32 bytes (byte_index + start_of_data))
  ;;
end

let bit_capacity_byte_index = -start_of_data

let[@inline] bit_capacity t =
  I64.to_int_trunc (Direct.unsafe_get_64 t ~byte_index:bit_capacity_byte_index)
;;

let[@inline] set_bit_capacity t v =
  Direct.unsafe_set_64 t ~byte_index:bit_capacity_byte_index (I64.of_int v)
;;

let[@inline] byte_index_of_bit ~bit = (bit lsr 6) lsl 3
let[@inline] byte_index_of_word_index ~word_index = word_index lsl 3
let[@inline] bit_index_of_word_index ~word_index = word_index lsl 6
let[@inline] mask ~bit = I64.( lsl ) #1L (bit land 63)

(* Describes how to iterate through a bitset.

   In general, one checks every word up to [complete_words], then uses [last_word_bitmask]
   to mask off just the important bits of the final word, as the number of bits in a
   bitset might not be a perfect multiple of 64. *)
module Bounds = struct
  type t =
    { complete_words : int (* The number of complete valid 64-bit words in [bytes]. *)
    ; last_word_bitmask : I64.t
    (* A mask of the valid bits in the last word of [bytes]. *)
    }
end

let[@inline] bounds t : Bounds.t =
  let bit_capacity = bit_capacity t in
  exclave_
  { complete_words = bit_capacity lsr 6
  ; last_word_bitmask = I64.(mask ~bit:bit_capacity - #1L)
  }
;;

module Masked : sig @@ portable
  (* The inclusive end bound of a for loop through each masked 64-bit word in the bitset.

     The last iteration of that for loop points to a word with only some bits as
     officially part of the bitset. You probably want to use [unsafe_set] to mutate the
     bitset, as it will automatically mask out those invalid bits on the last iteration. *)
  val for_loop_end : local_ Bounds.t -> int [@@zero_alloc]

  (* Indexing out of bounds is undefined behavior. Setting into the last word (the one at
     index [complete_words]) automatically masks. *)
  val unsafe_set : local_ Bytes.t -> local_ Bounds.t -> word_index:int -> I64.t -> unit
end = struct
  (* For loops through masked values need to include one extra word at the end, to handle
     the last word bitmask. Remember that for loop bounds are inclusive... *)
  let[@inline] for_loop_end (t : Bounds.t) = t.complete_words

  let[@inline] unsafe_set bytes (bounds : Bounds.t) ~word_index v =
    let%tydi { complete_words; last_word_bitmask } = bounds in
    let mask =
      I64.select (word_index < complete_words) #0xFFFF_FFFF_FFFF_FFFFL last_word_bitmask
    in
    let v = I64.(v land mask) in
    let byte_index = byte_index_of_word_index ~word_index in
    Direct.unsafe_set_64 bytes ~byte_index v
  ;;
end

let rec is_empty_loop bytes ~for_loop_end ~word_index =
  let byte_index = byte_index_of_word_index ~word_index in
  let word = Direct.unsafe_get_64 bytes ~byte_index in
  let word_empty = I64.(word = #0L) in
  if word_index = for_loop_end
  then word_empty
  else word_empty && is_empty_loop bytes ~for_loop_end ~word_index:(word_index + 1)
;;

(* Returns 0 if reading out of bounds of the bitset, branchfree. *)
let[@inline] read_word_for_eq bytes (bounds : Bounds.t) ~word_index =
  let effective_index = Int.min word_index bounds.complete_words in
  let effective_byte_index = byte_index_of_word_index ~word_index:effective_index in
  Direct.unsafe_get_64 bytes ~byte_index:effective_byte_index
  |> I64.select (word_index > bounds.complete_words) #0L
;;

let rec equal_loop bytes1 bounds1 bytes2 bounds2 ~for_loop_end ~word_index =
  let word1 = read_word_for_eq bytes1 bounds1 ~word_index in
  let word2 = read_word_for_eq bytes2 bounds2 ~word_index in
  let word_eq = I64.(word1 = word2) in
  if word_index = for_loop_end
  then word_eq
  else
    word_eq
    && equal_loop bytes1 bounds1 bytes2 bounds2 ~for_loop_end ~word_index:(word_index + 1)
;;

let equal__local t1 t2 =
  let t1_bounds = bounds t1 in
  let t2_bounds = bounds t2 in
  (* [read_word_for_eq] return 0 if accessing out of bounds, so walking through the max of
     the two sets has the semantics we want here. Those semantics being: pretend both
     bitsets are infinite length and any data beyond the true length is 0. *)
  let for_loop_end =
    Int.max (Masked.for_loop_end t1_bounds) (Masked.for_loop_end t2_bounds)
  in
  equal_loop t1 t1_bounds t2 t2_bounds ~for_loop_end ~word_index:0 [@nontail]
;;

let equal t1 t2 = equal__local t1 t2

let rec compare_loop bytes1 bounds1 bytes2 bounds2 ~for_loop_end ~word_index =
  let word1 = read_word_for_eq bytes1 bounds1 ~word_index in
  let word2 = read_word_for_eq bytes2 bounds2 ~word_index in
  let word_eq = I64.equal word1 word2 in
  if word_eq
  then
    if word_index = for_loop_end
    then 0
    else
      compare_loop bytes1 bounds1 bytes2 bounds2 ~for_loop_end ~word_index:(word_index + 1)
  else I64.compare word1 word2
;;

let compare__local t1 t2 =
  let t1_bounds = bounds t1 in
  let t2_bounds = bounds t2 in
  (* [read_word_for_eq] return 0 if accessing out of bounds, so walking through the max of
     the two sets has the semantics we want here. Those semantics being: pretend both
     bitsets are infinite length and any data beyond the true length is 0. *)
  let for_loop_end =
    Int.max (Masked.for_loop_end t1_bounds) (Masked.for_loop_end t2_bounds)
  in
  compare_loop t1 t1_bounds t2 t2_bounds ~for_loop_end ~word_index:0 [@nontail]
;;

let compare t1 t2 = compare__local t1 t2

let[@cold] [@zero_alloc assume] invariant t =
  let byte_len = Bytes.length t - 8 in
  let required_bytes = (bit_capacity t + 8 - 1) / 8 in
  if byte_len < required_bytes
  then
    raise_s
      [%message
        "bytes not big enough to store required bits"
          (required_bytes : int)
          (byte_len : int)];
  let bounds = bounds t in
  [%test_result: int]
    byte_len
    ~expect:(byte_index_of_word_index ~word_index:(bounds.complete_words + 1));
  let last_word =
    Direct.unsafe_get_64
      t
      ~byte_index:(byte_index_of_word_index ~word_index:bounds.complete_words)
  in
  let last_word_bits_outside_bitmask =
    I64.(last_word land lnot bounds.last_word_bitmask)
  in
  (* don't allocate the Int64.t unless the check fails *)
  if I64.(last_word_bits_outside_bitmask <> #0L)
  then [%test_result: Int64.t] I64.(box last_word_bits_outside_bitmask) ~expect:0L
;;

let[@inline] invariant_in_test t = if am_running_test then invariant t

let[@cold] bounds_check_failed t bit =
  raise_s
    [%message
      "bitset accessed out of bounds" ~capacity:(bit_capacity t : int) (bit : int)]
;;

let[@inline] bounds_check t bit =
  let open Bool.Non_short_circuiting in
  if bit < 0 || bit >= bit_capacity t then bounds_check_failed t bit
;;

module T = struct
  let[@inline] unsafe_mem t bit =
    let byte_index = byte_index_of_bit ~bit in
    let v = Direct.unsafe_get_64 t ~byte_index in
    let mask = mask ~bit in
    I64.(v land mask <> #0L)
  ;;

  let[@inline] unsafe_add t bit =
    let byte_index = byte_index_of_bit ~bit in
    let v = Direct.unsafe_get_64 t ~byte_index in
    let mask = mask ~bit in
    let v = I64.(v lor mask) in
    Direct.unsafe_set_64 t ~byte_index v
  ;;

  let[@inline] unsafe_remove t bit =
    let byte_index = byte_index_of_bit ~bit in
    let v = Direct.unsafe_get_64 t ~byte_index in
    let mask = mask ~bit in
    let v = I64.(v land lnot mask) in
    Direct.unsafe_set_64 t ~byte_index v
  ;;

  let[@inline] unsafe_assign t bit x : unit =
    let byte_index = byte_index_of_bit ~bit in
    let w = Direct.unsafe_get_64 t ~byte_index in
    let mask = mask ~bit in
    let v = I64.(select x (w lor mask) (w land lnot mask)) in
    Direct.unsafe_set_64 t ~byte_index v
  ;;

  let[@inline] mem t bit : bool =
    bounds_check t bit;
    unsafe_mem t bit
  ;;

  (* It feels not worthwhile to eat a well-predicted branch to check for invariant
     violations in test after an [add] or [remove]. These two functions are simple enough
     that the bounds check feels sufficient, and they're small enough that they're likely
     to appear in inner loops. *)

  let[@inline] add t bit : unit =
    bounds_check t bit;
    unsafe_add t bit
  ;;

  let[@inline] remove t bit : unit =
    bounds_check t bit;
    unsafe_remove t bit
  ;;

  let[@inline] assign t bit x : unit =
    bounds_check t bit;
    unsafe_assign t bit x
  ;;

  let union_into ~dst ~src =
    let src_end = Masked.for_loop_end (bounds src) in
    let dst_bounds = bounds dst in
    let dst_end = Masked.for_loop_end dst_bounds in
    let both_end = Int.min src_end dst_end in
    for index = both_end + 1 to src_end do
      let byte_index = index lsl 3 in
      if I64.( <> ) (Direct.unsafe_get_64 src ~byte_index) #0L
      then
        raise_s
          [%message
            "Bitset.union_into src must not have any elements set beyond the size of dst"]
    done;
    if dst_end <= src_end
    then (
      let%tydi { complete_words = word_index; last_word_bitmask } = dst_bounds in
      let byte_index = byte_index_of_word_index ~word_index in
      let last_src_word = Direct.unsafe_get_64 src ~byte_index in
      let out_of_bounds_bitmask = I64.(lnot) last_word_bitmask in
      if I64.(last_src_word land out_of_bounds_bitmask <> #0L)
      then
        raise_s
          [%message
            "Bitset.union_into src must not have any elements set beyond the size of dst"]);
    for word_index = 0 to both_end do
      let byte_index = byte_index_of_word_index ~word_index in
      let src_word = Direct.unsafe_get_64 src ~byte_index in
      let dst_word = Direct.unsafe_get_64 dst ~byte_index in
      let v = I64.(src_word lor dst_word) in
      (* Does not need to be masked because we just checked that [src_word] does not have
         any bits set outside of the valid bounds of [dst]. *)
      Direct.unsafe_set_64 dst ~byte_index v
    done;
    invariant_in_test dst
  ;;

  let inter_into ~dst ~src =
    let src_end = Masked.for_loop_end (bounds src) in
    let dst_end = Masked.for_loop_end (bounds dst) in
    let both_end = Int.min src_end dst_end in
    for word_index = 0 to both_end do
      let byte_index = byte_index_of_word_index ~word_index in
      let src_word = Direct.unsafe_get_64 src ~byte_index in
      let dst_word = Direct.unsafe_get_64 dst ~byte_index in
      let v = I64.(src_word land dst_word) in
      (* It's safe to use a direct [set] here because [land] won't set any bits that
         weren't already set, and everything after the end of the valid set of bits is 0. *)
      Direct.unsafe_set_64 dst ~byte_index v
    done;
    for word_index = both_end + 1 to dst_end do
      let byte_index = byte_index_of_word_index ~word_index in
      Direct.unsafe_set_64 dst ~byte_index #0L
    done;
    invariant_in_test dst
  ;;

  let remove_all ~dst ~src =
    let src_end = Masked.for_loop_end (bounds src) in
    let dst_end = Masked.for_loop_end (bounds dst) in
    let both_end = Int.min src_end dst_end in
    for word_index = 0 to both_end do
      let byte_index = byte_index_of_word_index ~word_index in
      let src_word = Direct.unsafe_get_64 src ~byte_index in
      let dst_word = Direct.unsafe_get_64 dst ~byte_index in
      let v = I64.(dst_word land lnot src_word) in
      (* It's safe to use a direct [set] here because [land] won't set any bits that
         weren't already set, and everything after the end of the valid set of bits is 0. *)
      Direct.unsafe_set_64 dst ~byte_index v
    done;
    invariant_in_test dst
  ;;

  let complement_inplace t =
    let%tydi { complete_words; last_word_bitmask } = bounds t in
    (* Directly complement every complete word, then complement and mask the final
       incomplete word. *)
    for word_index = 0 to complete_words - 1 do
      let byte_index = byte_index_of_word_index ~word_index in
      let v = Direct.unsafe_get_64 t ~byte_index in
      let v = I64.(lnot v) in
      Direct.unsafe_set_64 t ~byte_index v
    done;
    let word_index = complete_words in
    let byte_index = byte_index_of_word_index ~word_index in
    let last_v = Direct.unsafe_get_64 t ~byte_index in
    let last_v = I64.(lnot last_v land last_word_bitmask) in
    Direct.unsafe_set_64 t ~byte_index last_v;
    invariant_in_test t
  ;;

  let rec is_subset_loop bytes1 bounds1 bytes2 bounds2 ~for_loop_end ~word_index =
    let word1 = read_word_for_eq bytes1 bounds1 ~word_index in
    let word2 = read_word_for_eq bytes2 bounds2 ~word_index in
    (* A subset_of B <-> A intersect B = A *)
    let word_is_subset = I64.(word1 land lnot word2 = #0L) in
    if word_index = for_loop_end
    then word_is_subset
    else
      word_is_subset
      && is_subset_loop
           bytes1
           bounds1
           bytes2
           bounds2
           ~for_loop_end
           ~word_index:(word_index + 1)
  ;;

  let is_subset t1 ~of_:t2 =
    let t1_bounds = bounds t1 in
    let t2_bounds = bounds t2 in
    (* [read_word_for_eq] return 0 if accessing out of bounds, so walking through the max
       of the two sets has the semantics we want here. Those semantics being: pretend both
       bitsets are infinite length and any data beyond the true length is 0. *)
    let for_loop_end =
      Int.max (Masked.for_loop_end t1_bounds) (Masked.for_loop_end t2_bounds)
    in
    is_subset_loop t1 t1_bounds t2 t2_bounds ~for_loop_end ~word_index:0 [@nontail]
  ;;

  let is_empty t =
    let bounds = bounds t in
    let for_loop_end = Masked.for_loop_end bounds in
    is_empty_loop t ~for_loop_end ~word_index:0 [@nontail]
  ;;

  (* We optimize for small bitsets and minimize branching. For large bitsets we could
     consider early exit when non-zero intersection is detected. *)
  let[@zero_alloc] is_inter_empty a b =
    let r = I64.Ref.create_local #0L in
    let open I64.Ref.O in
    let open I64.O in
    let a_complete_words = (bounds a).complete_words in
    let b_complete_words = (bounds b).complete_words in
    (* this also iterates through the word at the end, not just the complete words *)
    let shared_complete_words = Int.min a_complete_words b_complete_words in
    for word_index = 0 to shared_complete_words do
      let byte_index = byte_index_of_word_index ~word_index in
      let a = Direct.unsafe_get_64 a ~byte_index in
      let b = Direct.unsafe_get_64 b ~byte_index in
      r := !r lor (a land b)
    done;
    !r = #0L
  ;;

  let[@zero_alloc] num_members t =
    let count = I64.Ref.create_local #0L in
    let open I64.Ref.O in
    let%tydi { complete_words; last_word_bitmask } = bounds t in
    for word_index = 0 to complete_words - 1 do
      let byte_index = byte_index_of_word_index ~word_index in
      let v = Direct.unsafe_get_64 t ~byte_index in
      count := I64.O.(!count + I64.popcount v)
    done;
    let word_index = complete_words in
    let byte_index = byte_index_of_word_index ~word_index in
    let v = Direct.unsafe_get_64 t ~byte_index in
    let v = I64.(v land last_word_bitmask) in
    I64.O.(!count + I64.popcount v) |> I64.to_int_trunc
  ;;

  (*=A mask where bit b, where bit indices are in [0, 32), is set iff b < i % 32.
     When i % 32 = 0, returns a mask where all bits are set. *)
  let[@inline] bits_set_until i =
    (* [i land 31] is more efficient than [i % 32] for nonnegative i; [32-i] might be
       negative but this will properly "wrap" it. *)
    let i = (32 - i) land 31 in
    I32.( lsr ) #0xffff_ffffl i
  ;;

  (*=A mask where bit b, where bit indices are in [0, 32), is set iff b >= i % 32.
     When i % 32 = 0, returns a mask where all bits are set. *)
  let[@inline] bits_set_from i =
    let i = i land 31 (* equivalent to [% 32] for non-negative values *) in
    I32.( lsl ) #0xffff_ffffl i
  ;;

  let[@inline] num_members_in_word_range bytes ~start_incl ~end_excl =
    let for_start = bits_set_from start_incl in
    let for_end = bits_set_until end_excl in
    let mask = I32.(for_start land for_end) in
    let word_i = start_incl lsr 5 (* [/ 32], for positive values *) in
    let word = Direct.unsafe_get_32 bytes ~byte_index:(word_i * 4) in
    I32.(popcount (word land mask))
  ;;

  let[@cold] oob_start t ~start =
    raise_s
      [%message
        "out of bounds"
          ~start:([%globalize: int Maybe_bound.t] start : int Maybe_bound.t)
          ~capacity:(bit_capacity t : int)]
  ;;

  let[@cold] oob_end t ~end_ =
    raise_s
      [%message
        "out of bounds"
          ~end_:([%globalize: int Maybe_bound.t] end_ : int Maybe_bound.t)
          ~capacity:(bit_capacity t : int)]
  ;;

  (* This function works in 32-bit chunks to avoid possibly allocating and generally to
     reduce latency compared to using 64-bit chunks. This comes at the cost of throughput,
     but the initial use case of this function was in the context of scanning only small
     ranges at a time (roughly 10 integers wide), so throughput was not the primary
     concern. *)
  let num_members_in_range t ~(local_ start) ~(local_ end_) =
    let bit_capacity = bit_capacity t in
    let start_incl : int =
      match start with
      | Incl n -> n
      | Excl n -> n + 1
      | Unbounded -> 0
    in
    let end_excl =
      match end_ with
      | Incl n -> n + 1
      | Excl n -> n
      | Unbounded -> bit_capacity
    in
    if start_incl >= bit_capacity then oob_start t ~start;
    if end_excl > bit_capacity then oob_end t ~end_;
    let start_i = ref start_incl in
    let count = ref 0 in
    while !start_i < end_excl do
      let start_word_end = ((!start_i / 32) + 1) * 32 in
      let end_i = Int.min start_word_end end_excl in
      count
      := !count
         + (num_members_in_word_range t ~start_incl:!start_i ~end_excl:end_i
            |> I32.to_int_trunc);
      start_i := start_word_end
    done;
    !count
  ;;

  let fill_data t c : unit =
    Bytes.fill t ~pos:start_of_data ~len:(Bytes.length t - start_of_data) c
  ;;

  let clear t =
    fill_data t '\x00';
    invariant_in_test t
  ;;

  let set_all t =
    fill_data t '\xFF';
    (* mask off the extra bits at the end of the last word *)
    let bounds = bounds t in
    let word_index = bounds.complete_words in
    Masked.unsafe_set t bounds ~word_index #0xFFFF_FFFF_FFFF_FFFFL;
    invariant_in_test t
  ;;

  let bytes_len ~bit_capacity =
    if bit_capacity < 0
    then raise_s [%message "Bitset.create got negative len" ~len:(bit_capacity : int)];
    (* round up to a multiple of complete words *)
    let word_capacity = (bit_capacity + 64 - 1) lsr 6 in
    (* We need an extra word at the end if the size in bits is an exact multiple of 64, to
       make iteration that always reads a (masked) word at the end never access out of
       bounds. *)
    let bit_size_is_multiple_of_word_size = word_capacity lsl 6 = bit_capacity in
    let word_capacity = word_capacity + Bool.to_int bit_size_is_multiple_of_word_size in
    start_of_data + byte_index_of_word_index ~word_index:word_capacity
  ;;

  let create ~len:bit_capacity =
    let t = Bytes.create (bytes_len ~bit_capacity) in
    set_bit_capacity t bit_capacity;
    clear t;
    t
  ;;

  let create_local ~len:bit_capacity = exclave_
    let t = Bytes.create_local (bytes_len ~bit_capacity) in
    set_bit_capacity t bit_capacity;
    clear t;
    t
  ;;

  let copy_and_truncate_into src ~dst =
    let blit_len =
      Int.min (Bytes.length src - start_of_data) (Bytes.length dst - start_of_data)
    in
    Bytes.blit ~src ~src_pos:start_of_data ~dst ~dst_pos:start_of_data ~len:blit_len;
    let src_bounds = bounds src in
    let dst_bounds = bounds dst in
    let src_complete_words = src_bounds.complete_words in
    let dst_complete_words = dst_bounds.complete_words in
    (* Copy the last word with masking if [src] is at least as long as [dst]. Don't want
       to index out of bounds if [src] is shorter. *)
    if src_complete_words >= dst_complete_words
    then (
      let word_index = dst_complete_words in
      let byte_index = byte_index_of_word_index ~word_index in
      let last_word = Direct.unsafe_get_64 dst ~byte_index in
      Masked.unsafe_set dst dst_bounds ~word_index last_word);
    invariant_in_test dst
  ;;

  let copy_and_truncate t ~new_len =
    let dst = create ~len:new_len in
    copy_and_truncate_into t ~dst;
    dst
  ;;

  let copy_and_truncate_local t ~new_len = exclave_
    let dst = create_local ~len:new_len in
    copy_and_truncate_into t ~dst;
    dst
  ;;

  let grow t ~new_len:dst_len =
    if dst_len < bit_capacity t
    then
      raise_s
        [%message "Bitset.grow got new_len smaller than capacity of t" (dst_len : int)];
    copy_and_truncate t ~new_len:dst_len
  ;;

  let grow_local t ~new_len:dst_len =
    if dst_len < bit_capacity t
    then
      raise_s
        [%message
          "Bitset.grow_local got new_len smaller than capacity of t" (dst_len : int)];
    exclave_ copy_and_truncate_local t ~new_len:dst_len
  ;;

  let copy t = copy_and_truncate t ~new_len:(bit_capacity t)
  let copy_local t = exclave_ copy_and_truncate_local t ~new_len:(bit_capacity t)

  let union a b =
    let dst = copy_and_truncate a ~new_len:(Int.max (bit_capacity a) (bit_capacity b)) in
    union_into ~dst ~src:b;
    dst
  ;;

  let union_local a b = exclave_
    let dst =
      copy_and_truncate_local a ~new_len:(Int.max (bit_capacity a) (bit_capacity b))
    in
    union_into ~dst ~src:b;
    dst
  ;;

  let inter a b =
    let dst = copy_and_truncate a ~new_len:(Int.min (bit_capacity a) (bit_capacity b)) in
    inter_into ~dst ~src:b;
    dst
  ;;

  let inter_local a b = exclave_
    let dst =
      copy_and_truncate_local a ~new_len:(Int.min (bit_capacity a) (bit_capacity b))
    in
    inter_into ~dst ~src:b;
    dst
  ;;

  let diff a b =
    let dst = copy a in
    remove_all ~dst ~src:b;
    dst
  ;;

  let diff_local a b = exclave_
    let dst = copy_local a in
    remove_all ~dst ~src:b;
    dst
  ;;

  let complement t =
    let dst = copy t in
    complement_inplace dst;
    dst
  ;;

  let complement_local t = exclave_
    let dst = copy_local t in
    complement_inplace dst;
    dst
  ;;

  (*=When [t] is sparsely populated the below performs considerably better than the more
     straight-forward loop:

     {[ for i = 0 to capacity t do if unsafe_mem t i then f i done }]

       As it becomes more dense, this can underperform. *)
  let iter_set t ~f =
    let bounds = bounds t in
    for word_index = 0 to Masked.for_loop_end bounds do
      let byte_index = byte_index_of_word_index ~word_index in
      let bit_index = bit_index_of_word_index ~word_index in
      let w = Direct.unsafe_get_64 t ~byte_index in
      let w = ref (I64.box w) in
      while Int64.(!w <> 0L) do
        let wi = Int64.ctz !w |> Int64.to_int_trunc in
        (f [@zero_alloc assume]) (bit_index + wi);
        w := Int64.(!w land lnot (one lsl wi))
      done
    done
  ;;

  let[@zero_alloc] rec fold_set_loop_this_word t acc ~f ~w ~bit_index =
    if I64.(w = #0L)
    then acc
    else (
      let wi = I64.ctz w |> I64.to_int_trunc in
      exclave_
      fold_set_loop_this_word
        t
        ((f [@zero_alloc assume]) acc (bit_index + wi))
        ~f
        ~w:I64.(w land lnot (#1L lsl wi))
        ~bit_index)
  ;;

  let[@zero_alloc] rec fold_set_loop_all_words t acc ~f ~word_index ~for_loop_end =
    if word_index > for_loop_end
    then acc
    else (
      let byte_index = byte_index_of_word_index ~word_index in
      let bit_index = bit_index_of_word_index ~word_index in
      let w = Direct.unsafe_get_64 t ~byte_index in
      exclave_
      let acc = fold_set_loop_this_word t acc ~f ~w ~bit_index in
      fold_set_loop_all_words
        t
        acc
        ~f
        ~word_index:(word_index + 1)
        ~for_loop_end [@nontail])
  ;;

  let fold_set_local t ~init ~f =
    let bounds = bounds t in
    let for_loop_end = Masked.for_loop_end bounds in
    exclave_ fold_set_loop_all_words t init ~f ~word_index:0 ~for_loop_end
  ;;

  let rec first_member_loop bytes (bounds : Bounds.t) ~for_loop_end ~word_index = exclave_
    if word_index > for_loop_end
    then None
    else (
      let byte_index = byte_index_of_word_index ~word_index in
      let w = Direct.unsafe_get_64 bytes ~byte_index in
      if I64.(w = #0L)
      then first_member_loop bytes bounds ~for_loop_end ~word_index:(word_index + 1)
      else (
        let bit_index = bit_index_of_word_index ~word_index in
        Some ((I64.ctz w |> I64.to_int_trunc) + bit_index)))
  ;;

  let first_member t = exclave_
    let bounds = bounds t in
    let for_loop_end = Masked.for_loop_end bounds in
    first_member_loop t bounds ~for_loop_end ~word_index:0
  ;;

  let into_bytes t bytes =
    for i = 0 to bit_capacity t - 1 do
      let v = Bool.select (unsafe_mem t i) '1' '0' in
      Bytes.unsafe_set bytes i v
    done
  ;;

  let to_string t =
    let bytes = Bytes.create (bit_capacity t) in
    into_bytes t bytes;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let to_string_local t = exclave_
    let bytes = Bytes.create_local (bit_capacity t) in
    into_bytes t bytes;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let of_string_into t (local_ s) =
    let len = String.length s in
    for index = 0 to len - 1 do
      let open Bool.Non_short_circuiting in
      let c = String.unsafe_get s index in
      let is_one = Char.( = ) c '1' in
      let valid = Char.( = ) c '0' || is_one in
      (* avoid branching in the common case of an entirely 0+1 string *)
      if valid
      then unsafe_assign t index is_one
      else
        raise_s
          [%message
            "bitset string contains a value other than 0 or 1"
              (index : int)
              ~invalid_char:(c : char)
              ~string:([%globalize: string] s : string)]
    done
  ;;

  let of_string s =
    let len = String.length s in
    let t = create ~len in
    of_string_into t s;
    t
  ;;

  let of_string_local s = exclave_
    let len = String.length s in
    let t = create_local ~len in
    of_string_into t s;
    t
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
  let sexp_of_t__stack t = exclave_ Sexp.Atom (to_string_local t)
  let t_of_sexp sexp = of_string ([%of_sexp: string] sexp)
  let capacity t = bit_capacity t

  let quickcheck_generator =
    let module G = Quickcheck.Generator in
    let open Base_quickcheck.Generator.Let_syntax [@mode portable] in
    let%bind len = G.small_non_negative_int in
    let%map vals =
      if len = 0
      then
        (* [return []] doesn't work without [@ cocontended]. *)
        return () >>| fun () -> []
      else (List.quickcheck_generator [@mode portable]) (Int.gen_incl 0 (len - 1))
    in
    let bitset = create ~len in
    List.iter vals ~f:(fun x -> add bitset x);
    bitset
  ;;

  module As_bit_array = struct
    type nonrec t = t

    let sexp_of_t t =
      let t = Array.init (capacity t) ~f:(mem t) in
      [%sexp (t : bool array)]
    ;;

    let t_of_sexp sexp =
      let array = [%of_sexp: bool array] sexp in
      let t = create ~len:(Array.length array) in
      Array.iteri array ~f:(fun i v -> if v then add t i);
      t
    ;;
  end
end

include T

module Expert = struct
  let[@inline] unsafe_to_bytes (t : t) : bytes = t

  let[@inline] unsafe_of_bytes (bytes : bytes) : t =
    invariant_in_test bytes;
    bytes
  ;;
end
