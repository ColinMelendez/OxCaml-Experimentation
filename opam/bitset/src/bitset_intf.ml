open! Core

(** [Bitset] provides a space-efficient set of non-negative integer values. *)

module type%template S = sig @@ portable
  type t : mutable_data

  (** [create ~len] creates a bitset with at least the given initial length (in bits). The
      set is initially empty, and does not grow. *)
  val create : len:int -> t

  val create_local : len:int -> t @ local [@@zero_alloc]
  val capacity : t @ local read -> int [@@zero_alloc]

  (** [is_empty t] returns [true] iff [mem t i] returns false on every [i] *)
  val is_empty : t @ local read -> bool
  [@@zero_alloc]

  (** [add t i] adds [i] to the set. *)
  val add : t @ local -> int -> unit [@@zero_alloc]

  val unsafe_add : t @ local -> int -> unit [@@zero_alloc]

  (** [remove t i] removes [i] from set. *)
  val remove : t @ local -> int -> unit
  [@@zero_alloc]

  val unsafe_remove : t @ local -> int -> unit [@@zero_alloc]

  (** [assign t i x = if x then add t i else remove t i], but branch-free. *)
  val assign : t @ local -> int -> bool -> unit
  [@@zero_alloc]

  val unsafe_assign : t @ local -> int -> bool -> unit [@@zero_alloc]

  (** [mem t i] returns [true] iff [i] is in [t]. *)
  val mem : t @ local read -> int -> bool
  [@@zero_alloc]

  val unsafe_mem : t @ local read -> int -> bool [@@zero_alloc]

  (** [clear t] empties the set. *)
  val clear : t @ local -> unit [@@zero_alloc]

  (** [set_all t] adds everything to the set. *)
  val set_all : t @ local -> unit
  [@@zero_alloc]

  (** [union a b] combines two bitsets; it creates a new bitset where [i] is present iff
      [i] is in [a] or [b] *)
  val union : t @ local read -> t @ local read -> t

  val union_local : t @ local read -> t @ local read -> t @ local [@@zero_alloc]

  (** [union_into ~dst ~src] adds all elements in [src] into [dst]. All elements set in
      [src] must be below [capacity dst]. This function raises if that is not the case. *)
  val union_into : dst:t @ local -> src:t @ local read -> unit
  [@@zero_alloc]

  (** [inter a b] intersects two bitsets; it creates a new bitset where [i] is present iff
      [i] is in [a] and [b] *)
  val inter : t @ local read -> t @ local read -> t

  val inter_local : t @ local read -> t @ local read -> t @ local [@@zero_alloc]
  val inter_into : dst:t @ local -> src:t @ local read -> unit [@@zero_alloc]

  (** [is_inter_empty a b = is_empty (inter_local a b)], but skips the intermediate
      allocation.

      This function is optimized for small bitsets and minimizes branching. For large
      bitsets, note that the entire intersection is computed (no early exit for non-zero
      intersection). *)
  val is_inter_empty : t @ local read -> t @ local read -> bool
  [@@zero_alloc]

  (** [diff a b] finds the elements in one but not the other bitset; it creates a new
      bitset where [i] is present iff [i] is in [a] and not [b] *)
  val diff : t @ local read -> t @ local read -> t

  val diff_local : t @ local read -> t @ local read -> t @ local [@@zero_alloc]

  (** [remove_all ~dst ~src] removes all elements set in [src] from [dst]. It is the
      in-place version of [diff] *)
  val remove_all : dst:t @ local -> src:t @ local read -> unit
  [@@zero_alloc]

  (** [complement t] creates a new bitset where [i] is present iff [i] is not in [t]. This
      operates only on [i]s from 0 to [capacity t] (which may be larger than the initial
      [len]) *)
  val complement : t @ local read -> t

  val complement_local : t @ local read -> t @ local [@@zero_alloc]
  val complement_inplace : t @ local -> unit [@@zero_alloc]

  (** [is_subset t1 ~of_:t2] returns true iff [t1] is a subset of [t2]. *)
  val is_subset : t @ local read -> of_:t @ local read -> bool
  [@@zero_alloc]

  (** returns the number of members -- values of [i] for which [mem t i] is true *)
  val num_members : t @ local read -> int
  [@@zero_alloc]

  (** {v
 returns the number of members -- values of [i] for which [mem t i] is true --
      within the range defined by start and end

      Raises if start is outside of [0, capacity t) or end is outside of [0, capacity t].
      v} *)
  val num_members_in_range
    :  t @ local read
    -> start:int Maybe_bound.t @ local
    -> end_:int Maybe_bound.t @ local
    -> int
  [@@zero_alloc]

  (** returns the index of the first element set in [t] *)
  val first_member : t @ local read -> int option @ local
  [@@zero_alloc]

  (** [iter t ~f] calls [f] for all elements in set [t] *)
  val iter_set : t @ local read -> f:(int -> unit) @ local -> unit
  [@@zero_alloc]

  (** [fold_set_local t ~init ~f] returns [f] folded over all the elements in set [t] *)
  val fold_set_local
    :  t @ local read
    -> init:'acc @ local
    -> f:('acc @ local -> int -> 'acc @ local) @ local
    -> 'acc @ local
  [@@zero_alloc]

  (** [grow t ~new_len] creates a new set from [t] with capacity [new_len]. *)
  val grow : t @ local read -> new_len:int -> t

  val grow_local : t @ local read -> new_len:int -> t @ local [@@zero_alloc]

  (** [copy_and_truncate t ~new_len] creates a new set from [t] with capacity [new_len].
      Bits above [new_len] may be cleared. *)
  val copy_and_truncate : t @ local read -> new_len:int -> t

  val copy_and_truncate_local : t @ local read -> new_len:int -> t @ local [@@zero_alloc]
  val copy : t @ local read -> t
  val copy_local : t @ local read -> t @ local [@@zero_alloc]

  (** Converts [t] into a string with length [capacity t], where each char is either '0'
      for an unset bit or '1' for a set bit. *)
  val to_string : t @ local read -> string

  (** Like [to_string], but returns a local [string]. *)
  val to_string_local : t @ local read -> string @ local
  [@@zero_alloc]

  (** Opposite of [to_string]. *)
  val of_string : string @ local -> t

  (** Like [of_string], but returns a local [t]. *)
  val of_string_local : string @ local -> t @ local
  [@@zero_alloc]

  val sexp_of_t : t @ local read -> Sexp.t @ m
  [@@alloc a @ m = (heap @ global, stack @ local)] [@@zero_alloc_if_stack a]

  val t_of_sexp : Sexp.t -> t
  val quickcheck_generator : t Quickcheck.Generator.t
end

module type S_plain = sig
  type t : mutable_data [@@deriving bin_io, globalize, compare ~localize, equal ~localize]

  [%%template:
  [@@@mode.default l = (local, global)]

  val equal : t @ l read -> t @ l read -> bool
  val compare : t @ l read -> t @ l read -> int]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, stable_witness]
    end
  end

  include S with type t := t

  module As_bit_array : sig
    type nonrec t = t [@@deriving sexp]
  end
end

module type Bitset = sig @@ portable
  (** Bitset implementation backed by a standard OCaml string. *)

  module type S_plain = S_plain

  include S_plain

  module Expert : sig
    val unsafe_to_bytes : t @ local -> Bytes.t @ local
    val unsafe_of_bytes : Bytes.t @ local -> t @ local
  end
end
