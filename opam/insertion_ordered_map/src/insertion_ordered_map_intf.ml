open! Core

(** See {!Insertion_ordered_map} below. *)

module type%template [@mode m = (local, global)] S_plain = sig
  module Key : sig
    type t [@@deriving (compare [@mode.explicit m]), sexp_of]

    include Comparator.S with type t := t
  end

  type ('key, 'a, 'cmp) insertion_ordered_map

  type 'a t = (Key.t, 'a, Key.comparator_witness) insertion_ordered_map
  [@@deriving sexp_of]

  (** [Semantic_equal.equal] will be [true] iff the two [t]s are semantically equivalent,
      but not necessarily structurally equivalent. Here, semantic equivalence refers to
      the property that two [t]s contain equal keys and values in the same relative order.

      Structural equivalence may not hold for two semantically equivalent [t]s if, for
      instance, one [t] has a key added and then removed. This is because that [t] will
      have a different internal structural state to the other [t]. *)
  module Semantic_equal : Equal.S1 with type 'a t = 'a t

  (** [Semantic_compare] provides a compare function with the same semantics as in
      [Semantic_equal]. *)
  module Semantic_compare : sig
    type nonrec 'a t = 'a t [@@deriving compare [@mode.explicit m]]
  end

  val empty : _ t
  val singleton : Key.t -> 'a -> 'a t
  val of_alist_exn : (Key.t * 'a) list -> 'a t
end

module type S = sig
  include S_plain
  include Sexpable.S1 with type 'a t := 'a t
end

module type S_binable = sig
  include S
  include Binable.S1 with type 'a t := 'a t
end

module type Insertion_ordered_map = sig @@ portable
  (** [Insertion_ordered_map] is [Map] with a notion of insertion order.

      An [Insertion_ordered_map.Make(Key).t] can be queried as normal with a [Key.t].
      Insertion, deletion, lookup, etc., all still have O(log N) complexity.

      However, any operation that iterates through keys (e.g. {!val:fold},
      {!val:iter_keys}) will iterate in insertion order with O(N) complexity. *)

  type ('key, 'a, 'cmp) t

  (** {1 Creators} *)

  val empty : ('key, 'cmp) Comparator.Module.t -> ('key, 'a, 'cmp) t
  val singleton : ('key, 'cmp) Comparator.Module.t -> 'key -> 'a -> ('key, 'a, 'cmp) t

  val of_alist_exn
    :  ('key, 'cmp) Comparator.Module.t
    -> ('key * 'a) list
    -> ('key, 'a, 'cmp) t

  (** {1 Order-invariant accessors} *)

  val is_empty : _ t -> bool
  val length : _ t -> int

  val add
    :  ('key, 'a, 'cmp) t
    -> key:'key
    -> data:'a
    -> ('key, 'a, 'cmp) t Map_intf.Or_duplicate.t

  val add_exn : ('key, 'a, 'cmp) t -> key:'key -> data:'a -> ('key, 'a, 'cmp) t

  (** [set] applied to an existing [key] does not change its insertion order. *)
  val set : ('key, 'a, 'cmp) t -> key:'key -> data:'a -> ('key, 'a, 'cmp) t

  (** [update] applied to an existing [key] does not change its insertion order. *)
  val update
    :  ('key, 'a, 'cmp) t
    -> 'key
    -> f:local_ ('a option -> 'a)
    -> ('key, 'a, 'cmp) t

  (** [change] applied to an existing [key] does not change its insertion order. *)
  val change
    :  ('key, 'a, 'cmp) t
    -> 'key
    -> f:local_ ('a option -> 'a option)
    -> ('key, 'a, 'cmp) t

  val find : ('key, 'a, _) t -> 'key -> 'a option
  val find_exn : ('key, 'a, _) t -> 'key -> 'a
  val remove : ('key, 'a, 'cmp) t -> 'key -> ('key, 'a, 'cmp) t
  val mem : ('key, _, _) t -> 'key -> bool
  val exists : (_, 'a, _) t -> f:local_ ('a -> bool) -> bool
  val existsi : ('key, 'a, _) t -> f:local_ (key:'key -> data:'a -> bool) -> bool

  (** Similar to [set] and [update], {!val:map}, {!val:filter} and {!val:filter_map} do
      not change keys' insertion order. *)

  val map : ('key, 'a, 'cmp) t -> f:local_ ('a -> 'b) -> ('key, 'b, 'cmp) t
  val filter : ('key, 'a, 'cmp) t -> f:local_ ('a -> bool) -> ('key, 'a, 'cmp) t

  val filteri
    :  ('key, 'a, 'cmp) t
    -> f:local_ (key:'key -> data:'a -> bool)
    -> ('key, 'a, 'cmp) t

  val filter_map : ('key, 'a, 'cmp) t -> f:local_ ('a -> 'b option) -> ('key, 'b, 'cmp) t

  (** {1 Insertion-ordered accessors} *)

  val iter_keys : ('key, _, _) t -> f:local_ ('key -> unit) -> unit
  val iter : (_, 'a, _) t -> f:local_ ('a -> unit) -> unit
  val iteri : ('key, 'a, _) t -> f:local_ (key:'key -> data:'a -> unit) -> unit

  val iteri_until
    :  ('key, 'a, _) t
    -> f:local_ (key:'key -> data:'a -> Map.Continue_or_stop.t)
    -> Map.Finished_or_unfinished.t

  val fold
    :  ('key, 'a, _) t
    -> init:'b
    -> f:local_ (key:'key -> data:'a -> 'b -> 'b)
    -> 'b

  val fold_until
    :  ('key, 'a, _) t
    -> init:'b
    -> f:local_ (key:'key -> data:'a -> 'b -> ('b, 'final) Container.Continue_or_stop.t)
    -> finish:local_ ('b -> 'final)
    -> 'final

  (** [fold_right] folds over keys and data in the map in reverse insertion order. *)
  val fold_right
    :  ('key, 'a, _) t
    -> init:'b
    -> f:local_ (key:'key -> data:'a -> 'b -> 'b)
    -> 'b

  val keys : ('key, _, _) t -> 'key list
  val data : (_, 'a, _) t -> 'a list
  val to_alist : ('key, 'a, _) t -> ('key * 'a) list

  (** [min_elt] returns the key-value pair corresponding to the oldest key by insertion
      order. *)
  val min_elt : ('key, 'a, _) t -> ('key * 'a) option

  val min_elt_exn : ('key, 'a, _) t -> 'key * 'a

  (** [max_elt] returns the key-value pair corresponding to the newest key by insertion
      order. *)
  val max_elt : ('key, 'a, _) t -> ('key * 'a) option

  val max_elt_exn : ('key, 'a, _) t -> 'key * 'a

  (** [nth] returns the {i nth} inserted key value pair. *)
  val nth : ('key, 'a, _) t -> int -> ('key * 'a) option

  val nth_exn : ('key, 'a, _) t -> int -> 'key * 'a

  (** [rank] returns the insertion-ordered rank of a key. *)
  val rank : ('key, _, _) t -> 'key -> int option

  (** NOTE: [to_map] and [of_map] are not roundtrippable. In particular, if the insertion
      order of [t] is not the same as the order of [Map.t], then insertion-order is lost
      when [of_map] is used to recreate [t].

      [of_map] and [to_map] are roundtrippable if the intermediate [t] is not modified. *)
  val to_map : ('key, 'a, 'cmp) t -> ('key, 'a, 'cmp) Map.t

  val of_map : ('key, 'a, 'cmp) Map.t -> ('key, 'a, 'cmp) t

  module type S_plain =
    S_plain with type ('key, 'a, 'cmp) insertion_ordered_map := ('key, 'a, 'cmp) t

  module type S = S with type ('key, 'a, 'cmp) insertion_ordered_map := ('key, 'a, 'cmp) t

  module type S_binable =
    S_binable with type ('key, 'a, 'cmp) insertion_ordered_map := ('key, 'a, 'cmp) t

  module%template.portable
    [@modality p] [@mode m = (local, global)] Make_plain (Key : sig
      type t [@@deriving (compare [@mode.explicit m]), sexp_of]

      include Comparator.S [@modality p] with type t := t
    end) : S_plain with module Key := Key

  module%template.portable
    [@modality p] Provide_of_sexp (Key : sig
      type t [@@deriving of_sexp]

      include Comparator.S [@modality p] with type t := t
    end) : sig
      type _ t [@@deriving of_sexp]
    end
    with type 'a t := (Key.t, 'a, Key.comparator_witness) t

  module%template.portable
    [@modality p] [@mode m = (local, global)] Make (Key : sig
      type t [@@deriving (compare [@mode.explicit m]), sexp]

      include Comparator.S [@modality p] with type t := t
    end) : S with module Key := Key

  module%template.portable
    [@modality p] Provide_bin_io
      (Key : Stable
    [@modality p]) : sig
      type _ t [@@deriving bin_io]
    end
    with type 'a t := (Key.t, 'a, Key.comparator_witness) t

  module%template.portable
    [@modality p] [@mode m = (local, global)] Make_binable (Key : sig
      type t [@@deriving bin_io, (compare [@mode.explicit m]), sexp]

      include Comparator.S [@modality p] with type t := t
    end) : S_binable with module Key := Key

  module Provide_sexp_grammar (Key : sig
      type t [@@deriving sexp_grammar]
      type comparator_witness
    end) : sig
      type _ t [@@deriving sexp_grammar]
    end
    with type 'a t := (Key.t, 'a, Key.comparator_witness) t
end
