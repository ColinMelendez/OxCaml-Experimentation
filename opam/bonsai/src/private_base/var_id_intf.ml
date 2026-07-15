open! Core

(** [Var_id] is a faster ['a Type_equal.Id.t] meant to store mappings of

    ['a Var_id.t -> 'a Data.t]

    in a way such that each ['a key * 'a data] have the same ['a].

    We are using our own version of this univ map mechanism for speed / memory
    optimization purposes. *)

module type S = sig
  (** An identifier for something parameterized by ['a]. More efficient than
      [Type_equal.Id]. *)
  type 'a t

  val create : unit -> _ t
  val same : 'a t -> 'b t -> bool
  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t

  module Packed : sig
    type t_gadt = T : 'a t -> t_gadt
    type t [@@deriving compare, equal, hash, sexp]

    include Comparable.S with type t := t
    module Table : Hashtbl.S_plain with type key = t

    val reveal : t -> t_gadt
  end

  val pack : 'a t -> Packed.t

  module Set : sig
    type 'a id := 'a t

    (** A type for containing a set of [Id.t]. Typically used to track free variables
        inside a computation *)
    type t

    (** the empty set *)
    val empty : t

    (** a singleton set *)
    val singleton : _ id -> t

    (** returns [true] if the set contains no elements *)
    val is_empty : t -> bool

    val length : t -> int

    (** Adds a type-id to the set. Nothing happens if the set already contains the item. *)
    val add : t -> _ id -> t

    (** Removes a type-id from the set. Nothing happens if the set doesn't already contain
        it *)
    val remove : t -> _ id -> t

    (** computes the union of two sets *)
    val union : t -> t -> t

    (** Folds over the elements in the set *)
    val fold : t -> init:'acc -> f:('acc -> Packed.t -> 'acc) -> 'acc

    (** maps over the set and sticks the return values in a list *)
    val map_to_list : t -> f:(Packed.t -> 'a) -> 'a list

    (** iterates over the elements in the set *)
    val iter : t -> f:(Packed.t -> unit) -> unit
  end
end

module type S_map = sig
  type t

  module Id : S
  module Data : T1

  val empty : t
  val singleton : 'a Id.t -> 'a Data.t -> t
  val add_exn : t -> key:'a Id.t -> data:'a Data.t -> t
  val add_overwriting : t -> key:'a Id.t -> data:'a Data.t -> t
  val find : t -> 'a Id.t -> 'a Data.t option
  val find_exn : t -> 'a Id.t -> 'a Data.t
  val remove : t -> 'a Id.t -> t

  type combine = { combine : 'a. key:'a Id.t -> 'a Data.t -> 'a Data.t -> 'a Data.t }
  type 'acc folder = { f : 'a. 'a Id.t -> 'a Data.t -> 'acc -> 'acc }
  type 'b mapper = { f : 'a. 'a Id.t -> 'a Data.t -> 'b }

  val merge : t -> t -> combine -> t
  val fold : t -> init:'acc -> 'acc folder -> 'acc
  val map_to_list : t -> 'b mapper -> 'b list
end

module type Var_id = sig
  module type S = S
  module type S_map = S_map

  include S

  module Map : sig
    module Make (Data : T1) : S_map with type 'a Id.t = 'a t and module Data = Data
  end
end
