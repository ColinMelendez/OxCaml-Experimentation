@@ portable

open! Import

(** Dynamically checked references that can only be accessed once. *)
type 'a t : mutable_data with 'a

(** [make a] creates a reference to a [once unique] value [a]. *)
val make : 'a @ once unique -> 'a t @ unique

(** [take_or_null t] returns the value inside [t], or [Null] if it was already accessed. *)
val take_or_null : 'a t @ local -> 'a or_null @ once unique

(** [take_exn t] returns the value inside [t], or raises [Failure] if it was already
    accessed. *)
val take_exn : 'a t @ local -> 'a @ once unique

(** [is_taken t] is whether or not [t] has already been taken. *)
val is_taken : 'a t @ local -> bool

(** [borrow_or_null t ~f] temporarily borrows the value inside [t], passing it to [f],
    then restores it. Returns [Null] if [t] was already accessed. If [f] raises, the value
    is restored before re-raising. *)
val borrow_or_null
  : ('a : value mod many) 'r.
  'a t @ local
  -> f:('a @ local -> 'r @ once unique) @ local once
  -> 'r or_null @ once unique

(** [borrow_exn t ~f] temporarily borrows the value inside [t], passing it to [f], then
    restores it. Raises [Failure] if [t] was already accessed. If [f] raises, the value is
    restored before re-raising. *)
val borrow_exn
  : ('a : value mod many) ('r : value_or_null).
  'a t @ local -> f:('a @ local -> 'r @ once unique) @ local once -> 'r @ once unique

module Atomic : sig
  type (!'a : value) t : immutable_data with 'a @@ contended portable

  (** [make a] creates a reference to a [once unique] value [a]. *)
  val make : 'a @ contended once portable unique -> 'a t @ unique

  (** [take_or_null t] returns the value inside [t], or [Null] if it was already accessed. *)
  val take_or_null
    : ('a : value).
    'a t @ local -> 'a or_null @ contended once portable unique

  (** [take_exn t] returns the value inside [t], or raises [Failure] if it was already
      accessed. *)
  val take_exn : ('a : value). 'a t @ local -> 'a @ contended once portable unique

  (** [is_taken t] is whether or not [t] has already been taken. *)
  val is_taken : 'a t @ local -> bool

  (** [borrow_or_null t ~f] temporarily borrows the value inside [t], passing it to [f],
      then restores it. Returns [Null] if [t] was already accessed. If [f] raises, the
      value is restored before re-raising.

      If multiple threads simultaneously call [borrow_or_null], only one will successfully
      borrow the value, and the rest will return [Null]. *)
  val borrow_or_null
    : ('a : value mod many) 'r.
    'a t @ local
    -> f:('a @ contended local portable -> 'r @ once unique) @ local once
    -> 'r or_null @ once unique

  (** [borrow_exn t ~f] temporarily borrows the value inside [t], passing it to [f], then
      restores it. Raises [Failure] if [t] was already accessed. If [f] raises, the value
      is restored before re-raising.

      If multiple threads simultaneously call [borrow_or_null], only one will successfully
      borrow the value, and the rest will raise. *)
  val borrow_exn
    : ('a : value mod many) ('r : value_or_null).
    'a t @ local
    -> f:('a @ contended local portable -> 'r @ once unique) @ local once
    -> 'r @ once unique
end

module Local : sig
  (** Dynamically checked reference to a local value that can only be accessed once. *)
  type 'a t : mutable_data with 'a

  (** [make a] creates a reference to a [local once unique] value [a]. *)
  val make : 'a @ local once unique -> 'a t @ local unique

  (** [take_or_null t] returns the value inside [t], or [Null] if it was already accessed. *)
  val take_or_null : 'a t @ local -> 'a or_null @ local once unique

  (** [take_exn t] returns the value inside [t], or raises [Failure] if it was already
      accessed. *)
  val take_exn : 'a t @ local -> 'a @ local once unique
end
