(*
 * zed_rope.mli
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

(** Unicode ropes *)

(** Type of unicode ropes. *)
type t [@@deriving sexp]

(** Alias. *)
type rope = t

(** Exception raised when trying to access a character which is outside the bounds of a
    rope. *)
exception Out_of_bounds

(** {6 Construction} *)

(** The empty rope. *)
val empty : t

(** [make length char] creates a rope of length [length] containing only [char]. *)
val make : int -> Zed_char.t -> t

(** [init n f] returns the contenation of [singleton (f 0)], [singleton (f 1)], ...,
    [singleton (f (n - 1))]. *)
val init : int -> (int -> Zed_char.t) -> t

(** [rev_init n f] returns the contenation of [singleton (f (n - 1))], ...,
    [singleton (f 1)], [singleton (f 0)]. *)
val rev_init : int -> (int -> Zed_char.t) -> t

(** [singleton ch] creates a rope of length 1 containing only [ch]. *)
val singleton : Zed_char.t -> t

(** {6 Informations} *)

(** Returns the length of the given rope. *)
val length : t -> int

(** [is_empty rope] returns whether [str] is the empty rope or not. *)
val is_empty : t -> bool

(** Compares two ropes (in code point order). *)
val compare : t -> t -> int

(** [equal r1 r2] retuns [true] iff [r1] is equal to [r2]. *)
val equal : t -> t -> bool

(** {6 To/from strings} *)

(** [of_string str] creates a rope from a string. The string must be UTF-8 encoded and is
    validated. Note that [str] must not be modified after this operation, if you intend to
    do so you must copy it before passing it to [of_string]. *)
val of_string : string -> t

(** [to_string rope] flatten a rope into a string encoded in UTF-8. *)
val to_string : t -> string

(** {6 Random access} *)

(** [get str rope] returns the character at index [idx] in [rope]. *)
val get : t -> int -> Zed_char.t

(** {6 Rope manipulation} *)

(** Concatenates the two given ropes. *)
val append : t -> t -> t

(** [concat sep l] concatenates all strings of [l] separating them by [sep]. *)
val concat : t -> t list -> t

(** [sub rope ofs len] Returns the sub-rope of [rope] starting at [ofs] and of length
    [len]. *)
val sub : t -> int -> int -> t

(** [break rope pos] returns the sub-ropes before and after [pos] in [rope]. It is more
    efficient than creating two sub-ropes with {!sub}. *)
val break : t -> int -> t * t

(** [before rope pos] returns the sub-rope before [pos] in [rope]. *)
val before : t -> int -> t

(** [after rope pos] returns the sub-string after [pos] in [rope]. *)
val after : t -> int -> t

(** [insert rope pos sub] inserts [sub] in [rope] at position [pos]. *)
val insert : t -> int -> t -> t

(** [remove rope pos len] removes the [len] characters at position [pos] in [rope] *)
val remove : t -> int -> int -> t

(** [replace rope pos len repl] replaces the [len] characters at position [pos] in [rope]
    by [repl]. *)
val replace : t -> int -> int -> t -> t

(** [lchop rope] returns [rope] without is first character. Returns {!empty} if [rope] is
    empty. *)
val lchop : t -> t

(** [rchop rope] returns [rope] without is last character. Returns {!empty} if [rope] is
    empty. *)
val rchop : t -> t

(** {6 Iteration, folding and mapping} *)

(** [iter f rope] applies [f] on all characters of [rope] starting from the left. *)
val iter : (Zed_char.t -> unit) -> t -> unit

(** [rev_iter f rope] applies [f] an all characters of [rope] starting from the right. *)
val rev_iter : (Zed_char.t -> unit) -> t -> unit

(** [fold f rope acc] applies [f] on all characters of [rope] starting from the left,
    accumulating a value. *)
val fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [rev_fold f rope acc] applies [f] on all characters of [rope] starting from the right,
    accumulating a value. *)
val rev_fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [map f rope] maps all characters of [rope] with [f]. *)
val map : (Zed_char.t -> Zed_char.t) -> t -> t

(** [rev_map f str] maps all characters of [rope] with [f] in reverse order. *)
val rev_map : (Zed_char.t -> Zed_char.t) -> t -> t

(** {6 Iteration and folding on leafs} *)

(** Note: for all of the following functions, the leaves must absolutely not be modified. *)

(** [iter_leaf f rope] applies [f] on all leaves of [rope] starting from the left. *)
val iter_leaf : (Zed_utf8.t -> unit) -> t -> unit

(** [iter_leaf f rope] applies [f] on all leaves of [rope] starting from the right. *)
val rev_iter_leaf : (Zed_utf8.t -> unit) -> t -> unit

(** [fold f rope acc] applies [f] on all leaves of [rope] starting from the left,
    accumulating a value. *)
val fold_leaf : (Zed_utf8.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [rev_fold f rope acc] applies [f] on all leaves of [rope] starting from the right,
    accumulating a value. *)
val rev_fold_leaf : (Zed_utf8.t -> 'a -> 'a) -> t -> 'a -> 'a

(** {6 Zippers} *)

module Zip : sig
  (** Type of zippers. A zipper allow to naviguate in a rope in a convenient and efficient
      manner. Note that a zipper points to a position between two characters, not to a
      character, so in a rope of length [len] there is [len + 1] positions. *)
  type t

  (** [make_f rope pos] creates a new zipper pointing to positon [pos] of [rope]. *)
  val make_f : rope -> int -> t

  (** [make_f rope pos] creates a new zipper pointing to positon [length rope - pos] of
      [rope]. *)
  val make_b : rope -> int -> t

  (** Returns the position of the zipper in the rope. *)
  val offset : t -> int

  (** [next zipper] returns the code point at the right of the zipper and a zipper to the
      next position. It raises [Out_of_bounds] if the zipper points to the end of the
      rope. *)
  val next : t -> Zed_char.t * t

  (** [prev zipper] returns the code point at the left of the zipper and a zipper to the
      previous position. It raises [Out_of_bounds] if the zipper points to the beginning
      of the rope. *)
  val prev : t -> Zed_char.t * t

  (** [move n zip] moves the zipper by [n] characters. If [n] is negative it is moved to
      the left and if it is positive it is moved to the right. It raises [Out_of_bounds]
      if the result is outside the bounds of the rope. *)
  val move : int -> t -> t

  (** [at_bos zipper] returns [true] iff [zipper] points to the beginning of the rope. *)
  val at_bos : t -> bool

  (** [at_eos zipper] returns [true] iff [zipper] points to the end of the rope. *)
  val at_eos : t -> bool

  (** [find_f f zip] search forward for a character to satisfy [f]. It returns a zipper
      pointing to the left of the first character to satisfy [f], or a zipper pointing to
      the end of the rope if no such character exists. *)
  val find_f : (Zed_char.t -> bool) -> t -> t

  (** [find_b f zip] search backward for a character to satisfy [f]. It returns a zipper
      pointing to the right of the first character to satisfy [f], or a zipper pointing to
      the beginning of the rope if no such character exists. *)
  val find_b : (Zed_char.t -> bool) -> t -> t

  (** [sub zipper len] returns the sub-rope of length [len] pointed by [zipper]. *)
  val sub : t -> int -> rope

  (** [slice zipper1 zipper2] returns the rope between [zipper1] and [zipper2]. If
      [zipper1 > zipper2] then this is the same as [slice zipper2 zipper1].

      The result is unspecified if the two zippers do not points to the same rope. *)
  val slice : t -> t -> rope
end

(** {6 Buffers} *)

module Buffer : sig
  (** This module is similar of the [Buffer] module of the standard library except that it
      works with rope. *)

  (** Type of rope buffers. *)
  type t

  (** Create a new empty buffer. *)
  val create : unit -> t

  (** [add buffer x] add [x] at the end of [buffer]. *)
  val add : t -> Zed_char.t -> unit

  (** [contents buffer] returns the contents of [buffer] as a rope. *)
  val contents : t -> rope

  (** [reset buffer] resets [buffer] to its initial state. *)
  val reset : t -> unit
end
