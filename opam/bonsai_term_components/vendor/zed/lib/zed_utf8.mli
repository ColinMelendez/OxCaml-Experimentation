(*
 * zed_utf8.mli
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

open Core

(** UTF-8 enoded strings *)

(** Type of UTF-8 encoded strings. *)
type t = string [@@deriving sexp]

(** [Invalid(error, text)] Exception raised when an invalid UTF-8 encoded string is
    encountered. [text] is the faulty text and [error] is a description of the first error
    in [text]. *)
exception Invalid of string * string

(** Exception raised when trying to access a character which is outside the bounds of a
    string. *)
exception Out_of_bounds

(** {6 Validation} *)

(** [check str] checks that [str] is a valid UTF-8 encoded string. Returns its length if
    it is or an error message if not. *)
val check : t -> (int, string) Result.t

(** Same as check but raises an exception in case the argument is not a valid text,
    otherwise returns the length of the string. *)
val validate : t -> int

(** [next_error str ofs] returns [(ofs', count, msg)] where [ofs'] is the offset of the
    start of the first invalid sequence after [ofs] (inclusive) in [str], [count] is the
    number of unicode character between [ofs] and [ofs'] (exclusive) and [msg] is an error
    message. If there is no error until the end of string then [ofs] is
    [String.length str] and [msg] is the empty string. *)
val next_error : t -> int -> int * int * string

(** {6 Construction} *)

(** [singleton ch] creates a string of length 1 containing only the given character. *)
val singleton : Zed_char.t -> t

(** [make n ch] creates a string of length [n] filled with [ch]. *)
val make : int -> Zed_char.t -> t

(** [init n f] returns the contenation of [singleton (f 0)], [singleton (f 1)], ...,
    [singleton (f (n - 1))]. *)
val init : int -> (int -> Zed_char.t) -> t

(** [rev_init n f] returns the contenation of [singleton (f (n - 1))], ...,
    [singleton (f 1)], [singleton (f 0)]. *)
val rev_init : int -> (int -> Zed_char.t) -> t

(** {6 Informations} *)

(** Returns the length of the given string. *)
val length : t -> int

(** {6 Comparison} *)

(** Compares two strings (in code point order). *)
val compare : t -> t -> int

(** {6 Random access} *)

(** [get str idx] returns the character at index [idx] in [str]. *)
val get : t -> int -> Zed_char.t

(** {6 String manipulation} *)

(** [sub str ofs len] Returns the sub-string of [str] starting at [ofs] and of length
    [len]. *)
val sub : t -> int -> int -> t

(** [break str pos] returns the sub-strings before and after [pos] in [str]. It is more
    efficient than creating two sub-strings with {!sub}. *)
val break : t -> int -> t * t

(** [before str pos] returns the sub-string before [pos] in [str] *)
val before : t -> int -> t

(** [after str pos] returns the sub-string after [pos] in [str] *)
val after : t -> int -> t

(** [insert str pos sub] inserts [sub] in [str] at position [pos]. *)
val insert : t -> int -> t -> t

(** [remove str pos len] removes the [len] characters at position [pos] in [str] *)
val remove : t -> int -> int -> t

(** [replace str pos len repl] replaces the [len] characters at position [pos] in [str] by
    [repl]. *)
val replace : t -> int -> int -> t -> t

(** {6 Tranformation} *)

(** [rev str] reverses all characters of [str]. *)
val rev : t -> t

(** [concat sep l] returns the concatenation of all strings of [l] separated by [sep]. *)
val concat : t -> t list -> t

(** [concat sep l] returns the concatenation of all strings of [l] in reverse order
    separated by [sep]. *)
val rev_concat : t -> t list -> t

(** [explode str] returns the list of all characters of [str]. *)
val explode : t -> Zed_char.t list

(** [rev_explode str] returns the list of all characters of [str] in reverse order. *)
val rev_explode : t -> Zed_char.t list

(** [implode l] returns the concatenation of all characters of [l]. *)
val implode : Zed_char.t list -> t

(** [rev_implode l] is the same as [implode (List.rev l)] but more efficient. *)
val rev_implode : Zed_char.t list -> t

(** {6 Text traversals} *)

(** [iter f str] applies [f] an all characters of [str] starting from the left. *)
val iter : (Zed_char.t -> unit) -> t -> unit

(** [rev_iter f str] applies [f] an all characters of [str] starting from the right. *)
val rev_iter : (Zed_char.t -> unit) -> t -> unit

(** [fold f str acc] applies [f] on all characters of [str] starting from the left,
    accumulating a value. *)
val fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [rev_fold f str acc] applies [f] on all characters of [str] starting from the right,
    accumulating a value. *)
val rev_fold : (Zed_char.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [map f str] maps all characters of [str] with [f]. *)
val map : (Zed_char.t -> Zed_char.t) -> t -> t

(** [rev_map f str] maps all characters of [str] with [f] in reverse order. *)
val rev_map : (Zed_char.t -> Zed_char.t) -> t -> t

(** [map f str] maps all characters of [str] with [f] and concatenate the result. *)
val map_concat : (Zed_char.t -> t) -> t -> t

(** [rev_map f str] maps all characters of [str] with [f] in reverse order and concatenate
    the result. *)
val rev_map_concat : (Zed_char.t -> t) -> t -> t

(** [filter f str] filters characters of [str] with [f]. *)
val filter : (Zed_char.t -> bool) -> t -> t

(** [rev_filter f str] filters characters of [str] with [f] in reverse order. *)
val rev_filter : (Zed_char.t -> bool) -> t -> t

(** [filter_map f str] filters and maps characters of [str] with [f]. *)
val filter_map : (Zed_char.t -> Zed_char.t option) -> t -> t

(** [rev_filter_map f str] filters and maps characters of [str] with [f] in reverse order. *)
val rev_filter_map : (Zed_char.t -> Zed_char.t option) -> t -> t

(** [filter_map f str] filters and maps characters of [str] with [f] and concatenate the
    result. *)
val filter_map_concat : (Zed_char.t -> t option) -> t -> t

(** [rev_filter_map f str] filters and maps characters of [str] with [f] in reverse order
    and concatenate the result. *)
val rev_filter_map_concat : (Zed_char.t -> t option) -> t -> t

(** {6 Scanning} *)

(** [for_all f text] returns whether all characters of [text] verify the predicate [f]. *)
val for_all : (Zed_char.t -> bool) -> t -> bool

(** [exists f text] returns whether at least one character of [text] verify [f]. *)
val exists : (Zed_char.t -> bool) -> t -> bool

(** [count f text] returhs the number of characters of [text] verifying [f]. *)
val count : (Zed_char.t -> bool) -> t -> int

(** {6 Tests} *)

(** [contains text sub] returns whether [sub] appears in [text] *)
val contains : t -> t -> bool

(** [starts_with text prefix] returns [true] iff [s] starts with [prefix]. *)
val starts_with : t -> t -> bool

(** [ends_with text suffix] returns [true] iff [s] ends with [suffix]. *)
val ends_with : t -> t -> bool

(** {6 Stripping} *)

(** [strip ?predicate text] returns [text] without its firsts and lasts characters that
    match [predicate]. [predicate] default to testing whether the given character has the
    [`White_Space] unicode property. For example:

    {[
      strip "\n  foo\n  " = "foo"
    ]} *)
val strip : ?predicate:(Zed_char.t -> bool) -> t -> t

(** [lstrip ?predicate text] is the same as {!strip} but it only removes characters at the
    left of [text]. *)
val lstrip : ?predicate:(Zed_char.t -> bool) -> t -> t

(** [lstrip ?predicate text] is the same as {!strip} but it only removes characters at the
    right of [text]. *)
val rstrip : ?predicate:(Zed_char.t -> bool) -> t -> t

(** [lchop t] returns [t] without is first character. Returns [""] if [t = ""] *)
val lchop : t -> t

(** [rchop t] returns [t] without is last character. Returns [""] if [t = ""]. *)
val rchop : t -> t

(** {6 Buffers} *)

(** [add buf ch] is the same as [Buffer.add_string buf (singleton ch)] but is more
    efficient. *)
val add : Buffer.t -> Zed_char.t -> unit

(** Return the position just after the written character *)
val encode_to_bigstring : Bigstring.t -> Zed_char.t -> pos:int -> int

(** {6 Escaping} *)

(** [escaped_char ch] returns a string containg [ch] or an escaped version of [ch] if:
    - [ch] is a control character (code < 32)
    - [ch] is the character with code 127
    - [ch] is a non-ascii, non-alphabetic character

    It uses the syntax [\xXX], [\uXXXX], [\UXXXXXX] or a specific escape sequence
    [\n, \r, ...]. *)
val escaped_char : Zed_char.t -> t

(** [add_escaped_char buf ch] is the same as [Buffer.add_string buf (escaped_char ch)] but
    a bit more efficient. *)
val add_escaped_char : Buffer.t -> Zed_char.t -> unit

(** [escaped text] escape all characters of [text] as with [escape_char]. *)
val escaped : t -> t

(** [add_escaped_char buf text] is the same as [Buffer.add_string buf (escaped text)] but
    a bit more efficient. *)
val add_escaped : Buffer.t -> t -> unit

(** [escaped_string enc str] is the same as [escaped] except that [str] may contain
    invalid UTF-8 data. In This case invalid sequence are escaped using the syntax
    [\\yAB]. *)
val escaped_string : string -> t

(** [add_escaped_char buf enc text] is the same as
    [Buffer.add_string buf (escaped_string enc text)] but a bit more efficient. *)
val add_escaped_string : Buffer.t -> string -> unit

(** {6 Safe offset API} *)

(** [next str ofs] returns the offset of the next character in [str]. *)
val next : t -> int -> int

(** [prev str ofs] returns the offset of the previous character in [str]. *)
val prev : t -> int -> int

(** [extract str ofs] returns the code-point at offset [ofs] in [str]. *)
val extract : t -> int -> Zed_char.t

(** [extract_next str ofs] returns the code-point at offset [ofs] in [str] and the offset
    the next character. *)
val extract_next : t -> int -> Zed_char.t * int

(** [extract_prev str ofs] returns the code-point at the previous offset in [str] and this
    offset. *)
val extract_prev : t -> int -> Zed_char.t * int

(** {6 Unsafe offset API} *)

(** These functions does not check that the given offset is inside the bounds of the given
    string. *)

(** [unsafe_next str ofs] returns the offset of the next character in [str]. *)
val unsafe_next : t -> int -> int

(** [unsafe_prev str ofs] returns the offset of the previous character in [str]. *)
val unsafe_prev : t -> int -> int

(** [unsafe_extract str ofs] returns the code-point at offset [ofs] in [str]. *)
val unsafe_extract : t -> int -> Zed_char.t

(** [unsafe_extract_next str ofs] returns the code-point at offset [ofs] in [str] and the
    offset the next character. *)
val unsafe_extract_next : t -> int -> Zed_char.t * int

(** [unsafe_extract_prev str ofs] returns the code-point at the previous offset in [str]
    and this offset. *)
val unsafe_extract_prev : t -> int -> Zed_char.t * int
