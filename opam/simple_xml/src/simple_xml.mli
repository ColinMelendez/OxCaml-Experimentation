open Core

(** The types here represent an XML document, including namespaces. Note that namespaces
    here are not namespace prefixes, but the actual namespace, as in, for example:
    {v
      <foo xmlns:prefix="https://foo/">
        <prefix:bar>test</prefix:bar>
      <foo>
    v}

    Would be represented as:
    {v
      { tag = { ns = ""; tag = "foo" }
      ; attributes =
        [ { ns = Xmlm.ns_xmlns
          ; key = "prefix"
          ; value = "https://foo/"
          }
        ]
      ; children =
        [ Element
          { tag =
            { ns = "https://foo/" (* We don't say prefix, we say https://foo/ *)
            ; tag = "bar"
            }
          ; attributes = []
          ; children = [ Text "test" ]
          }
        ]
      }
    v}

    If you wish to set the default namespace in an element, attach the following attribute
    to it: [{ ns = Xmlm.ns_xmlns; key = "xmlns"; value = your_namespace }].

    For more info about namespaces, read {:https://www.w3.org/TR/xml-names/}. *)

module Tag : sig
  type t =
    { ns : string
    ; tag : string
    }
  [@@deriving sexp_of, compare ~localize, equal ~localize, hash, quickcheck]

  val of_tuple : string * string -> t
end

module Attribute : sig
  type t =
    { ns : string
    ; key : string
    ; value : string
    }
  [@@deriving sexp_of, compare ~localize, equal ~localize, hash, quickcheck]

  val of_tuple : (string * string) * string -> t
end

type element =
  { tag : Tag.t
  ; attributes : Attribute.t list
  ; children : t list
  }

and t =
  | Element of element
  | Text of string
[@@deriving sexp_of, compare ~localize, equal ~localize, hash, quickcheck]

val parse_input : Xmlm.input -> element

type inp =
  [ `Channel of In_channel.t
  | `File of string
  | `String of string
  ]

val parse : ?strip:bool -> inp -> element

(** Transforms an XML object to an XML string. This will fail if an object is referring to
    an unknown namespace (for example an element has a namespace that is not the default
    namespace, or a previously bound namespace).

    @param decl determine if the standard xml version string should be included
    @param ns_prefix
      is called when a namespace is used when it is unbound. It allows the user to provide
      a namespace prefix instead of raising. Useful when displaying a snippet of XML where
      the namespace prefix was declared outside of the snippet.
    @param buf
      provides the buffer to use for stringification. Allocates a new buffer if not
      provided an existing buffer.
    @param fmt
      how to format the output. [`Minified] means no new lines or indents (default),
      [`Newlines_only] means new lines for every tag, and [`Indent of n] include new
      lines, and also indents each nested level w/ [n] spaces. *)
val to_string
  :  ?decl:bool
  -> ?ns_prefix:(string -> string option)
  -> ?buf:Buffer.t
  -> ?fmt:[ `Minified | `Newlines_only | `Indent of int ]
  -> t
  -> string

(** Shorthand for [parse (`String s)] *)
val element_of_string : string -> element

val map_element : t -> f:(element -> element) -> t

(** Returns false if there is an element with a non-default namespace that is not bound to
    a prefix, or an attribute with a namespace that is not bound to a prefix.

    [to_string t] will always raise if [all_namespaces_representable t] returns false. *)
val all_namespaces_representable
  :  ?default_namespace:string
  -> ?bound_namespaces:string String.Map.t (** Map from prefix to namespace *)
  -> t
  -> bool

(** Helpers to create XML objects. *)
module Creators : sig
  val element' : ?ns:string -> ?attributes:Attribute.t list -> string -> t list -> element
  val element : ?ns:string -> ?attributes:Attribute.t list -> string -> t list -> t
  val text : string -> t

  (** [leaf_element ?ns ?attributes tag leaf_text] *)
  val leaf_element : ?ns:string -> ?attributes:Attribute.t list -> string -> string -> t

  val attr : ?ns:string -> string -> string -> Attribute.t

  (** Declare the default namespace of a element. *)
  val default_namespace : string -> Attribute.t

  (** Declare a namespace prefix. *)
  val namespace_prefix : prefix:string -> ns:string -> Attribute.t
end

module Raise_on_namespace = Raise_on_namespace

module Stable : sig
  module V2 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare ~localize, hash, stable_witness]

    type nonrec element = element
    [@@deriving sexp, bin_io, compare ~localize, hash, stable_witness]
  end

  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare ~localize, hash, stable_witness]

    type nonrec element = element
    [@@deriving sexp, bin_io, compare ~localize, hash, stable_witness]
  end
end
