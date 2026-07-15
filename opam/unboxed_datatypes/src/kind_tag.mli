(** Types that correspond to kinds, e.g. for use in GADT tags.

    The standard way to reference a kind tag is via templating, e.g.
    [Kind_tag.t [@kind.explicit float64]], though we also define aliases such as
    [Kind_tag.float64] for readability.

    See [lib/unboxed_datatypes/test/kind_witness_example.ml] for an example. *)

module%template Templated : sig
  type t = [ `value ] [@@kind.explicit value]
  type t = [ `value_or_null ] [@@kind.explicit value_or_null]
  type t = [ `float64 ] [@@kind.explicit float64]
  type t = [ `float32 ] [@@kind.explicit float32]
  type t = [ `bits64 ] [@@kind.explicit bits64]
  type t = [ `bits32 ] [@@kind.explicit bits32]
  type t = [ `bits16 ] [@@kind.explicit bits16]
  type t = [ `bits8 ] [@@kind.explicit bits8]
  type t = [ `immediate ] [@@kind.explicit immediate]
  type t = [ `immediate64 ] [@@kind.explicit immediate64]
  type t = [ `word ] [@@kind.explicit word]
end

type value = (Templated.t[@kind.explicit value])
type value_or_null = (Templated.t[@kind.explicit value_or_null])
type float64 = (Templated.t[@kind.explicit float64])
type float32 = (Templated.t[@kind.explicit float32])
type bits64 = (Templated.t[@kind.explicit bits64])
type bits32 = (Templated.t[@kind.explicit bits32])
type bits16 = (Templated.t[@kind.explicit bits16])
type bits8 = (Templated.t[@kind.explicit bits8])
type immediate = (Templated.t[@kind.explicit immediate])
type immediate64 = (Templated.t[@kind.explicit immediate64])
type word = (Templated.t[@kind.explicit word])
