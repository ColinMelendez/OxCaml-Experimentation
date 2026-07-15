module Templated = struct
  type%template (_ : any) t = T : ('a : k). ('a t[@kind.explicit k])
  [@@kind.explicit k = (base_or_null_with_imm, value, bits8, bits16)]
end

type ('a : any) value = ('a Templated.t[@kind.explicit value])
type ('a : any) value_or_null = ('a Templated.t[@kind.explicit value_or_null])
type ('a : any) float32 = ('a Templated.t[@kind.explicit float32])
type ('a : any) float64 = ('a Templated.t[@kind.explicit float64])
type ('a : any) bits8 = ('a Templated.t[@kind.explicit bits8])
type ('a : any) bits16 = ('a Templated.t[@kind.explicit bits16])
type ('a : any) bits32 = ('a Templated.t[@kind.explicit bits32])
type ('a : any) bits64 = ('a Templated.t[@kind.explicit bits64])
type ('a : any) immediate = ('a Templated.t[@kind.explicit immediate])
type ('a : any) immediate64 = ('a Templated.t[@kind.explicit immediate64])
type ('a : any) word = ('a Templated.t[@kind.explicit word])
