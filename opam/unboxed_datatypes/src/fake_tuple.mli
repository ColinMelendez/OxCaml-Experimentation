(* This interface is based on [lib/core/src/tuple_intf.ml] and added to ad-hoc. It is to
   be deleted once non-values are supported in tuples. *)

module%template T2 : sig
  [@@@kind.default ka = value]
  [@@@kind.default kb = (value, float64, immediate64)]

  type ('a : ka, 'b : kb) t =
    { fst : 'a
    ; snd : 'b
    }
  [@@deriving sexp, equal ~localize, compare ~localize, globalize]

  val create : ('a : ka) ('b : kb). 'a @ m -> 'b @ m -> (('a, 'b) t[@kind ka kb]) @ m
  [@@alloc _ @ m = (heap_global, stack_local)]

  [@@@mode.default m = (global, local)]

  val get1 : ('a : ka) ('b : kb). (('a, 'b) t[@kind ka kb]) @ m -> 'a @ m
  val get2 : ('a : ka) ('b : kb). (('a, 'b) t[@kind ka kb]) @ m -> 'b @ m
end

module%template T3 : sig
  [@@@kind.default ka = value]
  [@@@kind.default kb = value]
  [@@@kind.default kc = (value, float64, immediate64)]

  type ('a : ka, 'b : kb, 'c : kc) t =
    { fst : 'a
    ; snd : 'b
    ; trd : 'c
    }
  [@@deriving sexp, equal ~localize, compare ~localize, globalize]

  val create
    : ('a : ka) ('b : kb) ('c : kc).
    'a @ m -> 'b @ m -> 'c @ m -> (('a, 'b, 'c) t[@kind ka kb kc]) @ m
  [@@alloc _ @ m = (heap_global, stack_local)]

  [@@@mode.default m = (global, local)]

  val get1 : ('a : ka) ('b : kb) ('c : kc). (('a, 'b, 'c) t[@kind ka kb kc]) @ m -> 'a @ m
  val get2 : ('a : ka) ('b : kb) ('c : kc). (('a, 'b, 'c) t[@kind ka kb kc]) @ m -> 'b @ m
  val get3 : ('a : ka) ('b : kb) ('c : kc). (('a, 'b, 'c) t[@kind ka kb kc]) @ m -> 'c @ m
end
