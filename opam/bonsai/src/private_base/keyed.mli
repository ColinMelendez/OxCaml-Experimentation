open! Core

type t =
  | T :
      { key : 'k
      ; id : 'k Var_id.t
      ; comparator : ('k, _) Comparator.Module.t
      }
      -> t
[@@deriving sexp_of]

val create : key:'k -> id:'k Var_id.t -> comparator:('k, _) Comparator.Module.t -> t

include Comparable.S_plain with type t := t
