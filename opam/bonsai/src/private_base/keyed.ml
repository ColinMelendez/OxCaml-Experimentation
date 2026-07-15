open! Core

module T = struct
  type t =
    | T :
        { key : 'k
        ; id : 'k Var_id.t
        ; comparator : ('k, _) Comparator.Module.t
        }
        -> t

  let compare
    (T { key = key1; id = id1; comparator = (module Cmp1); _ })
    (T { key = key2; id = id2; _ })
    =
    match Var_id.same_witness id1 id2 with
    | Some T -> Comparator.compare Cmp1.comparator key1 key2
    | None ->
      (* Use the id comparison function so that the comparator is stable. This function
         will never return 0 because we've already established that these ids are not
         equal *)
      Var_id.(Packed.compare (pack id1) (pack id2))
  ;;

  let sexp_of_t (T { key; id = _; comparator = (module Cmp) }) =
    Comparator.sexp_of_t Cmp.comparator key
  ;;

  let create ~key ~id ~comparator = T { key; id; comparator }
end

include T
include Comparable.Make_plain (T)
