open! Core
open! Import
include Var_id.Map.Make (Incr)

module Recursive = struct
  type entry =
    { environment : t
    ; resolved_contain : May_contain.Resolved.t
    }

  include Fix_id.Map.Make (struct
      type _ t = entry
    end)

  let resolve_may_contain t may_contain =
    let open May_contain in
    let ({ contains; recursive_dependencies } : Unresolved.t) = may_contain in
    match contains with
    (* If all of the fields are already resolved, we don't need iterate over any
       dependencies. *)
    | { path = Yes_or_maybe | No
      ; lifecycle = Yes_or_maybe | No
      ; input = Yes_or_maybe | No
      } -> May_contain.Unresolved.resolved_equivalent may_contain
    | _ ->
      Dependencies.fold
        recursive_dependencies
        ~init:(May_contain.Unresolved.resolved_equivalent may_contain)
        ~f:(fun acc id ->
          let (T id) = Fix_id.Packed.reveal id in
          let { resolved_contain; environment = _ } = find_exn t id in
          May_contain.Resolved.merge acc resolved_contain)
  ;;
end
