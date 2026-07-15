open! Core
open! Import

(** [computation_to_function t ~key_id ~data_id] attempts to reduce [t] to a plain
    (stateless) function with [key] and [data] inputs along with whether or not the
    function can reference the Bonsai path. Returns [None] if it fails to find such a
    function. The only pattern that this function attempts to optimize is [(return v)],
    where [v] is a [Value.t] built out of only constants, named values, and [Value]'s
    applicative interface. *)
val computation_to_function
  :  'result Computation.t
  -> key_id:'key Var_id.t
  -> key_comparator:('key, _) Comparator.Module.t
  -> data_id:'data Var_id.t
  -> ((Path.t -> 'key -> 'data -> 'result) * May_contain.resolved May_contain.t) option
