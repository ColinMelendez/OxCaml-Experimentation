open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  id:'a Var_id.t
  -> default:'b
  -> for_some:('a -> 'b)
  -> here:Source_code_position.t
  -> ('b, unit) Computation.packed_info Trampoline.t
