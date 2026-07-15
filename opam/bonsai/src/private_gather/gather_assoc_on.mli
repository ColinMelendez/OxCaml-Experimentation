open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  gather:'r Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> map:('io, 'v, 'cmp_io) Map_intf.Map.t Value.t
  -> io_comparator:
       (module Comparator.S with type comparator_witness = 'cmp_io and type t = 'io)
  -> model_comparator:
       (module Comparator.S with type comparator_witness = 'cmp_model and type t = 'model)
  -> io_key_id:'io Var_id.t
  -> io_cmp_id:'cmp_io Var_id.t
  -> model_key_id:'model Var_id.t
  -> model_cmp_id:'cmp_model Var_id.t
  -> data_id:'v Var_id.t
  -> by:'r Computation.t
  -> get_model_key:('io -> 'v -> 'model)
  -> here:Source_code_position.t
  -> (('io, 'r, 'cmp_io) Map_intf.Map.t, unit) Computation.packed_info Trampoline.t
