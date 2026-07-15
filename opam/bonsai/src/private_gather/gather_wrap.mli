open! Core
open! Bonsai_private_base.Import
open! Bonsai_private_base

val f
  :  gather:'result Computation.gather_fun
  -> recursive_scopes:Computation.Recursive_scopes.t
  -> time_source:Time_source.t
  -> wrapper_model:'model Meta.Model.t
  -> action_id:'action Var_id.t
  -> result_id:'result Meta.Input.t
  -> inject_id:('action -> unit Effect.t) Var_id.t
  -> model_id:'model Var_id.t
  -> inner:'result Computation.t
  -> dynamic_apply_action:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> time_source:Time_source.t
        -> 'result Computation_status.t
        -> 'model
        -> 'action
        -> 'model)
  -> reset:
       (inject:('action -> unit Effect.t)
        -> schedule_event:(unit Effect.t -> unit)
        -> time_source:Time_source.t
        -> 'model
        -> 'model)
  -> here:Source_code_position.t
  -> ('result, unit) Computation.packed_info Trampoline.t
