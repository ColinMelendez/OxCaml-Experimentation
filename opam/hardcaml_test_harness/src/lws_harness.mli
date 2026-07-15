(** Test harness for running simulations with [Hardcaml_lws]. *)

open Hardcaml
open Hardcaml_lws

module Make (I : Interface.S) (O : Interface.S) : sig
  module Sim : module type of Cyclesim.With_interface (I) (O)

  type sim_context = Lws_context.M(I)(O).t

  val run
    : (?timeout:int
       -> ?config:Hardcaml_lws.Lws.Config.t
       -> create:(Scope.t -> Signal.t I.t -> Signal.t O.t)
       -> (Lws.Handler.t @ local
           -> inputs:Bits.t ref I.t
           -> outputs:Bits.t ref O.t Before_and_after_edge.t
           -> 'a)
       -> 'a)
        Harness_base.with_test_config

  (** Provides the full [Lws_context.t] to the testbench instead of just input and output
      refs. *)
  val run_advanced
    : (?timeout:int
       -> ?config:Hardcaml_lws.Lws.Config.t
       -> create:(Scope.t -> Signal.t I.t -> Signal.t O.t)
       -> (Lws.Handler.t @ local -> sim_context -> 'a)
       -> 'a)
        Harness_base.with_test_config
end
