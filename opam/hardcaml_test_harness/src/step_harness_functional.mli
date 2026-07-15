open Hardcaml

module Make (I : Interface.S) (O : Interface.S) : sig
  open Hardcaml_step_testbench
  module Sim : module type of Cyclesim.With_interface (I) (O)
  module Step : Functional.Cyclesim.M(I)(O).S

  val run
    : (?input_default:Bits.t I.t (** Set the default input for the simulation. *)
       -> ?timeout:int
       -> create:(Scope.t -> Signal.t I.t -> Signal.t O.t)
       -> (Step.Handler.t @ local -> 'a)
       -> 'a)
        Harness_base.with_test_config

  (** Provides the full cyclesim to the testbench *)
  val run_advanced
    : (?input_default:Bits.t I.t (** Set the default input for the simulation. *)
       -> ?timeout:int
       -> create:(Scope.t -> Signal.t I.t -> Signal.t O.t)
       -> (Step.Handler.t @ local -> Sim.t -> 'a)
       -> 'a)
        Harness_base.with_test_config
end
