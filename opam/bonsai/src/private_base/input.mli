open! Core
open! Import

type 'input t

val dynamic : 'input Incr.t -> 'input t
val static : unit t
val static_dummy_for_assoc : ('key, 'w) Comparator.Module.t -> ('key, _, 'w) Map.t t
val static_dummy_for_switch : int Meta.Input.Hidden.t t
val to_incremental : 'input t -> 'input Incr.t
val merge : 'input1 t -> 'input2 t -> ('input1 * 'input2) t
val map : 'a t -> f:('a -> 'b) -> 'b t
val iter_incremental : 'a t -> f:(Incr.Packed.t -> unit) -> unit
