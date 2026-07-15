open! Core
include Var_id_intf.S

val to_var_id : 'a t -> 'a Var_id.t

module Map : sig
  module Make (Data : T1) :
    Var_id_intf.S_map with type 'a Id.t = 'a t and module Data = Data
end
