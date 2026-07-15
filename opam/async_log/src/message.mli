open! Core
open! Import
include module type of Async_log_kernel.Message

type t = Async_log_kernel.Message.t [@@deriving sexp_of]

val to_write_only_text : ?zone:Time_float.Zone.t -> t -> string

module Stable : sig
  module V3 : sig
    type nonrec t = t [@@deriving bin_io, sexp, stable_witness]
  end

  module V2 : sig
    type t = Time_float_unix.t Async_log_kernel.Message.Stable.T1.V2.t
    [@@deriving bin_io, sexp]

    val to_v3 : t -> V3.t
    val of_v3 : V3.t -> t
  end
end
