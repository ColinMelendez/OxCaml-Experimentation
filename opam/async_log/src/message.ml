module Stable = struct
  module V2 = struct
    type t = Time_float_unix.t Async_log_kernel.Message.Stable.T1.V2.t
    [@@deriving bin_io, sexp]

    let to_v3 = Async_log_kernel.Message.Stable.T1.V2.to_v3
    let of_v3 = Async_log_kernel.Message.Stable.T1.V2.of_v3
  end

  module V3 = struct
    type t = Time_float_unix.Stable.V1.t Async_log_kernel.Message.Stable.T1.V3.t
    [@@deriving bin_io, sexp, stable_witness]
  end
end

open! Core
open! Import

include (
  Async_log_kernel.Message :
    module type of Async_log_kernel.Message
    with module Stable := Async_log_kernel.Message.Stable)

type t = Time_float_unix.t Async_log_kernel.Message.T1.t [@@deriving sexp_of]

let to_write_only_text ?(zone = force Time_float_unix.Zone.local) t =
  to_write_only_text t zone
;;
