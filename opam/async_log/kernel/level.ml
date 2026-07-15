module Stable = struct
  open! Core.Core_stable

  module V2 = struct
    type t =
      [ `Debug
      | `Info
      | `Warn
      | `Error
      ]
    [@@deriving bin_io, sexp, stable_witness]

    let severity = function
      | `Debug -> 1
      | `Info -> 2
      | `Warn -> 3
      | `Error -> 4
    ;;

    let compare_by_severity a b = [%compare: int] (severity a) (severity b)

    [%%template
      let compare (a @ m) (b @ m) = compare_by_severity a b [@@mode m = (global, local)]]

    let%expect_test "bin_digest" =
      print_endline [%bin_digest: t];
      [%expect {| c90d477f028ce53eb75d463f68709eaf |}]
    ;;
  end

  module V1 = struct
    type t =
      [ `Debug
      | `Info
      | `Error
      ]
    [@@deriving bin_io, compare ~localize, sexp, stable_witness]

    let%expect_test "bin_digest Level.V1" =
      print_endline [%bin_digest: t];
      [%expect {| 62fa833cdabec8a41d614848cd11f858 |}]
    ;;

    let of_v2 = function
      | #t as v1 -> v1
      | `Warn -> `Error
    ;;

    let to_v2 (t : t) = (t :> V2.t)
  end
end

module T = struct
  type t =
    [ `Debug
    | `Info
    | `Warn
    | `Error
    ]
  [@@deriving bin_io, enumerate, equal ~localize, globalize, sexp, sexp_grammar]

  [%%template
    let compare (a @ m) (b @ m) = Stable.V2.compare_by_severity a b
    [@@mode m = (global, local)]
    ;;]

  let to_string = function
    | `Debug -> "Debug"
    | `Info -> "Info"
    | `Warn -> "Warn"
    | `Error -> "Error"
  ;;

  let of_string = function
    | "Debug" -> `Debug
    | "Info" -> `Info
    | "Warn" -> `Warn
    | "Error" -> `Error
    | s -> Core.failwithf "not a valid level %s" s ()
  ;;
end

open! Core
open! Import
include T

let arg =
  Command.Arg_type.enumerated
    ~list_values_in_help:true
    ~case_sensitive:false
    (module T : Command.Enumerable_stringable with type t = t)
;;

(* Ordering of log levels in terms of verbosity. *)
let as_or_more_verbose_than ~log_level ~msg_level =
  let msg_level = Option.value msg_level ~default:`Info in
  compare log_level msg_level <= 0
;;
