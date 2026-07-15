@@ portable

open! Core
open! Import

(** Describes both the level of a log and the level of a message sent to a log. There is
    an ordering to levels (`Debug < `Info < `Warn < `Error), and a log set to a level will
    never display messages at a lower log level.

    The level you set your messages or logs to are ultimately up to you, but the comments
    below provide some reasonable default semantics.

    Messages without a level are treated as `Info. *)
type t =
  [ `Debug
    (** Verbose events that you may want to turn on selectively for one-off debugging. *)
  | `Info
    (** General events that are useful to record for later investigations.

        Default level for messages, as well as the default threshold for [Log.t]s. *)
  | `Warn
    (** Events that indicate application oddities, but that don't cause the operation to
        fail.

        E.g., switching from primary to backup, retrying an operation, missing secondary
        data, etc. *)
  | `Error
    (** Events that indicate an operation has failed - but not necessarily the entire
        service or application - and that usually require prompt human attention. *)
  ]
[@@deriving
  bin_io, compare ~localize, enumerate, equal ~localize, globalize, sexp, sexp_grammar]

include Stringable with type t := t

val arg : t Command.Spec.Arg_type.t @@ nonportable
val as_or_more_verbose_than : log_level:t -> msg_level:t option -> bool

module Stable : sig
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, compare ~localize, sexp, stable_witness]
  end

  module V1 : sig
    type nonrec t [@@deriving bin_io, compare ~localize, sexp, stable_witness]

    val to_v2 : t -> V2.t
    val of_v2 : V2.t -> t
  end
end
