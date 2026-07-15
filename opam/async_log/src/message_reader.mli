open! Core
open! Async_kernel
open! Import

module type T = sig
  type t = Message.t [@@deriving of_sexp, bin_read]
end

(** [pipe format filename] returns a pipe of all the messages in the log. Errors
    encountered when opening or reading the file will be thrown as exceptions into the
    monitor current at the time [pipe] is called. *)
val pipe
  :  (module T)
  -> [< Output.Format.machine_readable ]
  -> string
  -> Message.t Pipe.Reader.t

val pipe_of_reader
  :  (module T)
  -> [< Output.Format.machine_readable ]
  -> Reader.t
  -> Message.t Pipe.Reader.t

module Expert : sig
  (** [read_one format reader] reads a single log message from the reader, advancing the
      position of the reader to the next log entry. *)
  val read_one
    :  (module T)
    -> [< Output.Format.machine_readable ]
    -> Reader.t
    -> Message.t Reader.Read_result.t Deferred.t
end
