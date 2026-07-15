open! Core

module type S = sig @@ portable
  include Filesystem_types.File_kind.S (** @inline *)

  (** Conversions (Unix) *)

  val to_unix_file_kind : t -> Core_unix.file_kind
  val of_unix_file_kind : Core_unix.file_kind -> t
end

module type File_kind = sig
  module type S = S

  include S with type t = Filesystem_types.File_kind.t
end
