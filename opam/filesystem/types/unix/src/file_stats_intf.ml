open! Core

module type S = sig @@ portable
  include Filesystem_types.File_stats.S (** @inline *)

  (** Conversions *)

  val of_unix_stats : Core_unix.stats -> t
  val to_unix_stats : t -> Core_unix.stats
end

module type File_stats = sig
  module type S = S

  include
    S
    with module File_kind := File_kind
     and module File_permissions := Filesystem_types.File_permissions
     and type t = Filesystem_types.File_stats.t
end
