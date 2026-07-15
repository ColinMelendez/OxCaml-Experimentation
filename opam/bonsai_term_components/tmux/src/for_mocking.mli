open! Core
open Async
open Bonsai_term

module type S = sig
  type t

  val create
    :  ?extra_args:string list
    -> ?env:Core_unix.env
    -> ?mouse:unit
    -> ?working_dir:string
    -> width:int
    -> height:int
    -> command:string
    -> unit
    -> t Deferred.Or_error.t

  val attach : Tmux.Session_id.t -> t Deferred.Or_error.t
  val close : t -> unit Deferred.Or_error.t
  val closed : t -> unit Deferred.t
  val get_cursor : t -> Tmux_cursor.t Deferred.Or_error.t
  val dump_screen : t -> string list Deferred.Or_error.t
  val resize : t -> Dimensions.t -> unit Deferred.Or_error.t
  val send_key : t -> Tmux.Key.t -> unit Deferred.Or_error.t
  val send_keys : t -> Tmux.Key.t list -> unit Deferred.Or_error.t
end

module Real_implementation : S
