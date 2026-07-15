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

module Real_implementation : S = struct
  type t = Tmux.t

  let create ?extra_args ?env ?mouse ?working_dir ~width ~height ~command () =
    Tmux.create ?env ?extra_args ?mouse ?working_dir ~size:{ width; height } ~command ()
  ;;

  let attach session_id = Tmux.attach session_id
  let close t = Tmux.close t
  let closed t = Tmux.closed t

  let get_cursor t =
    let%map.Deferred.Or_error { x; y; cursor_character } = Tmux.get_cursor t in
    { Tmux_cursor.cursor_character; position = { x; y } }
  ;;

  let dump_screen t =
    Tmux.dump_screen ~preserve_trailing_spaces:() ~dump_escape_sequences:() t
  ;;

  let resize t { Dimensions.height; width } = Tmux.resize t { height; width }
  let send_key t key = Tmux.send_key t key
  let send_keys t keys = Tmux.send_keys t keys
end
