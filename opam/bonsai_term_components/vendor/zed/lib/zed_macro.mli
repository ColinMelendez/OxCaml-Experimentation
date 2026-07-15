(*
 * zed_macro.mli
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

(** Macro recorder *)

(** Type of macro recorders. *)
type 'a t

(** [create macro] create a new macro recorder, with initial contents [macro]. *)
val create : 'a list -> 'a t

(** Whether the recorder is recording a macro. *)
val recording : 'a t -> bool React.signal

(** Returns the current state of the recorder. *)
val get_recording : 'a t -> bool

(** Starts or stops the macro recorder. *)
val set_recording : 'a t -> bool -> unit

(** Cancels the current macro if recording one. *)
val cancel : 'a t -> unit

(** The number of actions in the macro recorder. It is [0] if the recorder is not
    currently recording. *)
val count : 'a t -> int React.signal

(** Returns the current number of actions in the macro recorder. *)
val get_count : 'a t -> int

(** [add recorder x] adds [x] to the recorder if it is recording a macro. *)
val add : 'a t -> 'a -> unit

(** Returns the currently recorded macro. *)
val contents : 'a t -> 'a list

(** The contents of the macro counter. *)
val counter : 'a t -> int React.signal

(** Gets the contents of the macro counter. *)
val get_counter : 'a t -> int

(** Sets the macro counter to the given value. *)
val set_counter : 'a t -> int -> unit

(** Adds the given value to the macro counter. *)
val add_counter : 'a t -> int -> unit
