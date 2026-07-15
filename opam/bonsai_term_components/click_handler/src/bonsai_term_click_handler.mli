open! Core
open Bonsai_term

(** This is a tiny library that implements [on_click] and [on_right_click] handlers for
    bonsai_term views.

    A click fires only when mouse-down and mouse-release land on the same element (same
    identity and same region). There is no event bubbling; when regions overlap, the
    topmost view (in paint order) wins.

    {b Important:} You must call [handler] at the top level of your app for any click
    handlers to work. See [handler] below.

    {b Choosing between [add_click_handler] and [With_id]:}

    [add_click_handler] identifies elements by their "visual region". This is fine when
    each call site produces at most one on-screen element, but breaks down when a single
    call site is reused for many elements that may move (e.g. rows in a table), because
    two rows can share the same region after a shift.

    [With_id] lets you supply a stable, per-element ID so that identity is preserved even
    when elements move. You should prefer using [With_id]. *)

(** [Context] is also re-exported by [Bonsai_term_hover], which uses the same
    representation. *)
module Context : sig
  (** [relative_position] is the position of the event, translated so it is relative to
      the top-left corner of the [View.t] that the handler was attached to.

      [absolute_position] is the raw screen position reported by the event. Useful when
      you need to position something else (e.g. a floating overlay) at the click. *)
  type t =
    { relative_position : Position.t
    ; absolute_position : Position.t
    }
  [@@deriving sexp_of]
end

module With_id : sig
  module type S = sig
    type t [@@deriving equal]
  end

  (** Like [add_click_handler], but each element is identified by a user-provided ['id]
      rather than its region and path. *)
  val add_click_handler
    :  (module S with type t = 'id)
    -> local_ Bonsai.graph
    -> ('id -> View.t -> on_click:unit Effect.t -> View.t) Bonsai.t

  (** Like [add_click_handler] but for right clicks (Right -> Release). *)
  val add_right_click_handler
    :  (module S with type t = 'id)
    -> local_ Bonsai.graph
    -> ('id -> View.t -> on_right_click:unit Effect.t -> View.t) Bonsai.t
end

(** [on_click ~on_click view] will trigger [on_click] if there are consecutive Left ->
    Release mouse events on [view].

    NOTE: In order for this library to do anything, you need to make a call to [handler]
    at the top-level of your app:

    e.g.:

    {[
      let ~view, ~handler = ... in
      let handler = Aide_cli_click_handler.handler ~view ~handler graph in
      ~view, ~handler
    ]} *)
val add_click_handler
  :  local_ Bonsai.graph
  -> (View.t -> on_click:unit Effect.t -> View.t) Bonsai.t

(** Like [add_click_handler], but the [on_click] callback receives a [Context.t], which
    includes information like the relative position of the click. *)
val add_click_handler_with_context
  :  local_ Bonsai.graph
  -> (View.t -> on_click:(Context.t -> unit Effect.t) -> View.t) Bonsai.t

(** [add_right_click_handler] is like [add_click_handler] but for right clicks (Right ->
    Release). *)
val add_right_click_handler
  :  local_ Bonsai.graph
  -> (View.t -> on_right_click:unit Effect.t -> View.t) Bonsai.t

(** Like [add_right_click_handler], but the [on_right_click] callback receives a
    [Context.t], which includes information like the relative position of the click. *)
val add_right_click_handler_with_context
  :  local_ Bonsai.graph
  -> (View.t -> on_right_click:(Context.t -> unit Effect.t) -> View.t) Bonsai.t

(** In order for this library to work, you must add a call to [handler] at the top level
    of your app like:

    {[
      let ~view, ~handler = ... in
      let handler = Bonsai_term_click_handler.handler ~view ~handler graph in
      ~view, ~handler
    ]} *)
val handler
  :  view:View.t Bonsai.t
  -> handler:(Event.t -> unit Effect.t) Bonsai.t
  -> local_ Bonsai.graph
  -> (Event.t -> unit Effect.t) Bonsai.t

(** [Custom_handler] generalizes click handling to arbitrary user-defined event types.
    Instead of being triggered by mouse clicks, handlers fire when the caller invokes
    [maybe_handle] with an ['item] and a screen position.

    This is useful for bonsai_vim / bonsai_emacs where events like key presses at a cursor
    position should trigger actions on the view element under the cursor.

    Usage:
    {[
      (* Define your event type *)
      module My_event = struct
        type t = Enter | Press_e [@@deriving compare, sexp_of]
        include (val Comparator.make ~compare ~sexp_of_t)
      end

      (* Create the handler *)
      let { Custom_handler.create_handler; maybe_handle } =
        Custom_handler.create_custom_handler (module My_event)
      in

      (* Tag views with handlers. [on_event] receives a [Context.t], which includes
      information like the relative position of the event. *)
      let tag_view = create_handler (module Int) graph in
      let view =
        let%arr tag_view in
        tag_view ~id:row_id ~item:Enter ~on_event:(fun _pos -> my_effect) my_view
      in

      (* When an event arrives, dispatch it *)
      maybe_handle Enter cursor_position current_view
    ]} *)
module Custom_handler : sig
  type 'item t =
    { create_handler :
        'id.
        (module With_id.S with type t = 'id)
        -> local_ Bonsai.graph
        -> (id:'id
            -> item:'item
            -> on_event:(Context.t -> unit Effect.t)
            -> View.t
            -> View.t)
             Bonsai.t
    (** [on_event] receives the [Position.t] that [maybe_handle] was called with,
        translated so it is relative to the top-left corner of the tagged view. *)
    ; maybe_handle : 'item -> Position.t -> View.t -> Captured_or_ignored.t Effect.t
    }

  val create_custom_handler : (module Comparable.S with type t = 'item) -> 'item t
end

module Private : sig
  (** Opaque identity used by handlers to compare two regions for equality across
      re-renders. Useful for sharing identity logic between handlers (e.g.
      [Bonsai_term_hover]); ordinary callers should not need this. *)
  module Identity : sig
    type t

    module Maker : sig
      type identity := t
      type 'id t

      val create : equal:('id -> 'id -> bool) -> 'id t
      val make : 'id t -> 'id -> identity
    end

    val same : t -> t -> bool
  end
end
