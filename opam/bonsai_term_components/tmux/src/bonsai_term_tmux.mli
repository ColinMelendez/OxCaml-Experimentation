open! Core
open Bonsai_term
module For_mocking = For_mocking
module Tmux_cursor = Tmux_cursor

module Persistence : sig
  (** [Persistence.t] determines what happens to the [run] command. Upon de-activations of
      your bonsai component. *)
  type t =
    | Kill_command_when_component_deactivates
    (** The [command] will stop running as soon as your bonsai computation is deactivated. *)
    | Keep_command_alive_if_component_deactivates
    (** The [command] will keep running if the component de-activates. The command will
        still be attempted to be skilled at program shutdown though this is less reliable
        if the parent program gets forcefully killed. You can choose when the program is
        killed by scheduling a [close] effect manually. *)
end

module Mouse : sig
  type t =
    | Classic
    (** Forward mouse events to the embedded program using classic xterm encoding. This is
        useful for applications that expect terminal mouse escape sequences directly. *)
    | Scrollback
    (** Use wheel events to drive tmux's own scrollback / copy-mode behavior instead of
        forwarding mouse escape sequences into the embedded program. Other mouse events
        are ignored. *)
end

type t = private
  { last_view : View.t Pending_or_error.t (** The last successful view. *)
  ; handler : Event.t -> unit Effect.t
  (** [handler] lets you feed events to the embedded terminal UI. *)
  ; last_cursor : Tmux_cursor.t Pending_or_error.t (** The most recent cursor location. *)
  ; is_closed : bool (** [is_closed] tells you if the running program has finished. *)
  ; close : unit Effect.t (** [close] will let you close the embedded terminal UI. *)
  ; reinstantiate_command : unit Effect.t
  (** [reinstantiate_command] lets you re-run the embedded terminal command. *)
  }

(** [Bonsai_term_tmux.component] lets you embed other interactive terminal UIs. (e.g.
    embedding htop / nvim / emacs -nw / ...). This is kind of like the terminal version of
    a web "iframe".

    NOTE: embedded terminal programs may not support all terminal features namely:

    - OSC 8 hyperlinks.
    - OSC 52 clipboard.

    If there are any features that are missing that you would need for a usecase please
    let us know and we can look into adding it!

    [persistence] determines what happens to the [run] command. Upon de-activations of
    your bonsai component. More details in the [Persistence.t] module. Defaults to
    [Kill_command_when_component_deactivates].

    [do_not_render_cursor] will stop rendering the virtual cursor that we render. Letting
    you render your own "real" / "non-virtual" cursor.

    [command] is the bash string of the program that you want to embed. e.g. "nvim".

    [dimensions] determines the height and width of the embedded program.

    [extra_tmux_args] are extra args that will get passed to tmux.

    [mouse] enables mouse handling for the embedded tmux session. [Classic] forwards mouse
    escape sequences to the child program; [Scrollback] uses wheel events to drive tmux's
    own copy-mode scrollback behavior.

    [working_dir], when provided, is the directory the tmux server will [chdir]
    to *before* spawning the embedded command's shell. Use this when shell initialization
    reads values that depend on the current directory (e.g. per-workspace environment
    variables such as [HGROOT] or [FE_WORKSPACE]): prefixing [command] with [cd <dir> &&]
    is *not* equivalent, because shell init runs before the embedded [cd]. Changing
    [working_dir] invalidates the scoped model and causes a fresh pane to be instantiated,
    just like changing [command] or [env].

    [recolor_to_flavor] lets you make the embedded command attempt to match the colors of
    a color scheme, by finding the "closest" color in RGB space.

    [active] controls whether the embedded tmux session is polled every frame. Defaults to
    [true]. When [false], the tmux process stays alive but screen capturing and cursor
    polling are paused, which is useful when many tmux components are kept warm
    simultaneously (e.g. a multi-pane TUI) but only a subset are visible. *)
val component
  :  ?for_mocking:(module For_mocking.S)
  -> ?persistence:Persistence.t
  -> ?do_not_render_cursor:unit
  -> ?extra_tmux_args:string list
  -> ?mouse:Mouse.t
  -> ?env:Core_unix.env option Bonsai.t
  -> ?working_dir:string option Bonsai.t
  -> ?recolor_to_flavor:
       (flavor:Bonsai_term_color_scheme.Flavor.t * bg:Bonsai_term_color_scheme.t) option
         Bonsai.t
  -> ?active:bool Bonsai.t
  -> command:string Bonsai.t
  -> dimensions:Dimensions.t Bonsai.t
  -> local_ Bonsai.graph
  -> t Bonsai.t

(** The result of [attach_component]. Similar to [t] but without [reinstantiate_command],
    since the session is managed by another process. *)
module Attached : sig
  type t = private
    { last_view : View.t Pending_or_error.t
    ; handler : Event.t -> unit Effect.t
    ; last_cursor : Tmux_cursor.t Pending_or_error.t
    ; is_closed : bool
    ; close : unit Effect.t
    }
end

(** [attach_component] is like [component] but attaches to an existing tmux session
    identified by a [Tmux.Session_id.t], rather than creating a new one. This is intended
    for use when one process creates a tmux session and another process displays and
    interacts with it.

    The session lifecycle is managed by the process that created it. Calling [close] will
    still kill the session. *)
val attach_component
  :  ?for_mocking:(module For_mocking.S)
  -> ?do_not_render_cursor:unit
  -> ?recolor_to_flavor:
       (flavor:Bonsai_term_color_scheme.Flavor.t * bg:Bonsai_term_color_scheme.t) option
         Bonsai.t
  -> session_id:Tmux.Session_id.t Bonsai.t
  -> dimensions:Dimensions.t Bonsai.t
  -> local_ Bonsai.graph
  -> Attached.t Bonsai.t
