open! Core
open Bonsai_term

module Action : sig
  type t =
    | Scroll_to of
        { bottom : int
        ; top : int
        }
    | Up
    | Down
    | Top
    | Bottom
    | Up_half_screen
    | Down_half_screen
    | Stick_to_bottom
  [@@deriving sexp_of]
end

module Scroll_position : sig
  type t =
    | Top
    | Bottom
    | Percentage of Percent.t
    | All_visible
  [@@deriving sexp_of]
end

type t =
  { view : View.t
  ; inject : Action.t -> unit Effect.t
  ; less_keybindings_handler : Event.t -> Captured_or_ignored.t Effect.t
  ; stuck_to_bottom : bool
  (** Whether the scroller is in "stick to bottom" mode, i.e. it will automatically follow
      new content appended at the bottom. This is a mode flag, not a position indicator. *)
  ; scroll_position : Scroll_position.t
  }
(* [less_keybindings] will provide "less-like" keybindings. The keybindings are:

   [ up arrow ] or [ k ] -> Up [ down arrow ] or [ j ] -> Down [ d ] or [ ctrl + d ] ->
   Down_half_screen [ u ] or [ ctrl + u ] -> Up_half_screen [ gg ] -> Top [ G ] -> Bottom *)

(** [component ~dimensions view], will make a region of size [dimensions] containing
    [view]. If [view] is vertically bigger than [dimensions.height] then the region will
    be "scrollable". You can scroll by scheduling [inject] actions.

    A default "handler" for events with less-like navigation is provided as a helper
    utility. *)
val component
  :  ?default_stuck_to_bottom:bool
  -> crop_width_if_too_big:[ `No | `Yes ]
  -> dimensions:Dimensions.t Bonsai.t
  -> View.t Bonsai.t
  -> local_ Bonsai.graph
  -> t Bonsai.t

module Scrollbar : sig
  module Style : sig
    (** A vertical scrollbar rendered as a narrow column (aka track) with a thumb
        indicator. *)
    val vertical_bar
      :  ?track_attrs:Attr.t list
      -> ?thumb_attrs:Attr.t list
      -> scroll_position:Scroll_position.t
      -> height:int
      -> unit
      -> View.t

    (** A text label in the style of vim's status line: "Top", "Bot", "All", or a
        percentage. *)
    val vim_status
      :  ?attrs:Attr.t list
      -> scroll_position:Scroll_position.t
      -> unit
      -> View.t
  end
end
