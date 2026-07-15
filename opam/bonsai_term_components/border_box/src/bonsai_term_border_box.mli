open! Core
open Bonsai_term

module Line_type : sig
  (* {v
                       ┌──────┐
      Thin:            │hello!│
                       └──────┘
                       ┏━━━━━━┓
      Thick:           ┃hello!┃
                       ┗━━━━━━┛
                       ╔══════╗
      Double:          ║hello!║
                       ╚══════╝
                       ╭──────╮
      Rounded_corners: │hello!│
                       ╰──────╯
     v} *)
  type t =
    | Thin
    | Thick
    | Double
    | Round_corners

  val vertical_bar : t -> string
  val horizontal_bar : t -> string
  val top_left : t -> string
  val bottom_left : t -> string
  val bottom_right : t -> string
  val top_right : t -> string
  val vertical_right : t -> string
  val vertical_left : t -> string
  val horizontal_down : t -> string
  val horizontal_up : t -> string
end

(** [line_type] defaults to [Thin].

    Hiding a side removes the border on the respective side (and decreases the size of the
    returned [View.t] by 1 for that axis). Example of hiding the top and right sides:
    {v
    │hello!
    └──────
    v} *)
val view
  :  ?line_type:Line_type.t
  -> ?hide_left:bool
  -> ?hide_right:bool
  -> ?hide_top:bool
  -> ?hide_bottom:bool
  -> ?left_padding:int
       (** Adds padding to the left side of the box. Padding is opaque and not transparent *)
  -> ?right_padding:int
       (** Adds padding to the right side of the box. Padding is opaque and not
           transparent *)
  -> ?top_padding:int
       (** Adds padding to the top of the box. Padding is opaque and not transparent *)
  -> ?bottom_padding:int
       (** Adds padding to the bottom of the box. Padding is opaque and not transparent *)
  -> ?title:string (** Adds a title to the box with 1 space of left and right padding *)
  -> ?title_attrs:Attr.t list
  -> ?subtitle:string
       (** Adds a subtitle under the title with 1 space of left and right padding *)
  -> ?subtitle_attrs:Attr.t list
  -> ?attrs:Attr.t list
  -> View.t
  -> View.t

(** [view'] is like [view], but allows arbitrary [View.t] in the title and subtitle. *)
val view'
  :  ?line_type:Line_type.t
  -> ?hide_left:bool
  -> ?hide_right:bool
  -> ?hide_top:bool
  -> ?hide_bottom:bool
  -> ?left_padding:int
       (** Adds padding to the left side of the box. Padding is opaque and not transparent *)
  -> ?right_padding:int
       (** Adds padding to the right side of the box. Padding is opaque and not
           transparent *)
  -> ?top_padding:int
       (** Adds padding to the top of the box. Padding is opaque and not transparent *)
  -> ?bottom_padding:int
       (** Adds padding to the bottom of the box. Padding is opaque and not transparent *)
  -> ?title:View.t (** Adds a title to the box. Should be 1 line width high *)
  -> ?subtitle:View.t (** Adds a subtitle under the title. Should be 1 line width high *)
  -> ?attrs:Attr.t list
  -> View.t
  -> View.t
