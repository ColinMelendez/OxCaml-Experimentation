open! Core
open Bonsai_term

(** This library contains hard-coded constants for Catppuccin colors:
    https://github.com/catppuccin/catppuccin *)

(** The color space that, combined with a [Flavor], become an actual [Rgb] value *)
type t =
  | Rosewater
  | Flamingo
  | Pink
  | Mauve
  | Red
  | Maroon
  | Peach
  | Yellow
  | Green
  | Teal
  | Sky
  | Sapphire
  | Blue
  | Lavender
  | Text
  | Subtext1
  | Subtext0
  | Overlay2
  | Overlay1
  | Overlay0
  | Surface2
  | Surface1
  | Surface0
  | Base
  | Mantle
  | Crust
[@@deriving sexp, equal, enumerate, compare, string]

include Comparable.S_plain with type t := t

module Rgb : sig
  type t =
    { r : int
    ; b : int
    ; g : int
    }
  [@@deriving globalize, sexp ~portable, equal ~portable, compare]
end

(** The standard 16 ANSI terminal colors plus the terminal default color. Using these
    causes the terminal to emit SGR 30-37 / 90-97 style escape codes, so the actual
    displayed color is controlled by the user's terminal configuration rather than
    hardcoded RGB values. *)
module Ansi_16 : sig
  type t =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Light_black
    | Light_red
    | Light_green
    | Light_yellow
    | Light_blue
    | Light_magenta
    | Light_cyan
    | Light_white
    | Default
  [@@deriving sexp ~portable, equal ~portable, compare ~portable]

  val to_attr_color : t -> Attr.Color.t

  (** Returns approximate RGB values (based on standard xterm defaults) for use in
      contexts that require RGB, such as color-distance calculations. *)
  val approximate_rgb : t -> Rgb.t
end

(** A color value that is either a concrete RGB triple or one of the 16 standard ANSI
    terminal colors. *)
module Color_value : sig
  type t =
    | Rgb of Rgb.t
    | Ansi_16 of Ansi_16.t
  [@@deriving sexp ~portable, equal ~portable, compare ~portable]

  val to_attr_color : t -> Attr.Color.t

  (** Returns the [Rgb.t] if the value is [Rgb _], or an approximate RGB based on standard
      xterm defaults if the value is [Ansi_16 _]. *)
  val to_approximate_rgb : t -> Rgb.t
end

(** The mapping from the color space to concrete color values *)
module Flavor : sig
  type t =
    { rosewater : Color_value.t
    ; flamingo : Color_value.t
    ; pink : Color_value.t
    ; mauve : Color_value.t
    ; red : Color_value.t
    ; maroon : Color_value.t
    ; peach : Color_value.t
    ; yellow : Color_value.t
    ; green : Color_value.t
    ; teal : Color_value.t
    ; sky : Color_value.t
    ; sapphire : Color_value.t
    ; blue : Color_value.t
    ; lavender : Color_value.t
    ; text : Color_value.t
    ; subtext1 : Color_value.t
    ; subtext0 : Color_value.t
    ; overlay2 : Color_value.t
    ; overlay1 : Color_value.t
    ; overlay0 : Color_value.t
    ; surface2 : Color_value.t
    ; surface1 : Color_value.t
    ; surface0 : Color_value.t
    ; base : Color_value.t
    ; mantle : Color_value.t
    ; crust : Color_value.t
    ; is_dark : bool
    }
  [@@deriving sexp ~portable, equal ~portable, compare]

  val is_dark : t -> bool
  val map_color_values : t -> f:(Color_value.t -> Color_value.t) -> t
end

module Latte : sig
  val flavor : Flavor.t
end

module Mocha : sig
  val flavor : Flavor.t
end

module Frappe : sig
  val flavor : Flavor.t
end

module Macchiato : sig
  val flavor : Flavor.t
end

(** A variant of the valid [Flavor]s defined in this library *)

module Flavor_name : sig
  type t =
    | Mocha
    | Macchiato
    | Frappe
    | Latte
  [@@deriving sexp ~portable, equal ~portable, compare ~portable, enumerate, string]

  val to_flavor : t -> Flavor.t
end

val color : flavor:Flavor.t -> t -> Attr.Color.t
val to_color_value : Flavor.t -> t -> Color_value.t

(** Returns the [Rgb.t] for the color in the given flavor. For flavors that use [Ansi_16]
    colors, returns approximate RGB values based on standard xterm defaults. *)
val to_rgb : Flavor.t -> t -> Rgb.t

val rgb : int * int * int -> Color_value.t
