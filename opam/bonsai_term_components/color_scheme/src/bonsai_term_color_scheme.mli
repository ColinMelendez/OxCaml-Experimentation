open! Core
open Bonsai_term
open Bonsai

(** [Bonsai_term_color_scheme] is a library that contains several different kinds of
    editor-like color schemes. It contains the Catppuccin color scheme, along with
    additional color schemes whose individual colors look ~similar to Catppuccin variants. *)
module Catppuccin : sig
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

    (** Returns the [Rgb.t] if the value is [Rgb _], or an approximate RGB based on
        standard xterm defaults if the value is [Ansi_16 _]. *)
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

  (** Returns the [Rgb.t] for the color in the given flavor. For flavors that use
      [Ansi_16] colors, returns approximate RGB values based on standard xterm defaults. *)
  val to_rgb : Flavor.t -> t -> Rgb.t
end

include
  module type of Catppuccin
  with type t = Catppuccin.t
   and type Rgb.t = Catppuccin.Rgb.t
   and type Ansi_16.t = Catppuccin.Ansi_16.t
   and type Color_value.t = Catppuccin.Color_value.t
   and type Flavor.t = Catppuccin.Flavor.t
   and type comparator_witness = Catppuccin.comparator_witness

val color' : t Bonsai.t -> local_ Bonsai.graph -> Attr.Color.t Bonsai.t

(** The current color-scheme flavor. If no surrounding [set_flavor_within] or
    [set_flavor_within_app] has set a flavor, this defaults to the user's
    [BONSAI_TERM_COLOR_SCHEME] environment variable when it names a known [Flavor_name.t],
    or [Mocha.flavor] otherwise. *)
val flavor : local_ Bonsai.graph -> Flavor.t Bonsai.t

val set_flavor_within
  :  Flavor.t Bonsai.t
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

(** A version of [set_flavor_within] that is specialized for the common "application" type *)
val set_flavor_within_app
  :  Flavor.t Bonsai.t
  -> (local_ Bonsai.graph -> view:View.t Bonsai.t * handler:'b Bonsai.t)
  -> local_ Bonsai.graph
  -> view:View.t Bonsai.t * handler:'b Bonsai.t

module Vscode_dark : sig
  val flavor : Flavor.t
end

module Vscode_light : sig
  val flavor : Flavor.t
end

module Gruvbox_dark : sig
  val flavor : Flavor.t
end

module Gruvbox_light : sig
  val flavor : Flavor.t
end

module Dracula : sig
  val flavor : Flavor.t
end

module Kanagawa : sig
  val flavor : Flavor.t
end

module Tokyo_night_dark : sig
  val flavor : Flavor.t
end

module Tokyo_night_light : sig
  val flavor : Flavor.t
end

module Monokai : sig
  val flavor : Flavor.t
end

module Bluloco : sig
  val flavor : Flavor.t
end

module Solarized_dark : sig
  val flavor : Flavor.t
end

module Solarized_light : sig
  val flavor : Flavor.t
end

(** A theme that uses only the 16 standard ANSI terminal colors. Equivalent to the
    Catppuccin [Espresso] flavor. The actual displayed colors are determined by the user's
    terminal configuration, making this theme compatible with any terminal color scheme. *)
module Terminal_16 : sig
  val flavor : Flavor.t
end

(** Like [Terminal_16] but with inverted background/foreground roles, resulting in a light
    (typically white) background on most terminal configurations. *)
module Terminal_16_inverted : sig
  val flavor : Flavor.t
end

(** A variant of the valid [Flavor]s defined in this library, including the Catppuccin
    flavors exposed by [Catppuccin.Flavor_name]. *)
module Flavor_name : sig
  type t =
    | Catppuccin of Catppuccin.Flavor_name.t
    | Vscode_dark
    | Vscode_light
    | Gruvbox_dark
    | Gruvbox_light
    | Dracula
    | Kanagawa
    | Tokyo_night_dark
    | Tokyo_night_light
    | Monokai
    | Bluloco
    | Solarized_dark
    | Solarized_light
    | Terminal_16
    | Terminal_16_inverted
  [@@deriving sexp ~portable, equal ~portable, enumerate, compare ~portable, string]

  (** Translates flavor names into a "human-readable" string. The only difference between
      this and the derived [to_string] is that Catppuccin flavors produce bare names (e.g.
      [Mocha] instead of [Catppuccin_Mocha]). *)
  val to_string_hum : t -> string

  (** Extends the derived [of_string] to also accept bare Catppuccin names (e.g. [Mocha]),
      in addition to non-Catppuccin names ). *)
  val of_string : string -> t

  val to_flavor : t -> Flavor.t
end

(** The name of the environment variable users set to choose their default color scheme. *)
val color_scheme_env_var_name : string

(** Reads the [BONSAI_TERM_COLOR_SCHEME] environment variable and returns the
    corresponding [Flavor.t] if the value is one of the names produced by
    [Flavor_name.to_string_hum]. *)
val get_user_default_theme : unit -> Flavor.t option

(** Like [get_user_default_theme], but returns a [Flavor_name.t] instead of [Flavor.t]. *)
val get_user_default_theme_name : unit -> Flavor_name.t option

module For_testing : sig
  val reset_emitted_env_var_warning : unit -> unit
end
