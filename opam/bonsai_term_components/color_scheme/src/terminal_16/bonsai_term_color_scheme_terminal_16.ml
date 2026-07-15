open! Core
open Bonsai_term_color_scheme_catppuccin
open Bonsai_term_color_scheme_catppuccin.Ansi_16

let ansi_16 c = Color_value.Ansi_16 c

let flavor : Flavor.t =
  { rosewater = ansi_16 Light_red
  ; flamingo = ansi_16 Red
  ; pink = ansi_16 Light_magenta
  ; mauve = ansi_16 Magenta
  ; red = ansi_16 Red
  ; maroon = ansi_16 Light_red
  ; peach = ansi_16 Light_yellow
  ; yellow = ansi_16 Yellow
  ; green = ansi_16 Green
  ; teal = ansi_16 Cyan
  ; sky = ansi_16 Light_cyan
  ; sapphire = ansi_16 Light_blue
  ; blue = ansi_16 Blue
  ; lavender = ansi_16 Light_magenta
  ; text = ansi_16 White
  ; subtext1 = ansi_16 White
  ; subtext0 = ansi_16 Light_black
  ; overlay2 = ansi_16 Light_black
  ; overlay1 = ansi_16 Light_black
  ; overlay0 = ansi_16 Light_black
  ; surface2 = ansi_16 Light_black
  ; surface1 = ansi_16 Light_black
  ; surface0 = ansi_16 Light_black
  ; base = ansi_16 Black
  ; mantle = ansi_16 Black
  ; crust = ansi_16 Black
  ; is_dark = true
  }
;;
