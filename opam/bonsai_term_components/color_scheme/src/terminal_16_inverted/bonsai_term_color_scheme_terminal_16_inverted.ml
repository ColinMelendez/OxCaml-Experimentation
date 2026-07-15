open! Core
open Bonsai_term_color_scheme_catppuccin
open Bonsai_term_color_scheme_catppuccin.Ansi_16

let ansi_16 c = Color_value.Ansi_16 c

let flavor : Flavor.t =
  { rosewater = ansi_16 Red
  ; flamingo = ansi_16 Light_red
  ; pink = ansi_16 Magenta
  ; mauve = ansi_16 Light_magenta
  ; red = ansi_16 Red
  ; maroon = ansi_16 Light_red
  ; peach = ansi_16 Yellow
  ; yellow = ansi_16 Light_yellow
  ; green = ansi_16 Green
  ; teal = ansi_16 Cyan
  ; sky = ansi_16 Blue
  ; sapphire = ansi_16 Blue
  ; blue = ansi_16 Blue
  ; lavender = ansi_16 Magenta
  ; text = ansi_16 Black
  ; subtext1 = ansi_16 Black
  ; subtext0 = ansi_16 Light_black
  ; overlay2 = ansi_16 Light_black
  ; overlay1 = ansi_16 Light_black
  ; overlay0 = ansi_16 Light_black
  ; surface2 = ansi_16 Light_black
  ; surface1 = ansi_16 White
  ; surface0 = ansi_16 White
  ; base = ansi_16 Light_white
  ; mantle = ansi_16 Light_white
  ; crust = ansi_16 Light_white
  ; is_dark = false
  }
;;
