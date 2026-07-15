open! Core
open Bonsai_term_color_scheme_catppuccin

(* Kanagawa theme color mappings *)
let flavor : Flavor.t =
  { rosewater = rgb (255, 160, 102) (* Kanagawa surimiOrange *)
  ; flamingo = rgb (255, 93, 98) (* Kanagawa samuraiRed *)
  ; pink = rgb (213, 110, 154) (* Kanagawa sakuraPink *)
  ; mauve = rgb (149, 127, 184) (* Kanagawa oniViolet *)
  ; red = rgb (195, 64, 67) (* Kanagawa autumnRed *)
  ; maroon = rgb (195, 64, 67) (* Kanagawa autumnRed *)
  ; peach = rgb (255, 160, 102) (* Kanagawa surimiOrange *)
  ; yellow = rgb (230, 195, 132) (* Kanagawa autumnYellow *)
  ; green = rgb (152, 187, 108) (* Kanagawa springGreen *)
  ; teal = rgb (106, 149, 137) (* Kanagawa waveAqua1 *)
  ; sky = rgb (126, 156, 216) (* Kanagawa springBlue *)
  ; sapphire = rgb (126, 156, 216) (* Kanagawa springBlue *)
  ; blue = rgb (126, 156, 216) (* Kanagawa springBlue *)
  ; lavender = rgb (149, 127, 184) (* Kanagawa oniViolet *)
  ; text = rgb (220, 215, 186) (* Kanagawa fujiWhite *)
  ; subtext1 = rgb (195, 192, 176) (* Kanagawa oldWhite *)
  ; subtext0 = rgb (169, 168, 166) (* Slightly dimmed *)
  ; overlay2 = rgb (114, 113, 105) (* Kanagawa sumiInk4 *)
  ; overlay1 = rgb (84, 84, 109) (* Kanagawa sumiInk3 *)
  ; overlay0 = rgb (73, 73, 92) (* Kanagawa sumiInk2 *)
  ; surface2 = rgb (54, 54, 79) (* Kanagawa sumiInk1 *)
  ; surface1 = rgb (43, 43, 69) (* Kanagawa sumiInk0 *)
  ; surface0 = rgb (33, 33, 51) (* Darker surface *)
  ; base = rgb (31, 31, 40) (* Kanagawa background *)
  ; mantle = rgb (26, 26, 35) (* Darker background *)
  ; crust = rgb (22, 22, 29)
  ; is_dark = true
  }
;;
