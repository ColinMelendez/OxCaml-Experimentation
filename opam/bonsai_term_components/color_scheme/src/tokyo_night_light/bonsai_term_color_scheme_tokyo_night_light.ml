open! Core
open Bonsai_term_color_scheme_catppuccin

(* Tokyo Night Light theme color mappings *)
let flavor : Flavor.t =
  { rosewater = rgb (150, 80, 39) (* Brown orange *)
  ; flamingo = rgb (143, 94, 21) (* Brown *)
  ; pink = rgb (143, 94, 21) (* Brown *)
  ; mauve = rgb (90, 74, 120) (* Purple *)
  ; red = rgb (143, 94, 21) (* Tokyo Night Light red/brown *)
  ; maroon = rgb (143, 94, 21) (* Brown *)
  ; peach = rgb (150, 80, 39) (* Tokyo Night Light orange *)
  ; yellow = rgb (143, 94, 21) (* Tokyo Night Light yellow/brown *)
  ; green = rgb (72, 94, 48) (* Tokyo Night Light green *)
  ; teal = rgb (22, 103, 117) (* Tokyo Night Light teal *)
  ; sky = rgb (7, 101, 133) (* Tokyo Night Light sky *)
  ; sapphire = rgb (52, 84, 138) (* Tokyo Night Light blue *)
  ; blue = rgb (52, 84, 138) (* Tokyo Night Light blue *)
  ; lavender = rgb (90, 74, 120) (* Tokyo Night Light purple *)
  ; text = rgb (52, 59, 88) (* Tokyo Night Light foreground *)
  ; subtext1 = rgb (76, 82, 106) (* Slightly lighter *)
  ; subtext0 = rgb (100, 105, 124) (* More lighter *)
  ; overlay2 = rgb (130, 135, 150) (* Tokyo Night Light comment *)
  ; overlay1 = rgb (150, 154, 166) (* Lighter overlay *)
  ; overlay0 = rgb (169, 173, 182) (* Even lighter overlay *)
  ; surface2 = rgb (188, 192, 199) (* Tokyo Night Light bg_highlight *)
  ; surface1 = rgb (208, 211, 215) (* Tokyo Night Light bg_visual *)
  ; surface0 = rgb (227, 229, 232) (* Tokyo Night Light bg_dark *)
  ; base = rgb (213, 214, 219) (* Tokyo Night Light bg *)
  ; mantle = rgb (235, 236, 238) (* Lighter background *)
  ; crust = rgb (245, 245, 246)
  ; is_dark = false
  }
;;
