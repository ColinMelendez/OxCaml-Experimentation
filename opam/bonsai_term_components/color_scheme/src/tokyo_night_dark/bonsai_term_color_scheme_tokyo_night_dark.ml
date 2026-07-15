open! Core
open Bonsai_term_color_scheme_catppuccin

(* Tokyo Night Dark theme color mappings *)
let flavor : Flavor.t =
  { rosewater = rgb (255, 158, 100) (* Orange *)
  ; flamingo = rgb (247, 118, 142) (* Red pink *)
  ; pink = rgb (247, 118, 142) (* Red pink *)
  ; mauve = rgb (187, 154, 247) (* Purple *)
  ; red = rgb (247, 118, 142) (* Tokyo Night red *)
  ; maroon = rgb (219, 75, 75) (* Darker red *)
  ; peach = rgb (255, 158, 100) (* Tokyo Night orange *)
  ; yellow = rgb (224, 175, 104) (* Tokyo Night yellow *)
  ; green = rgb (158, 206, 106) (* Tokyo Night green *)
  ; teal = rgb (26, 188, 156) (* Tokyo Night teal *)
  ; sky = rgb (125, 207, 255) (* Tokyo Night sky *)
  ; sapphire = rgb (122, 162, 247) (* Tokyo Night blue *)
  ; blue = rgb (122, 162, 247) (* Tokyo Night blue *)
  ; lavender = rgb (187, 154, 247) (* Tokyo Night purple *)
  ; text = rgb (192, 202, 245) (* Tokyo Night foreground *)
  ; subtext1 = rgb (169, 177, 214) (* Slightly dimmed *)
  ; subtext0 = rgb (146, 153, 184) (* More dimmed *)
  ; overlay2 = rgb (86, 95, 137) (* Tokyo Night comment *)
  ; overlay1 = rgb (68, 75, 106) (* Darker overlay *)
  ; overlay0 = rgb (52, 59, 88) (* Even darker overlay *)
  ; surface2 = rgb (41, 46, 66) (* Tokyo Night bg_highlight *)
  ; surface1 = rgb (36, 40, 59) (* Tokyo Night bg_visual *)
  ; surface0 = rgb (32, 36, 54) (* Tokyo Night bg_dark *)
  ; base = rgb (26, 27, 38) (* Tokyo Night bg *)
  ; mantle = rgb (22, 23, 34) (* Darker background *)
  ; crust = rgb (20, 21, 30)
  ; is_dark = true
  }
;;
