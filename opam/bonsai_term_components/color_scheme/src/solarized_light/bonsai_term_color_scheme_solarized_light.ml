open! Core
open Bonsai_term_color_scheme_catppuccin

(* Solarized Light theme color mappings *)
let flavor : Flavor.t =
  { rosewater = rgb (203, 75, 22) (* Solarized orange *)
  ; flamingo = rgb (220, 50, 47) (* Solarized red *)
  ; pink = rgb (211, 54, 130) (* Solarized magenta *)
  ; mauve = rgb (108, 113, 196) (* Solarized violet *)
  ; red = rgb (220, 50, 47) (* Solarized red *)
  ; maroon = rgb (220, 50, 47) (* Solarized red *)
  ; peach = rgb (203, 75, 22) (* Solarized orange *)
  ; yellow = rgb (181, 137, 0) (* Solarized yellow *)
  ; green = rgb (133, 153, 0) (* Solarized green *)
  ; teal = rgb (42, 161, 152) (* Solarized cyan *)
  ; sky = rgb (38, 139, 210) (* Solarized blue *)
  ; sapphire = rgb (38, 139, 210) (* Solarized blue *)
  ; blue = rgb (38, 139, 210) (* Solarized blue *)
  ; lavender = rgb (108, 113, 196) (* Solarized violet *)
  ; text = rgb (101, 123, 131) (* Solarized base00 *)
  ; subtext1 = rgb (88, 110, 117) (* Solarized base01 *)
  ; subtext0 = rgb (131, 148, 150) (* Solarized base0 *)
  ; overlay2 = rgb (147, 161, 161) (* Solarized base1 *)
  ; overlay1 = rgb (238, 232, 213) (* Solarized base2 *)
  ; overlay0 = rgb (238, 232, 213) (* Solarized base2 *)
  ; surface2 = rgb (200, 206, 160) (* Solarized base2 *)
  ; surface1 = rgb (220, 206, 180) (* Solarized base3 *)
  ; surface0 = rgb (220, 206, 180) (* Solarized base3 *)
  ; base = rgb (253, 246, 227) (* Solarized base3 - background *)
  ; mantle = rgb (248, 241, 222) (* Slightly darker *)
  ; crust = rgb (243, 236, 217)
  ; is_dark = false
  }
;;
