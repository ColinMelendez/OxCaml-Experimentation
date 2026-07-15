open! Core
open Bonsai_term_color_scheme_catppuccin

(* Solarized Dark theme color mappings *)
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
  ; text = rgb (131, 148, 150) (* Solarized base0 *)
  ; subtext1 = rgb (147, 161, 161) (* Solarized base1 *)
  ; subtext0 = rgb (101, 123, 131) (* Solarized base00 *)
  ; overlay2 = rgb (88, 110, 117) (* Solarized base01 *)
  ; overlay1 = rgb (7, 54, 66) (* Solarized base02 *)
  ; overlay0 = rgb (7, 54, 66) (* Solarized base02 *)
  ; surface2 = rgb (7, 54, 66) (* Solarized base02 *)
  ; surface1 = rgb (20, 63, 74) (* Solarized base03 *)
  ; surface0 = rgb (0, 43, 54) (* Solarized base03 *)
  ; base = rgb (0, 43, 54) (* Solarized base03 - background *)
  ; mantle = rgb (0, 38, 48) (* Slightly darker *)
  ; crust = rgb (0, 33, 42)
  ; is_dark = true
  }
;;
