open! Core
open Bonsai_term_color_scheme_catppuccin

(* Dracula theme color mappings *)
let flavor : Flavor.t =
  { rosewater = rgb (255, 184, 108) (* Orange *)
  ; flamingo = rgb (255, 121, 198) (* Pink *)
  ; pink = rgb (255, 121, 198) (* Dracula pink *)
  ; mauve = rgb (189, 147, 249) (* Dracula purple *)
  ; red = rgb (255, 85, 85) (* Dracula red *)
  ; maroon = rgb (255, 85, 85) (* Dracula red *)
  ; peach = rgb (255, 184, 108) (* Dracula orange *)
  ; yellow = rgb (241, 250, 140) (* Dracula yellow *)
  ; green = rgb (80, 250, 123) (* Dracula green *)
  ; teal = rgb (139, 233, 253) (* Dracula cyan *)
  ; sky = rgb (139, 233, 253) (* Dracula cyan *)
  ; sapphire = rgb (139, 233, 253) (* Dracula cyan *)
  ; blue = rgb (139, 233, 253) (* Dracula cyan *)
  ; lavender = rgb (189, 147, 249) (* Dracula purple *)
  ; text = rgb (248, 248, 242) (* Dracula foreground *)
  ; subtext1 = rgb (230, 230, 220) (* Slightly dimmed *)
  ; subtext0 = rgb (200, 200, 190) (* More dimmed *)
  ; overlay2 = rgb (98, 114, 164) (* Dracula comment *)
  ; overlay1 = rgb (88, 104, 154) (* Darker comment *)
  ; overlay0 = rgb (78, 94, 144) (* Even darker *)
  ; surface2 = rgb (68, 71, 90) (* Dracula current line *)
  ; surface1 = rgb (58, 60, 78) (* Darker surface *)
  ; surface0 = rgb (48, 49, 66) (* Even darker surface *)
  ; base = rgb (40, 42, 54) (* Dracula background *)
  ; mantle = rgb (35, 37, 49) (* Darker background *)
  ; crust = rgb (30, 31, 41)
  ; is_dark = true
  }
;;
