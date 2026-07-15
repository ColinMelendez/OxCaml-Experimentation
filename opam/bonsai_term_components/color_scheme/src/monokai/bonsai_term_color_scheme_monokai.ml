open! Core
open Bonsai_term_color_scheme_catppuccin

(* Monokai theme color mappings *)
let flavor : Flavor.t =
  { rosewater = rgb (253, 151, 31) (* Orange *)
  ; flamingo = rgb (249, 38, 114) (* Pink *)
  ; pink = rgb (249, 38, 114) (* Monokai pink *)
  ; mauve = rgb (174, 129, 255) (* Purple *)
  ; red = rgb (249, 38, 114) (* Monokai red/pink *)
  ; maroon = rgb (249, 38, 114) (* Monokai red/pink *)
  ; peach = rgb (253, 151, 31) (* Monokai orange *)
  ; yellow = rgb (230, 219, 116) (* Monokai yellow *)
  ; green = rgb (166, 226, 46) (* Monokai green *)
  ; teal = rgb (102, 217, 239) (* Monokai cyan *)
  ; sky = rgb (102, 217, 239) (* Monokai cyan *)
  ; sapphire = rgb (102, 217, 239) (* Monokai cyan *)
  ; blue = rgb (102, 217, 239) (* Monokai cyan *)
  ; lavender = rgb (174, 129, 255) (* Monokai purple *)
  ; text = rgb (248, 248, 242) (* Monokai foreground *)
  ; subtext1 = rgb (230, 230, 220) (* Slightly dimmed *)
  ; subtext0 = rgb (200, 200, 190) (* More dimmed *)
  ; overlay2 = rgb (117, 113, 94) (* Monokai comment *)
  ; overlay1 = rgb (102, 99, 83) (* Darker overlay *)
  ; overlay0 = rgb (88, 85, 72) (* Even darker overlay *)
  ; surface2 = rgb (73, 72, 62) (* Monokai line highlight *)
  ; surface1 = rgb (58, 58, 50) (* Darker surface *)
  ; surface0 = rgb (49, 49, 42) (* Even darker surface *)
  ; base = rgb (39, 40, 34) (* Monokai background *)
  ; mantle = rgb (34, 35, 30) (* Darker background *)
  ; crust = rgb (30, 30, 26) (* Darkest background *)
  ; is_dark = true
  }
;;
