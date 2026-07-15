open! Core
open Bonsai_term_color_scheme_catppuccin

(* Gruvbox Dark theme color mappings *)
let flavor : Flavor.t =
  { rosewater = rgb (211, 134, 155) (* Light red *)
  ; flamingo = rgb (204, 36, 29) (* Red *)
  ; pink = rgb (211, 134, 155) (* Light red *)
  ; mauve = rgb (177, 98, 134) (* Purple *)
  ; red = rgb (251, 73, 52) (* Gruvbox bright red *)
  ; maroon = rgb (204, 36, 29) (* Gruvbox red *)
  ; peach = rgb (254, 128, 25) (* Gruvbox bright orange *)
  ; yellow = rgb (250, 189, 47) (* Gruvbox bright yellow *)
  ; green = rgb (184, 187, 38) (* Gruvbox bright green *)
  ; teal = rgb (142, 192, 124) (* Gruvbox aqua *)
  ; sky = rgb (131, 165, 152) (* Gruvbox bright aqua *)
  ; sapphire = rgb (131, 165, 152) (* Gruvbox bright blue *)
  ; blue = rgb (131, 165, 152) (* Gruvbox bright blue *)
  ; lavender = rgb (211, 134, 155) (* Gruvbox purple *)
  ; text = rgb (235, 219, 178) (* Gruvbox fg *)
  ; subtext1 = rgb (213, 196, 161) (* Gruvbox fg2 *)
  ; subtext0 = rgb (189, 174, 147) (* Gruvbox fg3 *)
  ; overlay2 = rgb (168, 153, 132) (* Gruvbox fg4 *)
  ; overlay1 = rgb (146, 131, 116) (* Gruvbox gray *)
  ; overlay0 = rgb (124, 111, 100) (* Gruvbox bg4 *)
  ; surface2 = rgb (102, 92, 84) (* Gruvbox bg3 *)
  ; surface1 = rgb (80, 73, 69) (* Gruvbox bg2 *)
  ; surface0 = rgb (60, 56, 54) (* Gruvbox bg1 *)
  ; base = rgb (40, 40, 40) (* Gruvbox bg0 *)
  ; mantle = rgb (50, 48, 47) (* Gruvbox bg0_s *)
  ; crust = rgb (29, 32, 33)
  ; is_dark = true
  }
;;
