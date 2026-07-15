open! Core
open Bonsai_term_color_scheme_catppuccin

(* Bluloco theme color mappings based on provided RGB values *)
let flavor : Flavor.t =
  { rosewater = rgb (223, 99, 28) (* Markup Attribute - orange *)
  ; flamingo = rgb (213, 39, 83) (* Class/Type/Interface - red-pink *)
  ; pink = rgb (206, 51, 192) (* Number - magenta *)
  ; mauve = rgb (130, 63, 241) (* Constant - purple *)
  ; red = rgb (213, 39, 83) (* Class/Type/Interface *)
  ; maroon = rgb (160, 90, 72) (* Property - brown *)
  ; peach = rgb (223, 99, 28) (* Markup Attribute - orange *)
  ; yellow = rgb (197, 163, 50) (* String - yellow *)
  ; green = rgb (35, 151, 74) (* Function/Method - green *)
  ; teal = rgb (0, 152, 221) (* Keyword - cyan-blue *)
  ; sky = rgb (39, 95, 228) (* Markup Tag - blue *)
  ; sapphire = rgb (39, 95, 228) (* Markup Tag - blue *)
  ; blue = rgb (0, 152, 221) (* Keyword - cyan-blue *)
  ; lavender = rgb (122, 130, 218) (* Operator/Punctuation - light purple *)
  ; text = rgb (56, 58, 66) (* Foreground *)
  ; subtext1 = rgb (80, 82, 90) (* Slightly dimmed text *)
  ; subtext0 = rgb (120, 121, 127) (* More dimmed text *)
  ; overlay2 = rgb (160, 161, 167) (* Comment *)
  ; overlay1 = rgb (180, 181, 187) (* Lighter gray *)
  ; overlay0 = rgb (200, 201, 207) (* Even lighter gray *)
  ; surface2 = rgb (220, 220, 224) (* Light gray *)
  ; surface1 = rgb (223, 225, 229) (* Very light gray *)
  ; surface0 = rgb (235, 234, 238) (* Almost white *)
  ; base = rgb (249, 249, 249) (* Background *)
  ; mantle = rgb (245, 245, 245) (* Slightly darker background *)
  ; crust = rgb (241, 241, 241)
  ; is_dark = false
  }
;;
