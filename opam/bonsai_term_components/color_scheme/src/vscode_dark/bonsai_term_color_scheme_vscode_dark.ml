open! Core
open Bonsai_term_color_scheme_catppuccin

let flavor : Flavor.t =
  { rosewater = rgb (255, 192, 203) (* Light pink *)
  ; flamingo = rgb (255, 182, 193) (* Pink *)
  ; pink = rgb (255, 105, 180) (* Hot pink *)
  ; mauve = rgb (197, 134, 192) (* Purple-pink *)
  ; red = rgb (244, 71, 71) (* VS Code error red *)
  ; maroon = rgb (205, 49, 49) (* Dark red *)
  ; peach = rgb (255, 159, 64) (* Orange *)
  ; yellow = rgb (220, 220, 170) (* VS Code yellow *)
  ; green = rgb (181, 206, 168) (* VS Code green *)
  ; teal = rgb (78, 201, 176) (* VS Code cyan *)
  ; sky = rgb (156, 220, 254) (* VS Code light blue *)
  ; sapphire = rgb (86, 156, 214) (* VS Code blue *)
  ; blue = rgb (86, 156, 214) (* VS Code blue *)
  ; lavender = rgb (206, 145, 120) (* VS Code brown/tan *)
  ; text = rgb (212, 212, 212) (* VS Code foreground *)
  ; subtext1 = rgb (187, 187, 187) (* Slightly dimmed text *)
  ; subtext0 = rgb (153, 153, 153) (* Dimmed text *)
  ; overlay2 = rgb (128, 128, 128) (* Comments *)
  ; overlay1 = rgb (102, 102, 102) (* Darker gray *)
  ; overlay0 = rgb (76, 76, 76) (* Even darker gray *)
  ; surface2 = rgb (60, 60, 60) (* VS Code activity bar *)
  ; surface1 = rgb (37, 37, 38) (* VS Code sidebar *)
  ; surface0 = rgb (45, 45, 45) (* VS Code panel *)
  ; base = rgb (30, 30, 30) (* VS Code editor background *)
  ; mantle = rgb (25, 25, 26) (* Darker background *)
  ; crust = rgb (20, 20, 20)
  ; is_dark = true
  }
;;
