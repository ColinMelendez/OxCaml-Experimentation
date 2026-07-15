open! Core
open Bonsai_term_color_scheme_catppuccin

let flavor : Flavor.t =
  { rosewater = rgb (255, 192, 203) (* Light pink *)
  ; flamingo = rgb (255, 182, 193) (* Pink *)
  ; pink = rgb (255, 105, 180) (* Hot pink *)
  ; mauve = rgb (175, 82, 222) (* Purple *)
  ; red = rgb (205, 49, 49) (* VS Code error red *)
  ; maroon = rgb (165, 42, 42) (* Dark red *)
  ; peach = rgb (255, 140, 0) (* Orange *)
  ; yellow = rgb (121, 94, 38) (* VS Code light yellow/brown *)
  ; green = rgb (9, 134, 88) (* VS Code light green *)
  ; teal = rgb (0, 139, 139) (* Dark cyan *)
  ; sky = rgb (0, 100, 148) (* VS Code light blue *)
  ; sapphire = rgb (0, 0, 255) (* Blue *)
  ; blue = rgb (0, 0, 255) (* VS Code light blue *)
  ; lavender = rgb (175, 82, 222) (* Purple *)
  ; text = rgb (0, 0, 0) (* VS Code light foreground *)
  ; subtext1 = rgb (51, 51, 51) (* Slightly dimmed text *)
  ; subtext0 = rgb (102, 102, 102) (* Dimmed text *)
  ; overlay2 = rgb (128, 128, 128) (* Comments *)
  ; overlay1 = rgb (153, 153, 153) (* Lighter gray *)
  ; overlay0 = rgb (179, 179, 179) (* Even lighter gray *)
  ; surface2 = rgb (204, 204, 204) (* Light gray *)
  ; surface1 = rgb (230, 230, 230) (* VS Code light sidebar *)
  ; surface0 = rgb (240, 240, 240) (* VS Code light panel *)
  ; base = rgb (255, 255, 255) (* VS Code light editor background *)
  ; mantle = rgb (248, 248, 248) (* Lighter background *)
  ; crust = rgb (245, 245, 245)
  ; is_dark = false
  }
;;
