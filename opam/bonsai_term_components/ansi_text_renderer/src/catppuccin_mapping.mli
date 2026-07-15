(** Converts ANSI color codes (from bat output, etc.) to Catppuccin theme colors. Maps the
    standard 16 ANSI colors to their closest equivalents in the active color scheme
    flavor. *)

open! Core
open Bonsai_term

module Color_kind : sig
  type t =
    | Fg
    | Bg
end

val convert_ansi_color_to_catppuccin
  :  color_kind:Color_kind.t
  -> color:Ansi_text.Color.t
  -> flavor:Bonsai_term_color_scheme.Flavor.t
  -> bg:Bonsai_term_color_scheme.t option
  -> Attr.Color.t
