open! Core
open Bonsai_term
open Bonsai.Let_syntax

let component (local_ graph) =
  let%arr flavor = Bonsai_term_color_scheme.flavor graph in
  let crust = Bonsai_term_color_scheme.color ~flavor Crust in
  let text = Bonsai_term_color_scheme.color ~flavor Text in
  fun ?(attrs = []) string ->
    View.text ~attrs:([ Attr.bg crust; Attr.fg text ] @ attrs) string
;;
