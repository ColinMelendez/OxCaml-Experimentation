open! Core
open Bonsai_term

module Fill_width : sig
  (** [Fill_width.t] determines if [render] should add extra right-padding if a line is
      less wide than [fill_width]. This makes it possible for background color to fill a
      whole line even when content does not. *)
  type t =
    | No
    | Yes of { fill_width : int }
end

val render
  :  ?bg:Bonsai_term_color_scheme.t
  -> ?flavor:Bonsai_term_color_scheme.Flavor.t
  -> ?fill_width:Fill_width.t
  -> Ansi_text.t
  -> View.t
