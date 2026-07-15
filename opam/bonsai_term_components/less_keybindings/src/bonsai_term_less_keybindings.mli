open! Core
open Bonsai_term

module Action : sig
  type t =
    | Up
    | Down
    | Top
    | Bottom
    | Up_half_screen
    | Down_half_screen
  [@@deriving sexp_of]
end

(* [component] will provide some common bonsai_term events and map them to more ergonomic
   [Action.t]'s.

   The supported keybindings are:

   [j/down/ctrl+e/scroll-down] -> Down [k/up/ctrl+y/scroll-up] -> Up [g] -> Top [G] ->
   Bottom [u/ctrl+u/pageup] -> Up_half_screen [d/ctrl+d/pagedown] -> Down_half_screen.
*)
val component
  :  (Action.t -> unit Effect.t) Bonsai.t
  -> local_ Bonsai.graph
  -> (Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t
