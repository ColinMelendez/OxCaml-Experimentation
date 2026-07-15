open! Core
open Bonsai_term
(* This module is an NCDU-like UI for a generic weighted tree. It visualizes a tree where
   each node in the tree has its own "weight".

   Here is an example visualization of this component:

   {v
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │  Tree of life   j Down  k Up  Enter go in  Backspace go back                   │
    │                                                                                │
    │  >  100 [####################] Formicidae                                      │
    │      22 [####                ] Canidae                                         │
    │      21 [####                ] Rodents                                         │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │  South America (186 total)  -> Animals (143)                                   │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
   v}

   The component has a "top bar", a scrollable/selectable list visualizing a tree node
   level and a "bottom bar" with a "navbar"-like navigation/summary. *)

(** TODOs (If you - the reader - need something that does not exist at the moment, feel
    free to add it here!)

    - Right now this library is very opinionated. It provides an entire "TUI" without much
      customization controls. It would be nice if this were embeddable as a component in
      another/followed the config pattern.
    - Add a "diff" mode, e.g. for diffing the weights of two different trees (e.g. in the
      binary size analysis case to diff the binary size attribution of the same binary at
      two different revisions.) *)

module Weight : sig
  type 'a t =
    { self : 'a (** self weight is the weight of just this node in the tree. *)
    ; dominated : 'a
    (** [dominated] is the size of this node + the size of its children. *)
    }
  [@@deriving sexp_of]
end

module type Key = sig
  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  val to_string_hum : t -> string
  val catppuccin_color : t -> Bonsai_term_color_scheme.t option
end

module type Size = sig
  type t [@@deriving compare, equal]

  include Core.Container.Summable with type t := t

  val to_string : t -> string
  val to_float : t -> float
end

module Make (Key : Key) (Size : Size) : sig
  module Tree_node : sig
    type t =
      { name : Key.t
      ; weight : Size.t Weight.t
      ; children : t Key.Map.t
      }
  end

  (** [component] will produce a component you can use to visualize an interactive
      weighted tree.

      [app_title] is the title that is shown in the top bar. (e.g. the name of your
      app/tool).

      [tree_name] is the name that is shown in the bottom bar. (e.g. if your app tool runs
      on a specific file the name of the file).

      [dimensions] is the amount of terminal dimensions that the component will use.

      [separator] ( defaults to " -> " ). There is a bottom bar with the "current path in
      the tree". (e.g. "a -> b -> c -> d"), if you pass "/", then the path will get
      rendered as "a/b/c/d".

      [on_leaf_select] is an optional callback fired when the user presses Enter on a leaf
      node (one with no children). The callback receives the path from the leaf to the
      root as a [Nonempty_list.t]. When not provided, pressing Enter on a leaf is a no-op.

      [total_size] is the weight of the "entire tree" shown in the bottom bar.

      [nodes] represents the data in your tree. *)
  val component
    :  app_title:string
    -> tree_name:string
    -> dimensions:Dimensions.t Bonsai.t
    -> ?separator:string
    -> ?on_leaf_select:(Key.t Nonempty_list.t -> unit Effect.t) Bonsai.t
    -> total_size:Size.t Bonsai.t
    -> nodes:Tree_node.t Key.Map.t Bonsai.t
    -> local_ Bonsai.graph
    -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
end
