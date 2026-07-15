open! Core
open Bonsai_term

type 'a t =
  | Leaf of 'a
  | Branch of 'a * 'a t list
  | Split of 'a t list

(** Composes a tree of views into a single view *)
val render : ?layout_attrs:Attr.t list -> View.t t -> View.t

(** Converts an expectree tree into a [Bonsai_term_tree_view.t] tree *)
val of_expectree : Expectree.t -> string t

(** Like [of_expectree] but it will convert the strings into [View.t] by using
    [View.text ?attrs] on all the text nodes. *)
val of_expectree_with_conv : ?attrs:Attr.t list -> Expectree.t -> View.t t
