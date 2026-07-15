open! Core
open Bonsai_term

type 'a t =
  | Leaf of 'a
  | Branch of 'a * 'a t list
  | Split of 'a t list

let rec map t ~f =
  match t with
  | Leaf a -> Leaf (f a)
  | Branch (a, c) -> Branch (f a, List.map c ~f:(map ~f))
  | Split c -> Split (List.map c ~f:(map ~f))
;;

let placeholder_text = "a"

let view_to_string_with_same_height view =
  String.concat ~sep:"\n" (List.init (View.height view) ~f:(fun _ -> placeholder_text))
;;

let rec to_expectree = function
  | Leaf view -> Expectree.Leaf (view_to_string_with_same_height view)
  | Branch (view, children) ->
    Expectree.Branch
      (view_to_string_with_same_height view, List.map children ~f:to_expectree)
  | Split children -> Split (List.map children ~f:to_expectree)
;;

let rec of_expectree = function
  | Expectree.Leaf text -> Leaf text
  | Expectree.Branch (text, children) -> Branch (text, List.map children ~f:of_expectree)
  | Expectree.Split children -> Split (List.map children ~f:of_expectree)
;;

let of_expectree_with_conv ?(attrs = []) =
  let multiline_string_to_text s =
    String.split ~on:'\n' s
    |> List.map ~f:(fun line -> View.text ~attrs line)
    |> View.vcat
  in
  fun expectree -> map (of_expectree expectree) ~f:multiline_string_to_text
;;

let render ?(layout_attrs = []) t =
  let take_and_apply lines n =
    (* Chop the first n lines out of the expectree output *)
    let #(first_n, rest) = List.split_n lines n in
    let layout =
      (* For each line of text in this chunk of expectree output, create a text view and
         vcat them all together. *)
      List.map first_n ~f:(fun text ->
        text
        |> String.chop_suffix_if_exists ~suffix:placeholder_text
        |> View.text ~attrs:layout_attrs)
      |> View.vcat
    in
    layout, rest
  in
  let rec loop lines = function
    | Leaf view ->
      let layout, lines = take_and_apply lines (View.height view) in
      View.hcat [ layout; view ], lines
    | Branch (view, children) ->
      let layout, lines = take_and_apply lines (View.height view) in
      let branch_view = View.hcat [ layout; view ] in
      let lines, child_views =
        List.fold_map ~init:lines children ~f:(fun lines child ->
          let child_view, lines = loop lines child in
          lines, child_view)
      in
      View.vcat (branch_view :: child_views), lines
    | Split children ->
      let lines, child_views =
        List.fold_map ~init:lines children ~f:(fun lines child ->
          let child_view, lines = loop lines child in
          lines, child_view)
      in
      View.vcat child_views, lines
  in
  (* First, render the expectree with dummy text inside all of the nodes (the dummy text
     must have the same height as the input view) *)
  let as_string = Expectree.to_string (to_expectree t) in
  let view, _ = loop (String.split ~on:'\n' as_string) t in
  view
;;
