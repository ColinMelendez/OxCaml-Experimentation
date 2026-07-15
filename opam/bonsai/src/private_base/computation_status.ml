open! Core

type 'input t =
  | Active of 'input
  | Inactive
[@@deriving sexp_of]

let of_option = function
  | Some x -> Active x
  | None -> Inactive
;;

let bind t ~f =
  match t with
  | Active x -> f x
  | Inactive -> Inactive
;;

let map t ~f =
  match t with
  | Active x -> Active (f x)
  | Inactive -> Inactive
;;
