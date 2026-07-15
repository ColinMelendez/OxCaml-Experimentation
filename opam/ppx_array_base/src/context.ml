open! Ppxlib
open Ast_builder.Default

type t =
  | Base
  | Deriving of core_type

let runtime_ident t label =
  let longident =
    match t with
    | Base -> label
    | Deriving _ -> "Ppx_array_runtime." ^ label
  in
  Longident.parse longident
;;

let runtime_fun t loc label = pexp_ident ~loc (Loc.make ~loc (runtime_ident t label))

let how_to_vary_kinds t ~input ~output ~output_separable : How_to_vary_kinds.t =
  let input : _ How_to_vary_kinds.Whether_to_vary.t =
    match t with
    | Base -> Vary { kinds = input }
    | Deriving input_type -> Do_not_vary input_type
  in
  let output : _ How_to_vary_kinds.Whether_to_vary.t * bool =
    ( (match output with
       | None -> Do_not_vary ()
       | Some output -> Vary { kinds = output })
    , output_separable )
  in
  { input; output }
;;
