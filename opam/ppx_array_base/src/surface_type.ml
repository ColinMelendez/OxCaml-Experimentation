open! Ppxlib
open! Stdppx
open Ast_builder.Default

type t =
  | Array
  | Iarray

let array = Array
let iarray = Iarray
let all = [ array; iarray ]

let to_string = function
  | Array -> "array"
  | Iarray -> "iarray"
;;

let extension_prefix = to_string

let to_core_type t context ~loc ty =
  ptyp_constr ~loc (Located.mk ~loc (Context.runtime_ident context (to_string t))) [ ty ]
;;

let wrap t context ~loc expr =
  match t with
  | Array -> expr
  | Iarray ->
    [%expr
      [%e Context.runtime_fun context loc "unsafe_of_array__promise_no_mutation"]
        [%e expr]]
;;

let unwrap_in t context ~loc ident expr =
  match t with
  | Array -> expr
  | Iarray ->
    [%expr
      let [%p ppat_var ~loc (Located.mk ~loc ident)] =
        [%e Context.runtime_fun context loc "unsafe_to_array__promise_no_mutation"]
          [%e pexp_ident ~loc (Located.mk ~loc (Lident ident))]
      in
      [%e expr]]
;;
