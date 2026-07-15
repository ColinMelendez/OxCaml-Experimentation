open! Ppxlib
open! Stdppx
open Ast_builder.Default
open Ppx_array_base

let assert_single_unboxed_type loc = function
  | [ type_decl ] ->
    let ({ ptype_name; ptype_params; ptype_kind; ptype_manifest; _ } as type_decl) =
      Common.map_type_decl_variables type_decl
    in
    let shim_kind = Ppxlib_jane.Shim.Type_kind.of_parsetree ptype_kind in
    let shim_type =
      Option.map ptype_manifest ~f:(fun type_ ->
        Ppxlib_jane.Shim.Core_type.of_parsetree type_)
    in
    let ptype_params = List.map ~f:(fun (type_, _) -> type_) ptype_params in
    let type_ =
      match shim_kind, shim_type with
      | Ptype_record_unboxed_product _, _
      | _, Some { ptyp_desc = Ptyp_unboxed_tuple _; _ } ->
        ptyp_constr ~loc (Loc.make ~loc (Lident ptype_name.txt)) ptype_params
      | Ptype_record _, _ ->
        (* t# is the implicit unboxed record derived from a record type declaration. *)
        ptyp_constr ~loc (Loc.make ~loc (Lident (ptype_name.txt ^ "#"))) ptype_params
      | _ -> Common.raise_unsupported loc ~why:"type must be a record or unboxed tuple"
    in
    type_, type_decl
  | _ -> Common.raise_unsupported loc ~why:"[type ... and ...] is not supported"
;;

let assert_at_least_one_function_included loc = function
  | [] -> Common.raise_unsupported loc ~why:"deriver must include at least one function"
  | _ -> ()
;;

let validate_and_return_single_type loc type_declaration args =
  assert_at_least_one_function_included loc args;
  assert_single_unboxed_type loc type_declaration
;;

let array_submodule loc surface_type ~type_name =
  let module_root =
    String.capitalize_ascii (Surface_type.extension_prefix surface_type)
  in
  let submodule =
    (match Ppx_helpers.demangle_template type_name with
     | "t", mangle -> [ module_root; mangle ]
     | type_name, mangle -> [ module_root; "_"; type_name; mangle ])
    |> String.concat ~sep:""
  in
  Loc.make ~loc (Some submodule)
;;

let array_submodule_type_decl loc surface_type ~params ~cstrs ~type_ =
  type_declaration
    ~loc
    ~name:(Loc.make ~loc "t")
    ~params
    ~cstrs
    ~kind:Ptype_abstract
    ~private_:Public
    ~manifest:(Some (Surface_type.to_core_type surface_type (Deriving type_) ~loc type_))
;;

let generator surface_type ~extensions ~wrap_in_array_submodule =
  let args =
    Deriving.Args.(
      empty
      +> Function.For_deriving.flag Function.map
      +> Function.For_deriving.flag Function.mapi
      +> Function.For_deriving.flag Function.iter
      +> Function.For_deriving.flag Function.iteri
      +> Function.For_deriving.flag Function.fold)
  in
  Deriving.Generator.make
    args
    (fun
        ~loc
        ~path:(_ : label)
        ((_ : rec_flag), type_declaration)
        incl_map
        incl_mapi
        incl_iter
        incl_iteri
        incl_fold
      ->
       let args =
         List.filter_opt [ incl_map; incl_mapi; incl_iter; incl_iteri; incl_fold ]
       in
       let type_, original_type_decl =
         validate_and_return_single_type loc type_declaration args
       in
       let type_decl =
         array_submodule_type_decl
           loc
           surface_type
           ~params:original_type_decl.ptype_params
           ~cstrs:original_type_decl.ptype_cstrs
           ~type_
       in
       let name =
         array_submodule loc surface_type ~type_name:original_type_decl.ptype_name.txt
       in
       args
       |> List.concat_map ~f:(fun function_ ->
         extensions function_ surface_type loc type_)
       |> wrap_in_array_submodule loc ~type_decl ~name)
;;

(* deriving *)
let () =
  List.iter Surface_type.all ~f:(fun surface_type ->
    let implementation =
      generator
        surface_type
        ~extensions:Function.For_deriving.structure_extensions
        ~wrap_in_array_submodule:(fun loc ~type_decl ~name items ->
          [ (let module_expr =
               (* We need to put items before a type declaration because if the type is an
                  implicit unboxed, we cannot shadow [t] and then refer to the implicit
                  unboxed [t#].

                  It also makes the code here a lot messier if we try to do something like
                  {[
                    type nonrec implicit_unboxed = t#
                    and t = implicit_unboxed array
                  ]}
               *)
               pmod_structure ~loc (items @ [ pstr_type ~loc Nonrecursive [ type_decl ] ])
             in
             let module_binding = module_binding ~loc ~name ~expr:module_expr in
             pstr_module ~loc module_binding)
          ])
    in
    let interface =
      generator
        surface_type
        ~extensions:Function.For_deriving.signature_extensions
        ~wrap_in_array_submodule:(fun loc ~type_decl ~name items ->
          [ (let module_type =
               pmty_signature ~loc (items @ [ psig_type ~loc Nonrecursive [ type_decl ] ])
             in
             let module_declaration = module_declaration ~loc ~name ~type_:module_type in
             psig_module ~loc module_declaration)
          ])
    in
    Deriving.add
      (Surface_type.extension_prefix surface_type)
      ~str_type_decl:implementation
      ~sig_type_decl:interface
    |> Deriving.ignore)
;;

(* register extensions *)
let () =
  List.iter Surface_type.all ~f:(fun surface_type ->
    let rules =
      List.concat_map
        ~f:(fun function_ -> Function.extensions function_ surface_type)
        Function.all
    in
    Driver.register_transformation (Surface_type.extension_prefix surface_type) ~rules)
;;
