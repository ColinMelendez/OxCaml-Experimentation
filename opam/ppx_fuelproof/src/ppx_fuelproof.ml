open Base
open Ppxlib

(*$ open Base *)
(*$*)

let ghostify =
  object
    inherit Ast_traverse.map
    method! location loc = { loc with loc_ghost = true }
  end
;;

let clear_non_builtin_attributes =
  object
    inherit Ast_traverse.map

    method! attributes attributes =
      List.filter attributes ~f:(fun a ->
        match a.attr_name.txt with
        | "atomic"
        | "ocaml.atomic"
        | "warning"
        | "ocaml.warning"
        | "alert"
        | "ocaml.alert"
        | "doc"
        | "ocaml.doc" -> true
        | _ -> false)
  end
;;

module Supported_axis = struct
  module T = struct
    type t =
      | Portable
      | Contended
      | Forkable
      | Unyielding
      | Many
      | Stateless
      | Immutable
      | Non_float
    [@@deriving compare ~localize, sexp_of]
  end

  include Comparable.Make (T)
  include T

  let of_modal_mod : Ppxlib_jane.mode -> t Or_error.t = function
    | Mode "portable" -> Ok Portable
    | Mode "contended" -> Ok Contended
    | Mode "forkable" -> Ok Forkable
    | Mode "unyielding" -> Ok Unyielding
    | Mode "many" -> Ok Many
    | Mode "stateless" -> Ok Stateless
    | Mode "immutable" -> Ok Immutable
    | Mode "non_float" -> Ok Non_float
    | Mode axis -> Or_error.errorf "Modifier not supported by fuelproof: %s" axis
  ;;

  let to_axis = function
    | Portable -> "portable"
    | Contended -> "contended"
    | Many -> "many"
    | Forkable -> "forkable"
    | Unyielding -> "unyielding"
    | Stateless -> "stateless"
    | Immutable -> "immutable"
    | Non_float -> "non_float"
  ;;

  let is_modal = function
    | Non_float -> false
    | _ -> true
  ;;
end

(* In the event that [immutable_data] or [mutable_data] grow new modes, add then here. The
   below tests ensure we aren't doing something unsound in the meantime, by checking (via
   a subkind check /in both directions/) that the set of axes we have here is exactly the
   set of axes in [immutable_data]. *)
(*$
  let immutable_data =
    [ "contended"
    ; "immutable"
    ; "many"
    ; "non_float"
    ; "portable"
    ; "stateless"
    ; "forkable"
    ; "unyielding"
    ]
  ;;

  let mutable_data =
    [ "many"; "non_float"; "portable"; "stateless"; "forkable"; "unyielding" ]
  ;;

  let sync_data =
    [ "contended"
    ; "many"
    ; "non_float"
    ; "portable"
    ; "stateless"
    ; "forkable"
    ; "unyielding"
    ]
  ;;
*)
(*$
  let print_list_and_test data ~name =
    Stdio.printf "let %s : Supported_axis.t list = [ " name;
    List.iter data ~f:(fun mod_ -> Stdio.printf {|%s; |} (String.capitalize mod_));
    Stdio.print_endline "];;";
    Stdio.printf
      {|
      module _ : sig
        type _t1 : %s
        type _t2 : value mod %s
      end = struct
        type _t1 : value mod %s
        type _t2 : %s
      end
    |}
      name
      (String.concat data ~sep:" ")
      (String.concat data ~sep:" ")
      name
  ;;

  let () = print_list_and_test immutable_data ~name:"immutable_data"
  let () = print_list_and_test mutable_data ~name:"mutable_data"
  let () = print_list_and_test sync_data ~name:"sync_data"
*)
let immutable_data : Supported_axis.t list =
  [ Contended; Immutable; Many; Non_float; Portable; Stateless; Forkable; Unyielding ]
;;

module _ : sig
  type _t1 : immutable_data

  type _t2 :
    value mod contended forkable immutable many non_float portable stateless unyielding
end = struct
  type _t1 :
    value mod contended forkable immutable many non_float portable stateless unyielding

  type _t2 : immutable_data
end

let mutable_data : Supported_axis.t list =
  [ Many; Non_float; Portable; Stateless; Forkable; Unyielding ]
;;

module _ : sig
  type _t1 : mutable_data
  type _t2 : value mod forkable many non_float portable stateless unyielding
end = struct
  type _t1 : value mod forkable many non_float portable stateless unyielding
  type _t2 : mutable_data
end

let sync_data : Supported_axis.t list =
  [ Contended; Many; Non_float; Portable; Stateless; Forkable; Unyielding ]
;;

module _ : sig
  type _t1 : sync_data
  type _t2 : value mod contended forkable many non_float portable stateless unyielding
end = struct
  type _t1 : value mod contended forkable many non_float portable stateless unyielding
  type _t2 : sync_data
end
(*$*)

let immutable_data ~loc = immutable_data |> List.map ~f:(fun txt -> { txt; loc })
let mutable_data ~loc = mutable_data |> List.map ~f:(fun txt -> { txt; loc })
let sync_data ~loc = sync_data |> List.map ~f:(fun txt -> { txt; loc })

let axes_to_ignore modalities =
  List.filter_map
    modalities
    ~f:
      (fun
        { txt = Ppxlib_jane.Shim.Modality.Modality field_modality; _ }
        : Supported_axis.t option
      ->
      match field_modality with
      | "portable" -> Some Portable
      | "contended" -> Some Contended
      | "many" -> Some Many
      | "forkable" -> Some Forkable
      | "unyielding" -> Some Unyielding
      | _ -> None)
  |> Set.of_list (module Supported_axis)
;;

let crossing_axes_is_implied_by_immutable_data axes =
  List.for_all axes ~f:(fun { txt = m; loc = _ } ->
    match (m : Supported_axis.t) with
    | Portable | Contended | Forkable | Unyielding | Many | Stateless | Immutable -> true
    | Non_float -> false)
;;

let with_unsafe_allow_any_mode_crossing attributes ~loc =
  attributes
  @ [ { attr_loc = loc
      ; attr_name = { loc; txt = "unsafe_allow_any_mode_crossing" }
      ; attr_payload = PStr []
      }
    ]
;;

let has_unsafe_allow_any_mode_crossing attributes =
  List.exists attributes ~f:(fun attr ->
    String.equal attr.attr_name.txt "unsafe_allow_any_mode_crossing")
;;

let type_with_builtin_cross_checking ty ~axes_to_cross ~axes_to_ignore =
  let axes_to_check_crossing =
    List.filter axes_to_cross ~f:(fun { txt = axis_to_cross; _ } ->
      not (Set.mem axes_to_ignore axis_to_cross))
  in
  let loc = { ty.ptyp_loc with loc_ghost = true } in
  match axes_to_check_crossing with
  | [] -> ty
  | _ :: _ ->
    let wrap_ty_in_check ty =
      Ppxlib_jane.Ast_builder.Default.ptyp_alias
        ty
        None
        (Some
           { pjka_desc =
               Pjk_mod
                 ( { pjka_desc = Pjk_abbreviation ({ txt = Lident "any"; loc }, [])
                   ; pjka_loc = loc
                   }
                 , List.map axes_to_check_crossing ~f:(fun axis ->
                     Supported_axis.to_axis axis.txt, axis.loc)
                     (* sort to match ocamlformat output *)
                   |> List.sort ~compare:[%compare__local: string * _]
                   |> List.map ~f:(fun (mod_, loc) ->
                     { txt = Ppxlib_jane.Shim.Mode.Mode mod_
                     ; loc = { loc with loc_ghost = true }
                     }) )
           ; pjka_loc = loc
           })
        ~loc
    in
    (* The following code block sidesteps an issue with ppx_fuelproof's method of ensuring
       mode crossing. The issue presents when the recursive instance of the type being
       defined appears under a type constructor, like [list] or [iarray]. In these cases,
       mode crossing inference is made happier when the wrapping check appears closer to
       the type being defined.

       Engineering effort will be paid toward obviating ppx_fuelproof rather than making
       this check better.
    *)
    let rec wrap_loop ty =
      match ty with
      | [%type: [%t? ty] list]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty] Ppx_fuelproof_runtime.list]
      | [%type: [%t? ty] iarray]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty] Ppx_fuelproof_runtime.Iarray.t]
      | [%type: [%t? ty] Iarray.t]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty] Ppx_fuelproof_runtime.Iarray.t]
      | [%type: [%t? ty1] * [%t? ty2]]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty1] * [%t wrap_loop ty2]]
      | [%type: [%t? ty] option]
        when crossing_axes_is_implied_by_immutable_data axes_to_check_crossing ->
        [%type: [%t wrap_loop ty] Ppx_fuelproof_runtime.option]
      | _ -> wrap_ty_in_check ty
    in
    wrap_loop ty
;;

let rewrite_fields original_fields ~axes_to_cross =
  let open Result.Let_syntax in
  let check_crossing_contention =
    List.exists axes_to_cross ~f:(fun axis ->
      [%compare.equal__local: Supported_axis.t] axis.txt Contended)
  in
  List.map original_fields ~f:(fun field ->
    let modalities, _ = Ppxlib_jane.Shim.Label_declaration.extract_modalities field in
    let axes_to_ignore = axes_to_ignore modalities in
    let%bind () =
      match field.pld_mutable with
      | Mutable ->
        let is_atomic =
          List.exists field.pld_attributes ~f:(fun attr ->
            match attr.attr_name.txt, attr.attr_payload with
            | ("atomic" | "ocaml.atomic"), PStr [] -> true
            | _ -> false)
        in
        if (not is_atomic) && check_crossing_contention
        then
          Error
            ( Error.of_string "Type with a mutable field can't cross contention"
            , field.pld_loc )
        else Ok ()
      | Immutable -> Ok ()
    in
    return
      { field with
        pld_type =
          type_with_builtin_cross_checking field.pld_type ~axes_to_cross ~axes_to_ignore
      })
  |> Result.all
;;

let rewrite_arg arg ~axes_to_cross =
  let modalities, arg_type = Ppxlib_jane.Shim.Pcstr_tuple_arg.extract_modalities arg in
  let axes_to_ignore = axes_to_ignore modalities in
  Ppxlib_jane.Shim.Pcstr_tuple_arg.create
    ~loc:(Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg).ptyp_loc
    ~modalities
    ~type_:(type_with_builtin_cross_checking arg_type ~axes_to_cross ~axes_to_ignore)
;;

let rewrite_constructors original_constructors ~axes_to_cross =
  let open Result.Let_syntax in
  List.map original_constructors ~f:(fun ctor ->
    let%bind args =
      match ctor.pcd_args with
      | Pcstr_record fields ->
        let%bind fields = rewrite_fields fields ~axes_to_cross in
        return (Pcstr_record fields)
      | Pcstr_tuple args ->
        let args = List.map args ~f:(fun arg -> rewrite_arg arg ~axes_to_cross) in
        return (Pcstr_tuple args)
    in
    return { ctor with pcd_args = args })
  |> Result.all
;;

let remove_non_modal axes =
  List.filter axes ~f:(fun loc -> Supported_axis.is_modal loc.txt)
;;

module Rewritten_tydecl = struct
  type t =
    { checked_with_fuelproof : bool
    ; tydecl : type_declaration
    }
end

let rewrite_tydecls (tydecls : type_declaration list) ~loc
  : (Rewritten_tydecl.t list, _) Result.t
  =
  let open Result.Let_syntax in
  let%bind new_tydecls =
    List.map tydecls ~f:(fun t ->
      (* Returning `Unchanged means that we won't attempt to prove anything about that
         type. fuelproof will produce an error if, for all types in the recursive knot,
         it's able to prove nothing.
      *)
      With_return.with_return (fun r ->
        let () =
          if has_unsafe_allow_any_mode_crossing t.ptype_attributes
          then r.return (Ok `Unchanged)
        in
        match Ppxlib_jane.Shim.Type_declaration.extract_jkind_annotation t with
        | None -> r.return (Ok `Unchanged)
        | Some jkind ->
          let%bind axes_to_cross =
            match jkind with
            | { pjka_desc =
                  Pjk_mod
                    ( { pjka_desc = Pjk_abbreviation ({ txt = Lident "value"; _ }, _); _ }
                    , mods )
              ; _
              } ->
              let%bind axes =
                List.map mods ~f:(fun mod_ ->
                  let%map axis =
                    Supported_axis.of_modal_mod mod_.txt
                    |> Result.map_error ~f:(fun err -> err, mod_.loc)
                  in
                  { mod_ with txt = axis })
                |> Result.all
              in
              Ok axes
            | { pjka_desc = Pjk_abbreviation ({ txt = Lident "immutable_data"; _ }, _)
              ; pjka_loc = loc
              } -> Ok (immutable_data ~loc)
            | { pjka_desc = Pjk_abbreviation ({ txt = Lident "mutable_data"; _ }, _)
              ; pjka_loc = loc
              } -> Ok (mutable_data ~loc)
            | { pjka_desc = Pjk_abbreviation ({ txt = Lident "sync_data"; _ }, _)
              ; pjka_loc = loc
              } -> Ok (sync_data ~loc)
            | { pjka_loc = loc; _ } ->
              Error (Error.of_string "Unsupported kind annotation for %fuelproof", loc)
          in
          let axes_to_cross =
            let is_unboxed =
              List.exists t.ptype_attributes ~f:(fun attr ->
                String.equal attr.attr_name.txt "unboxed")
            in
            (* For example, [Non_float]'s axis (separability) is not modal, so applies in
               different circumstances than modal axes. In particular, it is only relevant
               for fuelproof checks on [@@unboxed] types, where the separability of the
               overall type is inherited from the separability of the single field. *)
            if is_unboxed then axes_to_cross else remove_non_modal axes_to_cross
          in
          let%bind new_ptype_kind =
            match t.ptype_kind with
            | Ptype_record fields ->
              let%bind fields = rewrite_fields fields ~axes_to_cross in
              return (Ptype_record fields)
            | Ptype_variant constructors ->
              let%bind constructors = rewrite_constructors constructors ~axes_to_cross in
              return (Ptype_variant constructors)
            | Ptype_abstract -> r.return (Ok `Unchanged)
            | _ ->
              Error
                ( Error.of_string "Can only write %fuelproof on records and variants"
                , t.ptype_loc )
          in
          let loc = { t.ptype_loc with loc_ghost = true } in
          Ok
            (`Changed
              { t with
                ptype_kind = new_ptype_kind
              ; ptype_attributes =
                  with_unsafe_allow_any_mode_crossing t.ptype_attributes ~loc
              })))
    |> Result.all
  in
  if List.for_all new_tydecls ~f:(function
       | `Unchanged -> true
       | `Changed _ -> false)
  then
    Ok
      (List.map tydecls ~f:(fun t ->
         { Rewritten_tydecl.checked_with_fuelproof = false; tydecl = t }))
  else
    List.map2_exn tydecls new_tydecls ~f:(fun old new_ ->
      match new_ with
      | `Unchanged -> { Rewritten_tydecl.checked_with_fuelproof = false; tydecl = old }
      | `Changed new_ -> { Rewritten_tydecl.checked_with_fuelproof = true; tydecl = new_ })
    |> Result.return
;;

let create_extension_str ~loc rec_flag original_tydecls =
  match rewrite_tydecls original_tydecls ~loc with
  | Ok rewritten_tydecls ->
    (* For the original input:

       {[
         type%fuelproof t : value mod portable = { x : int }
       ]}

       We generate something like:

       {[
         module Check = struct
           type t = { x : int as (_ : value mod portable) } [@@unsafe_allow_any_mode_crossing]
         end

         type t = Check.t = { x : int } [@@unsafe_allow_any_mode_crossing]
       ]}

       The [Check] module is used to check the mode-crossing behavior is as expected, but
       doensn't interact well with deriving ppxes, like [@@deriving sexp_of].

       The [type t = Check.t = ...] declaration doesn't add any checking on top of
       [Check], but interacts well with deriving ppxes. This allows users to write
       [type%fuelproof t = ... [@@deriving sexp_of]], for example.
    *)
    let module_name = gen_symbol ~prefix:"Check" () in
    let fuelproof_check_decl =
      let rewritten_tydecls =
        List.map rewritten_tydecls ~f:(fun { checked_with_fuelproof; tydecl } ->
          if not checked_with_fuelproof
          then tydecl
          else (
            let attributes =
              List.filter tydecl.ptype_attributes ~f:(fun attr ->
                String.( <> ) attr.attr_name.txt "deriving")
            in
            (* Clear attributes, which may only be consumed by deriving ppxes. The
               attributes will be present on the [decl_for_deriving_ppxes], so we're not
               dropping information.
            *)
            let cleared_tydecl = clear_non_builtin_attributes#type_declaration tydecl in
            { cleared_tydecl with ptype_attributes = attributes }))
      in
      let original_type = Ast_builder.Default.pstr_type rec_flag rewritten_tydecls ~loc in
      Ast_builder.Default.pstr_module
        ~loc
        (Ast_builder.Default.module_binding
           ~loc
           ~name:(Loc.make (Some module_name) ~loc)
           ~expr:(Ast_builder.Default.pmod_structure ~loc [ original_type ]))
    in
    let decl_for_deriving_ppxes =
      List.map2_exn
        original_tydecls
        rewritten_tydecls
        ~f:
          (fun
            original_tydecl { checked_with_fuelproof = _; tydecl = rewritten_tydecl } ->
          let original_tydecl = Ppxlib.name_type_params_in_td original_tydecl in
          let attributes =
            if has_unsafe_allow_any_mode_crossing rewritten_tydecl.ptype_attributes
               && not
                    (has_unsafe_allow_any_mode_crossing original_tydecl.ptype_attributes)
            then with_unsafe_allow_any_mode_crossing original_tydecl.ptype_attributes ~loc
            else original_tydecl.ptype_attributes
          in
          let original_tydecl = { original_tydecl with ptype_attributes = attributes } in
          (* The redeclaration needs a re-export if [fuelproof_check_decl] is the original
             declaration of a nominal type, i.e. it is a nominal tydecl without a
             manifest. *)
          match original_tydecl.ptype_manifest with
          | Some _ -> original_tydecl
          | None ->
            let manifest =
              Some
                (Ast_builder.Default.ptyp_constr
                   ~loc
                   (Loc.make
                      ~loc
                      (Ldot (Lident module_name, original_tydecl.ptype_name.txt)))
                   (List.map original_tydecl.ptype_params ~f:fst))
            in
            { original_tydecl with
              ptype_manifest = manifest
            ; ptype_attributes = attributes
            })
      |> Ast_builder.Default.pstr_type rec_flag ~loc
      |> ghostify#structure_item
    in
    [%stri
      include struct
        open struct
          [@@@warning "-34"]

          [%%i fuelproof_check_decl]
        end

        [%%i decl_for_deriving_ppxes]
      end]
  | Error (err, err_loc) ->
    List.iter ~f:Attribute.explicitly_drop#type_declaration original_tydecls;
    let err = Location.error_extensionf ~loc:err_loc "%s" (Error.to_string_hum err) in
    Ast_builder.Default.pstr_extension err [] ~loc
;;

let create_extension_sig ~loc rec_flag tydecls =
  match rewrite_tydecls tydecls ~loc with
  | Ok tydecls ->
    Ast_builder.Default.psig_type
      rec_flag
      (List.map tydecls ~f:(fun decl -> decl.tydecl))
      ~loc
  | Error (err, err_loc) ->
    List.iter ~f:Attribute.explicitly_drop#type_declaration tydecls;
    let err = Location.error_extensionf ~loc:err_loc "%s" (Error.to_string_hum err) in
    Ast_builder.Default.psig_extension err [] ~loc
;;

let extension_str =
  Extension.declare
    "fuelproof"
    Structure_item
    Ast_pattern.(pstr (pstr_type __ __ ^:: nil))
    (fun ~loc ~path:_ rec_flag tydecls -> create_extension_str ~loc rec_flag tydecls)
;;

let extension_sig =
  Extension.declare
    "fuelproof"
    Signature_item
    Ast_pattern.(
      psig
        (map_value
           ~f:(fun s -> (Ppxlib_jane.Shim.Signature.of_parsetree s).psg_items)
           (psig_type __ __ ^:: nil)))
    (fun ~loc ~path:_ rec_flag tydecls -> create_extension_sig ~loc rec_flag tydecls)
;;

let () =
  Driver.register_transformation
    "fuelproof"
    ~rules:
      [ Context_free.Rule.extension extension_str
      ; Context_free.Rule.extension extension_sig
      ]
;;
