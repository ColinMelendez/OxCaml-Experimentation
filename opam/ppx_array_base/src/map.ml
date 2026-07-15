open! Ppxlib
open! Stdppx

let name = "map"

let function_body loc surface_type context ~create ~lower_bound ~runtime_fun =
  let unwrap_in = Surface_type.unwrap_in surface_type context ~loc "t" in
  let wrap = Surface_type.wrap surface_type context ~loc in
  unwrap_in
    [%expr
      let len = [%e runtime_fun "length"] t in
      [%e
        wrap
          [%expr
            if len = 0
            then [||]
            else (
              let r = [%e create] in
              for i = [%e lower_bound] to len - 1 do
                [%e runtime_fun "unsafe_set"] r i (f ([%e runtime_fun "unsafe_get"] t i))
              done;
              r)]]]
;;

let implement_via_create
  loc
  surface_type
  context
  how_to_vary_kinds
  ~create
  ~lower_bound
  ~runtime_fun
  =
  let wrapping_type ty = Surface_type.to_core_type surface_type context ~loc ty in
  How_to_vary_kinds.structure_item
    how_to_vary_kinds
    loc
    ~function_name:name
    ~function_implementation:(fun ~input_type ~output_type ->
      [%expr
        fun (t : [%t wrapping_type input_type] @ local)
          ~(f : (_ -> _) @ local)
          : [%t wrapping_type output_type] ->
          [%e function_body loc surface_type context ~create ~lower_bound ~runtime_fun]])
;;

let implementation loc context surface_type ~overwrite_output_kinds =
  let how_to_vary_kinds =
    Context.how_to_vary_kinds
      context
      ~input:(How_to_vary_kinds.base_layouts loc)
      ~output_separable:true
  in
  let runtime_fun = Context.runtime_fun context loc in
  let implement_via_create = implement_via_create loc surface_type context ~runtime_fun in
  let safe_implementation ~output_kinds =
    implement_via_create
      (how_to_vary_kinds ~output:(Some output_kinds))
      ~create:
        [%expr [%e runtime_fun "create"] ~len (f ([%e runtime_fun "unsafe_get"] t 0))]
      ~lower_bound:[%expr 1]
  in
  match overwrite_output_kinds with
  | None ->
    [ implement_via_create
        (how_to_vary_kinds ~output:(Some [%expr base_non_value]))
        ~create:[%expr [%e runtime_fun "magic_create_uninitialized"] ~len]
        ~lower_bound:[%expr 0]
    ; safe_implementation
        ~output_kinds:[%expr value_or_null, value_or_null mod external64]
    ]
  | Some output_kinds -> [ safe_implementation ~output_kinds ]
;;

let interface loc context surface_type ~overwrite_output_kinds =
  let how_to_vary_kinds =
    let output =
      Option.value overwrite_output_kinds ~default:(How_to_vary_kinds.base_layouts loc)
    in
    Context.how_to_vary_kinds
      context
      ~input:(How_to_vary_kinds.base_layouts loc)
      ~output:(Some output)
      ~output_separable:true
  in
  let wrapping_type ty = Surface_type.to_core_type surface_type context ~loc ty in
  How_to_vary_kinds.signature_item
    how_to_vary_kinds
    loc
    ~function_name:name
    ~function_type:(fun ~input_type ~output_type ->
      [%type:
        [%t wrapping_type input_type] @ local
        -> f:([%t input_type] -> [%t output_type]) @ local
        -> [%t wrapping_type output_type]])
;;
