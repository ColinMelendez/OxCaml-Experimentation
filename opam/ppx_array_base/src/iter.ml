open! Ppxlib
open! Stdppx

let name = "iter"

let validate_cannot_overwrite_output_kinds =
  Common.validate_cannot_overwrite_output_kinds ~function_name:name
;;

let implementation loc context surface_type ~overwrite_output_kinds =
  validate_cannot_overwrite_output_kinds loc ~overwrite_output_kinds;
  let how_to_vary_kinds =
    Context.how_to_vary_kinds
      context
      ~input:(How_to_vary_kinds.base_layouts loc)
      ~output:None
      ~output_separable:false
  in
  let runtime_fun = Context.runtime_fun context loc in
  let unwrap_in = Surface_type.unwrap_in surface_type context ~loc "t" in
  [ How_to_vary_kinds.structure_item
      how_to_vary_kinds
      loc
      ~function_name:name
      ~function_implementation:(fun ~input_type ~output_type:(_ : core_type) ->
        [%expr
          fun (t : [%t Surface_type.to_core_type surface_type context ~loc input_type])
            ~(f : (_ -> _) @ local) ->
            [%e
              unwrap_in
                [%expr
                  for i = 0 to [%e runtime_fun "length"] t - 1 do
                    f ([%e runtime_fun "unsafe_get"] t i)
                  done]]])
  ]
;;

let interface loc context surface_type ~overwrite_output_kinds =
  validate_cannot_overwrite_output_kinds loc ~overwrite_output_kinds;
  let how_to_vary_kinds =
    Context.how_to_vary_kinds
      context
      ~input:(How_to_vary_kinds.base_layouts loc)
      ~output:None
      ~output_separable:false
  in
  How_to_vary_kinds.signature_item
    how_to_vary_kinds
    loc
    ~function_name:name
    ~function_type:(fun ~input_type ~output_type:(_ : core_type) ->
      [%type:
        [%t Surface_type.to_core_type surface_type context ~loc input_type]
        -> f:([%t input_type] -> unit) @ local
        -> unit])
;;
