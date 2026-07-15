open! Ppxlib
open! Stdppx

(* register attributes --- we only use [array] in base right now *)
let () =
  let rules =
    List.concat_map
      ~f:(fun function_ -> Function.attributes function_ Surface_type.array)
      Function.all
  in
  Driver.register_transformation "array_base" ~rules
;;

(* exports *)
module Common = Common
module Function = Function
module Surface_type = Surface_type
