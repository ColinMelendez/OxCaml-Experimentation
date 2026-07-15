open! Core
module Bonsai = Bonsai.Cont
open Bonsai.Let_syntax

let value_with_override ?sexp_of_model ?equal value (local_ graph) =
  let state, set_state = Bonsai.state_opt graph ?sexp_of_model ?equal in
  let value =
    match%sub state with
    | Some override -> override
    | None -> value
  in
  let setter =
    let%arr set_state in
    fun v -> set_state (Some v)
  in
  value, setter
;;

let value_with_state_machine_override
  (type model action)
  ~(here : [%call_pos])
  ?reset
  ?sexp_of_model
  ?sexp_of_action
  ?equal
  ~(default_model : model Bonsai.t)
  ~(apply_action : _ -> model -> action -> model)
  (graph @ local)
  : model Bonsai.t * (action -> unit Bonsai.Effect.t) Bonsai.t
  =
  let state, inject =
    Bonsai.state_machine_with_input
      ~here
      ?reset:
        (Option.map reset ~f:(fun reset ctx model ->
           match model with
           | None -> None
           | Some model -> Some (reset ctx model)))
      ?sexp_of_model:(Option.map sexp_of_model ~f:Option.sexp_of_t)
      ?sexp_of_action
      ?equal:(Option.map equal ~f:Option.equal)
      ~default_model:None
      ~apply_action:(fun ctx default_model model action ->
        match model with
        | None ->
          (match default_model with
           | Inactive -> model
           | Active default_model -> Some (apply_action ctx default_model action))
        | Some model -> Some (apply_action ctx model action))
      default_model
      graph
  in
  let state =
    match%sub state with
    | None -> default_model
    | Some model -> model
  in
  state, inject
;;

let dynamic_cutoff a ~equal (local_ graph) =
  Bonsai.both a equal
  |> Bonsai.cutoff ~equal:(fun (a, _old_equal) (b, equal) -> equal a b)
  |> Bonsai.arr1 graph ~f:Tuple2.get1
;;
