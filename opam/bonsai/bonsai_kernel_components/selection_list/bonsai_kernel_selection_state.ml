open! Core
open Bonsai.Let_syntax

module Peek_or_ignore = struct
  module Let_syntax = struct
    module Let_syntax = struct
      let bind
        (t : 'a Bonsai.Computation_status.t Bonsai.Effect.t)
        ~(f : 'a -> unit Bonsai.Effect.t)
        : unit Bonsai.Effect.t
        =
        match%bind.Bonsai.Effect t with
        | Bonsai.Computation_status.Active result -> f result
        | Inactive -> Bonsai.Effect.Ignore
      ;;
    end
  end
end

module One_of_many = struct
  type 'a t = 'a Bonsai.t * ('a -> unit Bonsai.Effect.t) Bonsai.t

  module Selection_policy = struct
    type 'a t =
      [ `Select_first_item
      | `Select_custom of 'a option -> 'a
      ]

    let fallback_selection t ~equal ~selection ~items : 'a Bonsai.t =
      match%sub t with
      | `Select_first_item ->
        let%arr items and selection in
        (match selection with
         | Some x when Nonempty_list.mem items x ~equal -> x
         | _ -> Nonempty_list.hd items)
      | `Select_custom f ->
        let%arr selection and f in
        f selection
    ;;
  end

  let create
    ~equal
    ?(selection_policy = Bonsai.return `Select_first_item)
    items
    (local_ graph)
    =
    let selected_opt, set_selected_opt =
      Bonsai.state ~equal:(Option.equal equal) None graph
    in
    let fallback_selection =
      Selection_policy.fallback_selection
        selection_policy
        ~equal
        ~selection:(selected_opt : 'a option Bonsai.t)
        ~items
    in
    Bonsai.Edge.on_change
      items
      ~trigger:`Before_display
      ~equal:(Nonempty_list.equal equal)
      ~callback:
        (let%arr peek_selected_opt = Bonsai.peek selected_opt graph
         and peek_fallback_selection = Bonsai.peek fallback_selection graph
         and set_selected_opt in
         fun items ->
           match%bind.Peek_or_ignore peek_selected_opt with
           | Some item when Nonempty_list.mem items item ~equal -> Bonsai.Effect.Ignore
           | _ ->
             let%bind.Peek_or_ignore fallback_selection = peek_fallback_selection in
             set_selected_opt (Some fallback_selection))
      graph;
    let selected =
      let%arr items and selected_opt and fallback_selection in
      match selected_opt with
      | Some item when Nonempty_list.mem items item ~equal -> item
      | _ -> fallback_selection
    in
    let set_selected =
      let%arr set_selected_opt in
      fun selected -> set_selected_opt (Some selected)
    in
    selected, set_selected
  ;;
end

module Any_of_many = struct
  type 'a t = 'a list Bonsai.t * ('a list -> unit Bonsai.Effect.t) Bonsai.t

  let create ~equal ?(init = Bonsai.return []) (local_ graph) =
    Bonsai_kernel_value_utilities.value_with_override ~equal:(List.equal equal) init graph
  ;;

  let is_item_selected t ~equal =
    let items, _ = t in
    let%arr items in
    fun item -> List.mem items item ~equal
  ;;

  let is_all_selected t ~equal items =
    let selected_items, _ = t in
    let%arr items and selected_items in
    List.for_all items ~f:(fun item -> List.mem selected_items ~equal item)
  ;;

  let is_some_selected t ~equal items =
    let selected_items, _ = t in
    let%arr items and selected_items in
    List.find items ~f:(fun item -> List.mem selected_items ~equal item) |> Option.is_some
  ;;

  let add_unique ~equal lst x = if List.mem lst x ~equal then lst else x :: lst
  let remove_all ~equal lst x = List.filter lst ~f:(fun y -> not (equal x y))

  let set_item_selected t ~equal =
    let items, set_items = t in
    let%arr items and set_items in
    fun item -> function
      | true -> set_items (add_unique ~equal items item)
      | false -> set_items (remove_all ~equal items item)
  ;;

  let is_none_selected t =
    let items, _ = t in
    let%arr items in
    List.is_empty items
  ;;

  let select_all (t : 'a t) items =
    let _, set_items = t in
    let%arr items and set_items in
    set_items items
  ;;

  let unselect_all (t : 'a t) =
    let _, set_items = t in
    let%arr set_items in
    set_items []
  ;;
end
