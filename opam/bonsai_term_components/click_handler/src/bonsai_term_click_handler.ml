open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Context = struct
  type t =
    { relative_position : Position.t
    ; absolute_position : Position.t
    }
  [@@deriving sexp_of]
end

module Identity : sig
  type t

  module Maker : sig
    type identity := t
    type 'id t

    val create : equal:('id -> 'id -> bool) -> 'id t
    val make : 'id t -> 'id -> identity
  end

  val same : t -> t -> bool
end = struct
  type t =
    | T :
        { type_id : 'id Type_equal.Id.t
        ; equal : 'id -> 'id -> bool
        ; value : 'id
        }
        -> t

  let same (T a) (T b) =
    match Type_equal.Id.same_witness a.type_id b.type_id with
    | Some T -> a.equal a.value b.value
    | None -> false
  ;;

  module Maker = struct
    type identity = t

    type 'id t =
      { type_id : 'id Type_equal.Id.t
      ; equal : 'id -> 'id -> bool
      }

    let create ~equal =
      let type_id =
        Type_equal.Id.create ~name:"click_handler_identity" [%sexp_of: opaque]
      in
      { type_id; equal }
    ;;

    let make (maker : 'id t) (value : 'id) : identity =
      T { type_id = maker.type_id; equal = maker.equal; value }
    ;;
  end
end

module Region_with_handler = struct
  type t =
    { region : Region.t
    ; on_click : Context.t -> unit Effect.t
    ; path : Bonsai.Path.t
    ; identity : Identity.t
    }
end

let create_tag
  (type item comparator_witness)
  (m :
    (module Comparator.S
       with type t = item
        and type comparator_witness = comparator_witness))
  =
  View.Tag.create
    m
    ~transform_regions:(fun (regions_with_handlers : Region_with_handler.t list) f ->
      let%map.List ({ region; _ } as region_with_handlers) = regions_with_handlers in
      let region = f region in
      { region_with_handlers with region })
    ~reduce:(fun (a : Region_with_handler.t list) b -> b @ a)
;;

let button_tag : (unit, Region_with_handler.t list) View.Tag.t = create_tag (module Unit)

let right_button_tag : (unit, Region_with_handler.t list) View.Tag.t =
  create_tag (module Unit)
;;

module With_id = struct
  module type S = sig
    type t [@@deriving equal]
  end

  let generic_click_handler (type id) ~tag (module Id : S with type t = id) (local_ graph)
    =
    let path = Bonsai.path graph in
    let identity_maker = Identity.Maker.create ~equal:Id.equal in
    let%arr path in
    fun ~(id : id) ~item ~on_click view ->
      let identity = Identity.Maker.make identity_maker id in
      View.Tag.mark view ~id:tag ~key:item ~f:(fun region ->
        [ { Region_with_handler.on_click; region; path; identity } ])
  ;;

  let add_click_handler id_module (local_ graph) =
    let%arr handler = generic_click_handler ~tag:button_tag id_module graph in
    fun id view ~on_click ->
      handler ~id ~item:() view ~on_click:(fun (_ : Context.t) -> on_click)
  ;;

  let add_right_click_handler id_module (local_ graph) =
    let%arr handler = generic_click_handler ~tag:right_button_tag id_module graph in
    fun id view ~on_right_click ->
      handler ~id ~item:() view ~on_click:(fun (_ : Context.t) -> on_right_click)
  ;;
end

let add_click_handler_with_context (local_ graph) =
  let path = Bonsai.path graph in
  let identity_maker = Identity.Maker.create ~equal:[%equal: unit] in
  let%arr path in
  fun view ~on_click ->
    let identity = Identity.Maker.make identity_maker () in
    View.Tag.mark view ~id:button_tag ~key:() ~f:(fun region ->
      [ { Region_with_handler.on_click; region; path; identity } ])
;;

let add_click_handler (local_ graph) =
  let add_handler = add_click_handler_with_context graph in
  let%arr add_handler in
  fun view ~on_click -> add_handler view ~on_click:(fun _ -> on_click)
;;

let add_right_click_handler_with_context (local_ graph) =
  let path = Bonsai.path graph in
  let identity_maker = Identity.Maker.create ~equal:Unit.equal in
  let%arr path in
  fun view ~on_right_click ->
    let identity = Identity.Maker.make identity_maker () in
    View.Tag.mark view ~id:right_button_tag ~key:() ~f:(fun region ->
      [ { Region_with_handler.on_click = on_right_click; region; path; identity } ])
;;

let add_right_click_handler (local_ graph) =
  let add_handler = add_right_click_handler_with_context graph in
  let%arr add_handler in
  fun view ~on_right_click -> add_handler view ~on_right_click:(fun _ -> on_right_click)
;;

module Captured_or_not = struct
  type t =
    | Captured
    | Ignored
end

module State_machine = struct
  module Model = struct
    type t =
      { last_press :
          [ `Left of Region_with_handler.t | `Right of Region_with_handler.t ] option
      }

    let default = { last_press = None }
  end

  module Action = struct
    type t =
      | Mouse of
          { kind : Event.mouse_kind
          ; position : Position.t
          ; mods : Event.Modifier.t list
          }
  end

  let find_region_at_position
    (type item)
    ~(tag : (item, Region_with_handler.t list) View.Tag.t)
    ~item
    view
    position
    =
    match View.Tag.find view ~id:tag item with
    | None -> None
    | Some regions_with_handlers ->
      (* The list order here corresponds to paint order for zcat/hcat/vcat composition.
         When multiple regions overlap the click position, we break ties using paint order
         (first wins). *)
      List.find_map regions_with_handlers ~f:(fun region_with_handler ->
        if not (Region.contains region_with_handler.region position)
        then None
        else Some region_with_handler)
  ;;

  let find_region_that_was_clicked_at_position view position =
    find_region_at_position ~tag:button_tag view position
  ;;

  let find_region_that_was_right_clicked_at_position view position =
    find_region_at_position ~tag:right_button_tag view position
  ;;

  let apply_action
    ctx
    (input : View.t Bonsai.Computation_status.t)
    (model : Model.t)
    (action : Action.t)
    =
    let resolve_release
      ctx
      view
      ~absolute_position
      ~last_press_handler
      ~(find_region : View.t -> Position.t -> Region_with_handler.t option)
      =
      let ({ identity; region; path; on_click = _ } : Region_with_handler.t) =
        last_press_handler
      in
      let current_click = find_region view absolute_position in
      match current_click with
      | None -> ()
      | Some current_mouse_up ->
        let same_identity = Identity.same identity current_mouse_up.identity in
        let same_region =
          [%equal: Region.t * Bonsai.Path.t]
            (current_mouse_up.region, current_mouse_up.path)
            (region, path)
        in
        if same_identity && same_region
        then (
          let relative_position =
            { Position.x = absolute_position.x - current_mouse_up.region.x
            ; y = absolute_position.y - current_mouse_up.region.y
            }
          in
          let context = { Context.relative_position; absolute_position } in
          Bonsai.Apply_action_context.schedule_event
            ctx
            (current_mouse_up.on_click context))
    in
    match action with
    | Mouse { kind = Left; position; mods = [] } ->
      (match input with
       | Inactive -> Model.default, Captured_or_not.Captured
       | Active view ->
         let handler = find_region_that_was_clicked_at_position ~item:() view position in
         let last_press = Option.map handler ~f:(fun h -> `Left h) in
         { Model.last_press }, Captured)
    | Mouse { kind = Right; position; mods = [] } ->
      (match input with
       | Inactive -> Model.default, Captured_or_not.Captured
       | Active view ->
         (match find_region_that_was_right_clicked_at_position ~item:() view position with
          | None -> Model.default, Ignored
          | Some handler -> { last_press = Some (`Right handler) }, Captured))
    | Mouse { kind = Release; position = absolute_position; mods = [] } ->
      (match input with
       | Inactive -> Model.default, Captured
       | Active view ->
         (match model.last_press with
          | None -> ()
          | Some (`Left last_press_handler) ->
            resolve_release
              ctx
              view
              ~absolute_position
              ~last_press_handler
              ~find_region:(fun view position ->
                find_region_that_was_clicked_at_position ~item:() view position)
          | Some (`Right last_press_handler) ->
            resolve_release
              ctx
              view
              ~absolute_position
              ~last_press_handler
              ~find_region:(fun view position ->
                find_region_that_was_right_clicked_at_position ~item:() view position));
         Model.default, Captured)
    | Mouse { kind = Drag; position = _; mods = [] } -> model, Ignored
    | Mouse { kind = Scroll _ | Middle; position = _; mods = _ } -> Model.default, Ignored
    | Mouse { kind = Drag; position = _; mods = _ :: _ } -> Model.default, Ignored
    | Mouse { kind = Hover; position = _; mods = _ } -> model, Ignored
    | Mouse { kind = Left | Right | Release; position = _; mods = _ :: _ } ->
      Model.default, Ignored
  ;;

  let component (view : View.t Bonsai.t) (local_ graph) =
    let _model, inject =
      Bonsai.actor_with_input ~default_model:Model.default ~recv:apply_action view graph
    in
    inject
  ;;
end

module Custom_handler = struct
  type 'item t =
    { create_handler :
        'id.
        (module With_id.S with type t = 'id)
        -> local_ Bonsai.graph
        -> (id:'id
            -> item:'item
            -> on_event:(Context.t -> unit Effect.t)
            -> View.t
            -> View.t)
             Bonsai.t
    ; maybe_handle : 'item -> Position.t -> View.t -> Captured_or_ignored.t Effect.t
    }

  let create_custom_handler (type item) (module Item : Comparable.S with type t = item)
    : item t
    =
    let tag : (item, Region_with_handler.t list) View.Tag.t = create_tag (module Item) in
    let create_handler (type id) (module Id : With_id.S with type t = id) (local_ graph) =
      let%arr handler = With_id.generic_click_handler ~tag (module Id) graph in
      fun ~id ~item ~on_event view -> handler ~id ~item ~on_click:on_event view
    in
    let maybe_handle (item : item) (position : Position.t) (view : View.t) =
      match State_machine.find_region_at_position ~tag ~item view position with
      | None -> Captured_or_ignored.ignore
      | Some region_with_handler ->
        let relative_position =
          { Position.x = position.x - region_with_handler.region.x
          ; y = position.y - region_with_handler.region.y
          }
        in
        let context = { Context.relative_position; absolute_position = position } in
        Captured_or_ignored.capture (region_with_handler.on_click context)
    in
    { create_handler; maybe_handle }
  ;;
end

let handler' ~view (local_ graph) =
  let inject = State_machine.component view graph in
  let%arr inject in
  fun ~kind ~position ~mods -> inject (Mouse { kind; position; mods })
;;

let handler ~view ~handler:default_handler (local_ graph) =
  let handle_click_event = handler' ~view graph in
  let%arr default_handler and handle_click_event in
  fun (event : Event.t) ->
    match event with
    | Mouse { kind; position; mods } ->
      (match%bind.Effect handle_click_event ~kind ~position ~mods with
       | Captured -> Effect.Ignore
       | Ignored -> default_handler event)
    | event -> default_handler event
;;

module Private = struct
  module Identity = Identity
end
