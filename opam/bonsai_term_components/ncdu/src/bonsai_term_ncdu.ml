open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Weight = struct
  type 'a t =
    { self : 'a
    ; dominated : 'a
    }
  [@@deriving sexp_of]
end

module type Key = sig
  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  val to_string_hum : t -> string
  val catppuccin_color : t -> Bonsai_term_color_scheme.t option
end

module type Size = sig
  type t [@@deriving compare, equal]

  include Core.Container.Summable with type t := t

  val to_string : t -> string
  val to_float : t -> float
end

module Make (Key : Key) (Size : Size) = struct
  module Tree_node = struct
    type t =
      { name : Key.t
      ; weight : Size.t Weight.t
      ; children : t Key.Map.t
      }
  end

  let bg = Bonsai_term_color_scheme.Crust

  let render_instruction key action (local_ graph) =
    let%arr flavor = Bonsai_term_color_scheme.flavor graph in
    let text = Bonsai_term_color_scheme.color ~flavor Text in
    let subtext = Bonsai_term_color_scheme.color ~flavor Subtext0 in
    let crust = Bonsai_term_color_scheme.color ~flavor bg in
    View.hcat
      [ View.text ~attrs:[ Attr.bold; Attr.fg text; Attr.bg crust ] key
      ; View.text ~attrs:[ Attr.bg crust ] " "
      ; View.text ~attrs:[ Attr.fg subtext; Attr.bg crust ] action
      ]
  ;;

  let instructions ~navigation (local_ graph) =
    let instructions =
      match%sub navigation with
      | [] ->
        Bonsai.all
          ([ "j", "Down"; "k", "Up"; "Enter", "go in" ]
           |> List.map ~f:(fun (key, action) -> render_instruction key action graph))
      | _ ->
        Bonsai.all
          ([ "j", "Down"; "k", "Up"; "Enter", "go in"; "Backspace", "go back" ]
           |> List.map ~f:(fun (key, action) -> render_instruction key action graph))
    in
    let%arr instructions
    and text = Text.component graph in
    View.hcat (List.intersperse ~sep:(text "  ") instructions)
  ;;

  let top_bar ~app_title ~navigation (local_ graph) =
    let instructions = instructions ~navigation graph in
    let%arr instructions
    and text = Text.component graph
    and flavor = Bonsai_term_color_scheme.flavor graph in
    let mauve = Bonsai_term_color_scheme.color ~flavor Mauve in
    View.hcat
      [ text ~attrs:[ Attr.fg mauve; Attr.bold ] app_title; text "   "; instructions ]
  ;;

  let bottom_bar ~tree_name ~separator ~navigation ~total_size ~nodes (local_ graph) =
    let current_size =
      let%arr nodes in
      List.sum (module Size) nodes ~f:(fun tree_node ->
        tree_node.Tree_node.weight.dominated)
    in
    let%arr navigation
    and current_size
    and total_size
    and text = Text.component graph
    and flavor = Bonsai_term_color_scheme.flavor graph in
    let maybe_navigation =
      match navigation with
      | [] -> View.none
      | _ :: _ ->
        let navigation = List.rev navigation in
        let arrow = text separator in
        let banner =
          View.hcat
          @@ List.intersperse ~sep:arrow
          @@ List.map navigation ~f:(fun navigation ->
            let current = Key.to_string_hum navigation in
            text
              ~attrs:
                [ Attr.fg (Bonsai_term_color_scheme.color ~flavor Lavender); Attr.bold ]
              [%string "%{current}"])
        in
        View.hcat
          [ arrow
          ; banner
          ; text
              ~attrs:[ Attr.fg (Bonsai_term_color_scheme.color ~flavor Yellow) ]
              [%string " (%{Size.to_string current_size})"]
          ]
    in
    View.hcat
      [ text ~attrs:[ Attr.fg (Bonsai_term_color_scheme.color ~flavor Peach) ] tree_name
      ; text " "
      ; text
          ~attrs:[ Attr.fg (Bonsai_term_color_scheme.color ~flavor Yellow) ]
          [%string "(%{Size.to_string total_size} total)"]
      ; text " "
      ; maybe_navigation
      ]
  ;;

  let backdrop (dimensions : Dimensions.t Bonsai.t) (local_ graph) =
    let flavor = Bonsai_term_color_scheme.flavor graph in
    let%arr { height; width } = dimensions
    and flavor in
    let bg_color = Bonsai_term_color_scheme.color ~flavor bg in
    View.rectangle ~attrs:[ Attr.bg bg_color ] ~height ~width ()
  ;;

  module Navigation = struct
    (* [Navigation] is kind of like a URL. We use it to know which hotkeys we should
       display as instructions and also to render the currently navigated tree node path
       in the bottom bar. *)
    type t = Key.t list [@@deriving compare, sexp_of]

    include functor Comparable.Make_plain
  end

  module Focus = struct
    (* NOTE: This state machine keeps the "currently focused item" in view. It does so by
       only making the bonsai web ui scroller when the currently focused item is out of
       view. *)
    type t =
      | Down
      | Up
      | Top
      | Bottom
      | Other_key_pressed
      | Up_half_page
      | Down_half_page

    type model =
      { focus : int
      ; last_top_press : Time_ns.t option
      }

    type input =
      { time_source : Bonsai.Time_source.t
      ; item_count : int
      ; scroll_into_view : bottom:int -> top:int -> unit Effect.t
      ; dimensions : Dimensions.t
      }

    let apply_action
      context
      (input : input Bonsai.Computation_status.t)
      ({ focus; last_top_press } as model : model)
      (action : t)
      =
      match input with
      | Inactive -> model
      | Active { item_count; time_source; scroll_into_view; dimensions } ->
        let bottom_most = item_count - 1 in
        let new_model =
          match action with
          | Down ->
            if focus >= item_count - 1
            then { last_top_press = None; focus = item_count - 1 }
            else { last_top_press = None; focus = focus + 1 }
          | Up -> { last_top_press = None; focus = Int.max 0 (focus - 1) }
          | Other_key_pressed -> { last_top_press = None; focus }
          | Top ->
            let now = Bonsai.Time_source.now time_source in
            (match last_top_press with
             | None -> { last_top_press = Some now; focus }
             | Some last_top_press ->
               (match
                  Time_ns.Span.O.(
                    Time_ns.diff now last_top_press < Time_ns.Span.of_sec 0.3)
                with
                | true -> { last_top_press = None; focus = 0 }
                | false -> { last_top_press = Some now; focus }))
          | Bottom -> { last_top_press = None; focus = bottom_most }
          | Down_half_page ->
            { last_top_press = None
            ; focus = Int.min bottom_most (focus + (dimensions.height / 2))
            }
          | Up_half_page ->
            { last_top_press = None; focus = Int.max 0 (focus - (dimensions.height / 2)) }
        in
        if not (focus = new_model.focus)
        then
          Bonsai.Apply_action_context.schedule_event
            context
            (scroll_into_view ~bottom:new_model.focus ~top:new_model.focus);
        new_model
    ;;

    let component ~inject_scroller ~viewport_dimensions ~nodes (local_ graph) =
      let input =
        let%arr inject_scroller
        and viewport_dimensions
        and nodes
        and time_source = Bonsai.Incr.with_clock ~f:Ui_incr.return graph in
        let scroll_into_view ~bottom ~top =
          inject_scroller (Bonsai_term_scroller.Action.Scroll_to { top; bottom })
        in
        { scroll_into_view
        ; dimensions = viewport_dimensions
        ; item_count = Map.length nodes
        ; time_source
        }
      in
      Bonsai.state_machine_with_input
        ~default_model:{ focus = 0; last_top_press = None }
        ~apply_action
        input
        graph
    ;;
  end

  let ncdu_rows ~nodes ~focus ~viewport_height (local_ graph) =
    let max_weight =
      let%arr nodes in
      let%map.Option node =
        List.max_elt
          nodes
          ~compare:
            (Comparable.lift [%compare: Size.t] ~f:(fun node ->
               node.Tree_node.weight.dominated))
      in
      node.weight.dominated
    in
    (* Precompute size strings and max width once per navigation change, not per focus
       change. *)
    let nodes_with_sizes =
      let%arr nodes in
      List.map nodes ~f:(fun node -> node, Size.to_string node.weight.dominated)
    in
    let max_size_width =
      let%arr nodes_with_sizes in
      List.map nodes_with_sizes ~f:(fun (_, size_hum) -> String.length size_hum)
      |> List.max_elt ~compare:[%compare: int]
      |> Option.value ~default:4
    in
    let total_items =
      let%arr nodes_with_sizes in
      List.length nodes_with_sizes
    in
    let%arr text = Text.component graph
    and flavor = Bonsai_term_color_scheme.flavor graph
    and nodes_with_sizes
    and focus
    and max_weight
    and max_size_width
    and total_items
    and viewport_height in
    (* Only render items in the visible window to avoid O(n) View allocation on every
       focus change. Items outside the window are replaced by [View.pad] spacers so the
       scroller's total content height stays correct. *)
    let buffer = 5 in
    let render_start = Int.max 0 (focus - viewport_height - buffer) in
    let render_end = Int.min total_items (focus + viewport_height + buffer) in
    let visible_rows =
      List.filter_mapi nodes_with_sizes ~f:(fun i (node, size_hum) ->
        if i < render_start || i >= render_end
        then None
        else (
          let is_focused = i = focus in
          let maybe_focused =
            if is_focused
            then
              Attr.many
                [ Attr.fg (Bonsai_term_color_scheme.color ~flavor Green); Attr.bold ]
            else Attr.empty
          in
          let name =
            text
              ~attrs:
                [ Option.value_map
                    (Key.catppuccin_color node.name)
                    ~default:Attr.empty
                    ~f:(fun color ->
                      Attr.fg (Bonsai_term_color_scheme.color ~flavor color))
                ; maybe_focused
                ]
              (Key.to_string_hum node.name)
          in
          let weight =
            let string = size_hum in
            let string = String.pad_left string ~len:(max_size_width + 1) in
            text ~attrs:[ maybe_focused ] string
          in
          let prefix =
            text
              ~attrs:[ Attr.fg (Bonsai_term_color_scheme.color ~flavor Mauve) ]
              (if is_focused then "> " else "  ")
          in
          let bar =
            let width = 20 in
            let current_width =
              match max_weight with
              | None -> 0
              | Some max_weight ->
                Option.try_with (fun () ->
                  Float.to_int
                    (Float.of_int width
                     *. (Size.to_float node.weight.dominated /. Size.to_float max_weight)
                    ))
                |> Option.value ~default:0
            in
            text
              ~attrs:
                [ (if is_focused
                   then
                     Attr.many
                       [ Attr.bg (Bonsai_term_color_scheme.color ~flavor Mauve)
                       ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Crust)
                       ]
                   else Attr.empty)
                ]
              (String.concat
                 [ "["; String.pad_right (String.make current_width '#') ~len:width; "]" ])
          in
          Some (View.hcat [ prefix; weight; text " "; bar; text " "; name ])))
    in
    View.pad ~t:render_start ~b:(total_items - render_end) (View.vcat visible_rows)
  ;;

  let sort_nodes_by_descending_weight ~nodes =
    let%arr nodes in
    let nodes = Map.data nodes in
    let compare =
      Comparable.lift (Comparable.reverse Size.compare) ~f:(fun x ->
        x.Tree_node.weight.dominated)
    in
    List.sort nodes ~compare
  ;;

  let main_key_handler
    ~nodes
    ~focus
    ~inject_focus
    ~inject_navigation
    ~inject_scroller
    ~navigation
    ~on_leaf_select
    (local_ _graph)
    =
    let%arr nodes
    and focus
    and inject_focus
    and inject_navigation
    and inject_scroller
    and navigation
    and on_leaf_select in
    fun (event : Event.t) ->
      match event with
      | Key_press { key = Backspace; mods = [] }
      | Key_press { key = ASCII 'h'; mods = [] }
      | Key_press { key = Arrow `Left; mods = [] } -> inject_navigation `Pop
      | Key_press { key = Enter; mods = [] }
      | Key_press { key = ASCII 'l'; mods = [] }
      | Key_press { key = Arrow `Right; mods = [] } ->
        (match List.nth nodes focus with
         | None -> Effect.Ignore
         | Some node ->
           if Map.is_empty node.Tree_node.children
           then (
             match on_leaf_select with
             | None -> Effect.Ignore
             | Some on_leaf_select ->
               let path = Nonempty_list.create node.name navigation in
               on_leaf_select path)
           else inject_navigation (`Add node.name))
      | Key_press { key = ASCII 'k'; mods = [] }
      | Key_press { key = Arrow `Up; mods = [] }
      | Mouse { kind = Scroll `Up; _ } -> inject_focus Focus.Up
      | Key_press { key = ASCII ('d' | 'D'); mods = [ Ctrl ] | [] } ->
        inject_focus Down_half_page
      | Key_press { key = ASCII ('u' | 'U'); mods = [ Ctrl ] | [] } ->
        inject_focus Up_half_page
      | Key_press { key = ASCII ('e' | 'E'); mods = [ Ctrl ] } ->
        Effect.all_unit
          [ inject_scroller Bonsai_term_scroller.Action.Down; inject_focus Down ]
      | Key_press { key = ASCII ('y' | 'Y'); mods = [ Ctrl ] } ->
        Effect.all_unit [ inject_scroller Up; inject_focus Up ]
      | Key_press { key = ASCII 'j'; mods = [] }
      | Key_press { key = Arrow `Down; mods = [] }
      | Mouse { kind = Scroll `Down; _ } -> inject_focus Down
      | Key_press { key = ASCII 'G'; mods = [] } -> inject_focus Bottom
      | Key_press { key = ASCII 'g'; mods = [] } -> inject_focus Top
      | _ -> inject_focus Other_key_pressed
  ;;

  let component
    ~app_title
    ~dimensions
    ~tree_name
    ~separator
    ~on_leaf_select
    ~(nodes : Tree_node.t Key.Map.t Bonsai.t)
    ~(total_size : Size.t Bonsai.t)
    (local_ graph)
    =
    let navigation, inject_navigation =
      Bonsai.state_machine
        ~default_model:[]
        ~apply_action:(fun _ model action ->
          match action with
          | `Pop -> Option.value ~default:[] (List.tl model)
          | `Add x -> x :: model)
        graph
    in
    let top_bar = top_bar ~app_title ~navigation graph in
    Bonsai.scope_model
      (module Navigation)
      ~on:navigation
      ~for_:(fun (local_ graph) ->
        let nodes =
          let%arr navigation and nodes in
          let rec loop navigation nodes =
            match navigation with
            | [] -> Some nodes
            | first :: rem ->
              let%bind.Option first = Map.find nodes first in
              loop rem first.Tree_node.children
          in
          loop (List.rev navigation) nodes
        in
        let nodes =
          let%arr nodes in
          match nodes with
          | None -> Key.Map.empty
          | Some x -> x
        in
        let viewport_dimensions = dimensions in
        let%sub ~bottom_bar, ~content, ~nodes, ~inject_scroller, ~focus, ~inject_focus =
          Bonsai_extra.Fixed_point.with_inject_fixed_point
            (fun inject_scroller (local_ graph) ->
              let%sub { focus; _ }, inject_focus =
                Tuple2.uncurry Bonsai.both
                @@ Focus.component ~inject_scroller ~viewport_dimensions ~nodes graph
              in
              let nodes = sort_nodes_by_descending_weight ~nodes in
              let bottom_bar =
                bottom_bar ~tree_name ~separator ~navigation ~total_size ~nodes graph
              in
              let viewport_height =
                let%arr { Dimensions.height; _ } = viewport_dimensions in
                height
              in
              let ncdu_rows = ncdu_rows ~nodes ~focus ~viewport_height graph in
              let%sub { view = content
                      ; inject = inject_scroller
                      ; less_keybindings_handler = _
                      ; stuck_to_bottom = _
                      ; scroll_position = _
                      }
                =
                Bonsai_term_scroller.component
                  ~crop_width_if_too_big:`No
                  ~dimensions:viewport_dimensions
                  ncdu_rows
                  graph
              in
              let content =
                let%arr bottom_bar
                and content
                and nodes
                and inject_scroller
                and focus
                and inject_focus in
                ~bottom_bar, ~content, ~nodes, ~inject_scroller, ~focus, ~inject_focus
              in
              content, inject_scroller)
            graph
        in
        let content_handler =
          main_key_handler
            ~nodes
            ~focus
            ~inject_focus
            ~inject_navigation
            ~inject_scroller
            ~navigation
            ~on_leaf_select
            graph
        in
        let view =
          let%arr content and top_bar and bottom_bar and viewport_dimensions in
          let content =
            View.pad
              ~b:(Int.max 0 (viewport_dimensions.height - View.height content))
              content
          in
          View.vcat [ top_bar; View.text ""; content; View.text ""; bottom_bar ]
        in
        let%arr view and content_handler in
        view, content_handler)
      graph
  ;;

  let component
    ~app_title
    ~tree_name
    ~dimensions
    ?(separator = " -> ")
    ?on_leaf_select
    ~(total_size : Size.t Bonsai.t)
    ~(nodes : Tree_node.t Key.Map.t Bonsai.t)
    (local_ graph)
    =
    let on_leaf_select =
      match on_leaf_select with
      | None -> Bonsai.return None
      | Some cb ->
        let%arr cb in
        Some cb
    in
    let backdrop = backdrop dimensions graph in
    let%sub app, handler =
      let dimensions =
        let%arr dimensions in
        { Dimensions.height = dimensions.height - 6; width = dimensions.width - 2 }
      in
      component
        ~app_title
        ~separator
        ~on_leaf_select
        ~dimensions
        ~tree_name
        ~nodes
        ~total_size
        graph
    in
    let view =
      let%arr app and backdrop in
      View.zcat [ View.pad ~l:2 ~t:1 app; backdrop ]
    in
    ~view, ~handler
  ;;
end
