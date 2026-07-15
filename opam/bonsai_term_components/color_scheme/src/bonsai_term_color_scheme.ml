open! Core
open! Bonsai_term
module Catppuccin = Bonsai_term_color_scheme_catppuccin
module Vscode_dark = Bonsai_term_color_scheme_vscode_dark
module Vscode_light = Bonsai_term_color_scheme_vscode_light
module Gruvbox_dark = Bonsai_term_color_scheme_gruvbox_dark
module Gruvbox_light = Bonsai_term_color_scheme_gruvbox_light
module Dracula = Bonsai_term_color_scheme_dracula
module Kanagawa = Bonsai_term_color_scheme_kanagawa
module Tokyo_night_dark = Bonsai_term_color_scheme_tokyo_night_dark
module Tokyo_night_light = Bonsai_term_color_scheme_tokyo_night_light
module Monokai = Bonsai_term_color_scheme_monokai
module Bluloco = Bonsai_term_color_scheme_bluloco
module Solarized_dark = Bonsai_term_color_scheme_solarized_dark
module Solarized_light = Bonsai_term_color_scheme_solarized_light
module Terminal_16 = Bonsai_term_color_scheme_terminal_16
module Terminal_16_inverted = Bonsai_term_color_scheme_terminal_16_inverted
include Catppuccin

module Flavor_name = struct
  type t =
    | Catppuccin of Catppuccin.Flavor_name.t [@nested]
    | Vscode_dark
    | Vscode_light
    | Gruvbox_dark
    | Gruvbox_light
    | Dracula
    | Kanagawa
    | Tokyo_night_dark
    | Tokyo_night_light
    | Monokai
    | Bluloco
    | Solarized_dark
    | Solarized_light
    | Terminal_16
    | Terminal_16_inverted
  [@@deriving sexp ~portable, equal ~portable, enumerate, compare ~portable, string]

  let to_string_hum = function
    | Catppuccin name -> Catppuccin.Flavor_name.to_string name
    | other -> to_string other
  ;;

  let of_string s =
    match Catppuccin.Flavor_name.of_string s with
    | name -> Catppuccin name
    | exception _ -> of_string s
  ;;

  let to_flavor = function
    | Vscode_dark -> Vscode_dark.flavor
    | Vscode_light -> Vscode_light.flavor
    | Gruvbox_dark -> Gruvbox_dark.flavor
    | Gruvbox_light -> Gruvbox_light.flavor
    | Dracula -> Dracula.flavor
    | Kanagawa -> Kanagawa.flavor
    | Tokyo_night_dark -> Tokyo_night_dark.flavor
    | Tokyo_night_light -> Tokyo_night_light.flavor
    | Monokai -> Monokai.flavor
    | Bluloco -> Bluloco.flavor
    | Solarized_dark -> Solarized_dark.flavor
    | Solarized_light -> Solarized_light.flavor
    | Terminal_16 -> Terminal_16.flavor
    | Terminal_16_inverted -> Terminal_16_inverted.flavor
    | Catppuccin flavor_name -> Catppuccin.Flavor_name.to_flavor flavor_name
  ;;
end

let color_scheme_env_var_name = "BONSAI_TERM_COLOR_SCHEME"

let known_theme_names : Flavor_name.t String.Map.t ref =
  ref
    (List.map Flavor_name.all ~f:(fun name -> Flavor_name.to_string_hum name, name)
     |> String.Map.of_alist_exn)
;;

(* Keep track of whether or not we've emitted a warning about not being able to parse the
   color scheme env var, as it should only be emitted once. Without this guard, we would
   print the warning multiple times because [get_user_default_theme] is repeatedly called
   as the dynamic scope fallback.
*)
let emitted_env_var_warning = ref false

let get_user_default_theme_name () =
  match Sys.getenv color_scheme_env_var_name with
  | Some s ->
    (match Core.Map.find !known_theme_names s with
     | Some (_ : Flavor_name.t) as result -> result
     | None ->
       if not !emitted_env_var_warning
       then (
         emitted_env_var_warning := true;
         eprintf
           "Warning: %s=%s is not a valid color scheme. Valid values: %s\n"
           color_scheme_env_var_name
           s
           (Core.Map.keys !known_theme_names |> String.concat ~sep:", "));
       None)
  | None -> None
;;

let get_user_default_theme () =
  Option.map (get_user_default_theme_name ()) ~f:Flavor_name.to_flavor
;;

module For_testing = struct
  let reset_emitted_env_var_warning () = emitted_env_var_warning := false
end

let default_flavor () = Option.value (get_user_default_theme ()) ~default:Mocha.flavor

let variable =
  Bonsai.Dynamic_scope.create
    ~name:"bonsai-term-color-scheme-flavor"
    ~fallback:default_flavor
    ()
;;

let color' c (local_ graph) =
  let open Bonsai.Let_syntax in
  let get_flavor = Bonsai.Dynamic_scope.lookup variable graph in
  let%arr c and get_flavor in
  color ~flavor:(get_flavor ()) c
;;

let flavor graph =
  let open Bonsai.Let_syntax in
  let get_flavor = Bonsai.Dynamic_scope.lookup variable graph in
  let%arr get_flavor in
  get_flavor ()
;;

let set_flavor_within flavor inside =
  let open Bonsai.Let_syntax in
  let get_flavor =
    let%arr flavor in
    fun () -> flavor
  in
  Bonsai.Dynamic_scope.set variable get_flavor ~inside
;;

let set_flavor_within_app flavor inside (local_ graph) =
  let open Bonsai.Let_syntax in
  let get_flavor =
    let%arr flavor in
    fun () -> flavor
  in
  let%sub view, handler =
    Bonsai.Dynamic_scope.set variable get_flavor graph ~inside:(fun (local_ graph) ->
      let ~view, ~handler = inside graph in
      Bonsai.both view handler)
  in
  let view =
    let%arr view and get_flavor in
    let flavor = get_flavor () in
    let bg = color ~flavor Base
    and fg = color ~flavor Text in
    Bonsai_term.View.with_colors ~fill_backdrop:true view ~fg ~bg
  in
  ~view, ~handler
;;
