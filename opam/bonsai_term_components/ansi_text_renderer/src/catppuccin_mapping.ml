open! Core
open Bonsai_term

module Color_kind = struct
  type t =
    | Fg
    | Bg
end

(* Convert ansi_text Color to RGB tuple *)
let ansi_color_to_rgb (color : Ansi_text.Color.t) : (int * int * int) option =
  match color with
  | Default -> Some (255, 255, 255) (* Default to white *)
  | Standard sgr8 | Bright sgr8 ->
    (match sgr8 with
     | Black -> Some (0, 0, 0)
     | Red -> Some (255, 0, 0)
     | Green -> Some (0, 255, 0)
     | Yellow -> Some (255, 255, 0)
     | Blue -> Some (0, 0, 255)
     | Magenta -> Some (255, 0, 255)
     | Cyan -> Some (0, 255, 255)
     | White -> Some (255, 255, 255))
  | Rgb6 rgb6 ->
    let r, g, b = Ansi_text.Color.Rgb6.to_rgb rgb6 in
    let scale x = x * 255 / 5 in
    Some (scale r, scale g, scale b)
  | Gray24 gray24 ->
    let level = Ansi_text.Color.Gray24.to_level gray24 in
    let gray_value = level * 255 / 23 in
    Some (gray_value, gray_value, gray_value)
  | Rgb256 rgb256 ->
    let r, g, b = Ansi_text.Color.Rgb256.to_rgb rgb256 in
    Some (r, g, b)
;;

(* Color distance calculation for finding closest Catppuccin color *)
let color_distance (r1, g1, b1) (r2, g2, b2) =
  let dr = r1 - r2 in
  let dg = g1 - g2 in
  let db = b1 - b2 in
  Float.sqrt (Float.of_int ((dr * dr) + (dg * dg) + (db * db)))
;;

(* Find closest Catppuccin color to given RGB values, optionally avoiding colors too close
   to a background color *)
let find_closest_catppuccin_color
  (r, g, b)
  ~(flavor : Bonsai_term_color_scheme.Flavor.t)
  ~(bg : Bonsai_term_color_scheme.t option)
  =
  let bg_rgb =
    Option.map bg ~f:(fun color_scheme ->
      let%tydi { r; g; b } =
        Bonsai_term_color_scheme.to_color_value flavor color_scheme
        |> Bonsai_term_color_scheme.Color_value.to_approximate_rgb
      in
      r, g, b)
  in
  let catppuccin_rgb_values =
    List.map Bonsai_term_color_scheme.all ~f:(fun color ->
      let%tydi { r; g; b } =
        Bonsai_term_color_scheme.to_color_value flavor color
        |> Bonsai_term_color_scheme.Color_value.to_approximate_rgb
      in
      color, (r, g, b))
  in
  let distances =
    List.map catppuccin_rgb_values ~f:(fun (catppuccin_color, (cr, cg, cb)) ->
      let distance = color_distance (r, g, b) (cr, cg, cb) in
      distance, catppuccin_color, (cr, cg, cb))
  in
  let sorted =
    List.sort distances ~compare:(fun (d1, _, _) (d2, _, _) -> Float.compare d1 d2)
  in
  (* Filter out colors that are too close to the background (contrast threshold) *)
  let min_contrast_distance = 150.0 in
  let candidates =
    match bg_rgb with
    | None -> sorted
    | Some (br, bg, bb) ->
      (* Try to find colors with good contrast *)
      let good_contrast =
        List.filter sorted ~f:(fun (_, _, (cr, cg, cb)) ->
          let bg_distance = color_distance (cr, cg, cb) (br, bg, bb) in
          Float.(bg_distance >= min_contrast_distance))
      in
      if List.is_empty good_contrast then sorted else good_contrast
  in
  match candidates with
  | (_, closest_color, _) :: _ -> closest_color
  | [] -> Bonsai_term_color_scheme.Text (* fallback *)
;;

(* Convert RGB to Catppuccin color *)
let convert_rgb_to_catppuccin r g b ~flavor ~bg =
  let closest_catppuccin = find_closest_catppuccin_color (r, g, b) ~flavor ~bg in
  Bonsai_term_color_scheme.color ~flavor closest_catppuccin
;;

(* Convert ansi_text Color to Catppuccin color *)
let sgr8_to_catppuccin (sgr8 : Ansi_text.Color.Sgr8.t) ~bright
  : Bonsai_term_color_scheme.t
  =
  match sgr8, bright with
  | Black, false -> Surface0
  | Black, true -> Surface2
  | Red, false -> Red
  | Red, true -> Maroon
  | Green, false -> Green
  | Green, true -> Teal
  | Yellow, false -> Yellow
  | Yellow, true -> Peach
  | Blue, false -> Blue
  | Blue, true -> Sapphire
  | Magenta, false -> Mauve
  | Magenta, true -> Pink
  | Cyan, false -> Sky
  | Cyan, true -> Teal
  | White, false -> Text
  | White, true -> Rosewater
;;

let convert_ansi_color_to_catppuccin
  ~(color_kind : Color_kind.t)
  ~(color : Ansi_text.Color.t)
  ~(flavor : Bonsai_term_color_scheme.Flavor.t)
  ~(bg : Bonsai_term_color_scheme.t option)
  : Attr.Color.t
  =
  match color with
  | Default ->
    Bonsai_term_color_scheme.color
      ~flavor
      (match color_kind with
       | Fg -> Text
       | Bg -> Crust)
  | Standard sgr8 ->
    Bonsai_term_color_scheme.color ~flavor (sgr8_to_catppuccin sgr8 ~bright:false)
  | Bright sgr8 ->
    Bonsai_term_color_scheme.color ~flavor (sgr8_to_catppuccin sgr8 ~bright:true)
  | Rgb6 _ | Gray24 _ | Rgb256 _ ->
    (match ansi_color_to_rgb color with
     | Some (r, g, b) ->
       let exact_match =
         List.find Bonsai_term_color_scheme.all ~f:(fun c ->
           [%equal: Bonsai_term_color_scheme.Rgb.t]
             (Bonsai_term_color_scheme.to_rgb flavor c)
             { r; g; b })
       in
       (match exact_match with
        | Some c -> Bonsai_term_color_scheme.color ~flavor c
        | None -> convert_rgb_to_catppuccin r g b ~flavor ~bg)
     | None -> Bonsai_term_color_scheme.color ~flavor Text)
;;
