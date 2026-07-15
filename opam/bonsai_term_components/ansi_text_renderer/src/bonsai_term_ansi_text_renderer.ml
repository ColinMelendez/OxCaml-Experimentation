open! Core
open! Bonsai_term

let convert_color_directly ~(color : Ansi_text.Color.t) =
  match color with
  | Default -> Attr.Color.Expert.default
  | Standard sgr8 ->
    (match sgr8 with
     | Black -> Attr.Color.Expert.black
     | Red -> Attr.Color.Expert.red
     | Green -> Attr.Color.Expert.green
     | Yellow -> Attr.Color.Expert.yellow
     | Blue -> Attr.Color.Expert.blue
     | Magenta -> Attr.Color.Expert.magenta
     | Cyan -> Attr.Color.Expert.cyan
     | White -> Attr.Color.Expert.white)
  | Bright sgr8 ->
    (match sgr8 with
     | Black -> Attr.Color.Expert.lightblack
     | Red -> Attr.Color.Expert.lightred
     | Green -> Attr.Color.Expert.lightgreen
     | Yellow -> Attr.Color.Expert.lightyellow
     | Blue -> Attr.Color.Expert.lightblue
     | Magenta -> Attr.Color.Expert.lightmagenta
     | Cyan -> Attr.Color.Expert.lightcyan
     | White -> Attr.Color.Expert.lightwhite)
  | Rgb6 rgb6 ->
    let r, g, b = Ansi_text.Color.Rgb6.to_rgb rgb6 in
    (* Convert from 0-5 range to 0-255 range *)
    let scale x = x * 255 / 5 in
    Attr.Color.rgb ~r:(scale r) ~g:(scale g) ~b:(scale b)
  | Gray24 gray24 ->
    let level = Ansi_text.Color.Gray24.to_level gray24 in
    (* Convert from 0-23 range to 0-255 range *)
    let gray_value = level * 255 / 23 in
    Attr.Color.rgb ~r:gray_value ~g:gray_value ~b:gray_value
  | Rgb256 rgb256 ->
    let r, g, b = Ansi_text.Color.Rgb256.to_rgb rgb256 in
    Attr.Color.rgb ~r ~g ~b
;;

(* Convert ansi_text Color to bonsai_term Attr.Color *)
let convert_color ~color_kind ~flavor ~bg (color : Ansi_text.Color.t)
  : Attr.Color.t option
  =
  match flavor with
  | None -> Some (convert_color_directly ~color)
  | Some flavor ->
    Some
      (Catppuccin_mapping.convert_ansi_color_to_catppuccin ~color_kind ~color ~flavor ~bg)
;;

(* Convert ansi_text Attr to bonsai_term Attr *)
let convert_attr ~flavor ~bg (attr : Ansi_text.Attr.t) : Attr.t option =
  match attr with
  | Reset -> None (* Reset doesn't translate directly *)
  | Bold -> Some Attr.bold
  | Italic -> Some Attr.italic
  | Underline -> Some Attr.underline
  | Blink -> Some Attr.blink
  | Invert -> Some Attr.invert
  | Fg color -> Option.map (convert_color ~color_kind:Fg ~flavor ~bg color) ~f:Attr.fg
  | Bg color -> Option.map (convert_color ~color_kind:Bg ~flavor ~bg color) ~f:Attr.bg
  | Faint
  | Fast_blink
  | Double_ul
  | Hide
  | Strike
  | Overline
  | Normal_weight
  | Not_emphasis
  | Not_underline
  | Not_blink
  | Not_invert
  | Not_hide
  | Not_strike
  | Not_overline
  | Ul_color _
  | Variable_width
  | Fixed_width
  | Framed
  | Encircled
  | Not_framed
  | Superscript
  | Subscript
  | Not_script
  | Fraktur
  | Font _
  | Ideogram _
  | Other _ -> None (* These don't have direct equivalents in bonsai_term *)
;;

module Fill_width = struct
  type t =
    | No
    | Yes of { fill_width : int }
end

(* Convert ansi_text Style to bonsai_term Attr list *)
let convert_style ~current_attrs ~flavor ~bg (style : Ansi_text.Style.t) : Attr.t list =
  let out = Vec.of_list current_attrs in
  List.iter style ~f:(fun attr ->
    match attr with
    | Reset -> Vec.clear out
    | attr ->
      Option.iter (convert_attr ~flavor ~bg attr) ~f:(fun attr -> Vec.push_back out attr));
  Vec.to_list out
;;

let string_display_width (s : string) : int =
  let uchars = String.Utf8.of_string s |> String.Utf8.to_list in
  List.fold uchars ~init:0 ~f:(fun acc uchar -> acc + View.uchar_tty_width uchar)
;;

(* Convert ansi_text to bonsai_term View with optional Catppuccin theming *)
let render ?bg ?flavor ?(fill_width = Fill_width.No) (ansi_text : Ansi_text.t) : View.t =
  (* First, we need to process the ansi_text and group elements by lines *)
  let with_bg attrs =
    match #(bg, flavor) with
    | #(None, _) | #(_, None) -> attrs
    | #(Some bg, Some flavor) ->
      [ Attr.bg (Bonsai_term_color_scheme.color ~flavor bg) ] @ attrs
  in
  let commit_line current_line current_attrs current_width =
    let line = List.rev current_line in
    match fill_width with
    | No -> line
    | Yes { fill_width } ->
      if current_width < fill_width
      then (
        let padding_width = fill_width - current_width in
        let padding = String.make padding_width ' ' in
        let padding_view = View.text ~attrs:(with_bg current_attrs) padding in
        line @ [ padding_view ])
      else line
  in
  let rec process_elements elements current_attrs acc_lines current_line current_width =
    match elements with
    | [] ->
      let final_lines =
        if List.is_empty current_line
        then List.rev acc_lines
        else List.rev (commit_line current_line current_attrs current_width :: acc_lines)
      in
      final_lines
    | element :: rest ->
      (match element with
       | `Text text ->
         let text_str = Ansi_text.Text.to_string text in
         (* Split text on newlines using a more explicit approach *)
         let parts = String.split_on_chars text_str ~on:[ '\n' ] in
         (match parts with
          | [] -> process_elements rest current_attrs acc_lines current_line current_width
          | [ single_part ] ->
            (* No newlines in this text, add to current line *)
            if String.is_empty single_part
            then process_elements rest current_attrs acc_lines current_line current_width
            else (
              let text_view = View.text ~attrs:(with_bg current_attrs) single_part in
              let new_width = current_width + string_display_width single_part in
              process_elements
                rest
                current_attrs
                acc_lines
                (text_view :: current_line)
                new_width)
          | first_part :: remaining_parts ->
            (* Text contains newlines *)
            let first_text_view =
              if String.is_empty first_part
              then View.text ~attrs:(with_bg current_attrs) ""
              else View.text ~attrs:(with_bg current_attrs) first_part
            in
            let first_part_width = current_width + string_display_width first_part in
            let completed_first_line =
              commit_line (first_text_view :: current_line) current_attrs first_part_width
            in
            (* Process all remaining parts except the last one (each becomes its own
               complete line) *)
            let middle_parts, last_part =
              match List.rev remaining_parts with
              | [] -> [], ""
              | last :: middle_rev -> List.rev middle_rev, last
            in
            let middle_line_views =
              List.map middle_parts ~f:(fun part ->
                let part_width = string_display_width part in
                let line = [ View.text ~attrs:(with_bg current_attrs) part ] in
                commit_line line current_attrs part_width)
            in
            (* Handle the last part - if it's empty, it means we had a trailing newline *)
            let new_current_line, new_width =
              if String.is_empty last_part
              then [], 0
              else
                ( [ View.text ~attrs:(with_bg current_attrs) last_part ]
                , string_display_width last_part )
            in
            let new_acc_lines =
              List.rev_append middle_line_views (completed_first_line :: acc_lines)
            in
            process_elements rest current_attrs new_acc_lines new_current_line new_width)
       | `Style style ->
         let new_attrs = convert_style ~current_attrs ~flavor ~bg style in
         process_elements rest new_attrs acc_lines current_line current_width
       | `Control _ | `Hyperlink _ | #Ansi_text.Ansi.emulation | `Unknown _ ->
         (* Skip control sequences for now *)
         process_elements rest current_attrs acc_lines current_line current_width)
  in
  let lines = process_elements ansi_text [] [] [] 0 in
  (* Convert each line (list of views) to a single view using hcat, then combine all lines
     with vcat *)
  let line_views = List.map lines ~f:(fun line_elements -> View.hcat line_elements) in
  (* Set a default fg color so that if text has no ansi color specified, we still give it
     a reasonable default color *)
  let default_fg =
    Option.map flavor ~f:(fun flavor -> Bonsai_term_color_scheme.color ~flavor Text)
  in
  View.vcat line_views |> View.with_colors' ?fg:default_fg
;;
