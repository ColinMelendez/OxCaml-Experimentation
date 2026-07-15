open! Core
open Bonsai_term

module Line_type = struct
  type t =
    | Thin
    | Thick
    | Double
    | Round_corners

  let vertical_bar = function
    | Thin | Round_corners -> "│"
    | Thick -> "┃"
    | Double -> "║"
  ;;

  let horizontal_bar = function
    | Thin | Round_corners -> "─"
    | Thick -> "━"
    | Double -> "═"
  ;;

  let top_left = function
    | Thin -> "┌"
    | Thick -> "┏"
    | Double -> "╔"
    | Round_corners -> "╭"
  ;;

  let bottom_left = function
    | Thin -> "└"
    | Thick -> "┗"
    | Double -> "╚"
    | Round_corners -> "╰"
  ;;

  let bottom_right = function
    | Thin -> "┘"
    | Thick -> "┛"
    | Double -> "╝"
    | Round_corners -> "╯"
  ;;

  let top_right = function
    | Thin -> "┐"
    | Thick -> "┓"
    | Double -> "╗"
    | Round_corners -> "╮"
  ;;

  let vertical_right = function
    | Thin | Round_corners -> "├"
    | Thick -> "┣"
    | Double -> "╠"
  ;;

  let vertical_left = function
    | Thin | Round_corners -> "┤"
    | Thick -> "┫"
    | Double -> "╣"
  ;;

  let horizontal_down = function
    | Thin | Round_corners -> "┬"
    | Thick -> "┳"
    | Double -> "╦"
  ;;

  let horizontal_up = function
    | Thin | Round_corners -> "┴"
    | Thick -> "┻"
    | Double -> "╩"
  ;;
end

let view'
  ?(line_type = Line_type.Thin)
  ?(hide_left = false)
  ?(hide_right = false)
  ?(hide_top = false)
  ?(hide_bottom = false)
  ?(left_padding = 0)
  ?(right_padding = 0)
  ?(top_padding = 0)
  ?(bottom_padding = 0)
  ?title
  ?subtitle
  ?(attrs = [])
  content
  =
  let get_corner_view hide_side_1 hide_side_2 char_if_shown =
    if hide_side_1 || hide_side_2 then View.none else View.text ~attrs char_if_shown
  in
  let { Dimensions.height; width } = View.dimensions content in
  let width = width + left_padding + right_padding in
  let height = height + top_padding + bottom_padding in
  let hbar = Line_type.horizontal_bar line_type in
  let vbar = Line_type.vertical_bar line_type in
  let horizontal_border = List.create ~len:width (View.text ~attrs hbar) |> View.hcat in
  let vertical_border = List.create ~len:height (View.text ~attrs vbar) |> View.vcat in
  let top_border =
    if hide_top
    then View.none
    else (
      match title with
      | None ->
        View.hcat
          [ get_corner_view hide_left hide_top (Line_type.top_left line_type)
          ; horizontal_border
          ; get_corner_view hide_right hide_top (Line_type.top_right line_type)
          ]
      | Some title ->
        let rest_length = width - View.width title in
        (match Int.sign rest_length with
         | Neg ->
           View.hcat
             [ get_corner_view hide_left hide_top (Line_type.top_left line_type); title ]
         | Zero ->
           View.hcat
             [ get_corner_view hide_left hide_top (Line_type.top_left line_type)
             ; title
             ; get_corner_view hide_right hide_top (Line_type.top_right line_type)
             ]
         | Pos ->
           let rest_line =
             List.create ~len:(Int.max 0 rest_length) hbar |> String.concat
           in
           View.hcat
             [ get_corner_view hide_left hide_top (Line_type.top_left line_type)
             ; title
             ; View.text ~attrs rest_line
             ; get_corner_view hide_right hide_top (Line_type.top_right line_type)
             ]))
  in
  let top_border =
    match subtitle with
    | None -> top_border
    | Some subtitle ->
      let len = View.width subtitle in
      View.zcat
        [ View.pad ~l:(Int.max 0 (View.width top_border - len - 2)) subtitle; top_border ]
  in
  let bottom_border =
    if hide_bottom
    then View.none
    else
      View.hcat
        [ get_corner_view hide_left hide_bottom (Line_type.bottom_left line_type)
        ; horizontal_border
        ; get_corner_view hide_right hide_bottom (Line_type.bottom_right line_type)
        ]
  in
  let left_border = if hide_left then View.none else vertical_border in
  let right_border = if hide_right then View.none else vertical_border in
  let rectangle = View.rectangle ~attrs ~width ~height () in
  let content_with_padding =
    View.zcat
      [ View.pad ~l:left_padding ~r:right_padding ~t:top_padding ~b:bottom_padding content
      ; rectangle
      ]
  in
  View.vcat
    [ top_border
    ; View.hcat [ left_border; content_with_padding; right_border ]
    ; bottom_border
    ]
;;

let view
  ?(line_type = Line_type.Thin)
  ?(hide_left = false)
  ?(hide_right = false)
  ?(hide_top = false)
  ?(hide_bottom = false)
  ?(left_padding = 0)
  ?(right_padding = 0)
  ?(top_padding = 0)
  ?(bottom_padding = 0)
  ?title
  ?(title_attrs = [])
  ?subtitle
  ?(subtitle_attrs = [])
  ?(attrs = [])
  content
  =
  view'
    ~line_type
    ~hide_left
    ~hide_right
    ~hide_top
    ~hide_bottom
    ~left_padding
    ~right_padding
    ~top_padding
    ~bottom_padding
    ?title:
      (Option.map title ~f:(fun title -> View.text ~attrs:title_attrs (" " ^ title ^ " ")))
    ?subtitle:
      (Option.map subtitle ~f:(fun subtitle ->
         View.text ~attrs:subtitle_attrs (" " ^ subtitle ^ " ")))
    ~attrs
    content
;;
