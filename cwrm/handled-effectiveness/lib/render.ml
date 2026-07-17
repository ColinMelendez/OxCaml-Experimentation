open! Core
open Bonsai_term
open Types

module Color = Bonsai_term_color_scheme

let flavor = Color.Mocha.flavor
let color c = Color.color ~flavor c
let fg c = Attr.fg (color c)

let pad_or_crop line ~width =
  let len = String.length line in
  if len > width
  then String.prefix line width
  else line ^ String.make (Int.max 0 (width - len)) ' '
;;

let header ~(demo : Demo.t) ~(ui : Ui_state.t) ~width =
  let play = if ui.playing then "[play]" else "[stop]" in
  let text =
    sprintf
      " handled-effectiveness  -  %s  -  step %d/%d  %s "
      demo.id
      (ui.step + 1)
      (Demo.num_steps demo)
      play
  in
  View.text
    ~attrs:[ Attr.bold; fg Color.Text; Attr.bg (color Color.Surface0) ]
    (pad_or_crop text ~width)
;;

let blurb_box ~(demo : Demo.t) ~width ~height =
  let inner_w = Int.max 8 (width - 2) in
  let inner_h = Int.max 1 (height - 2) in
  let title_line =
    View.text ~attrs:[ Attr.bold; fg Color.Mauve ] (pad_or_crop demo.title ~width:inner_w)
  in
  let words = String.split ~on:' ' demo.blurb in
  let lines =
    let acc = ref [] in
    let cur = ref "" in
    List.iter words ~f:(fun w ->
      let candidate = if String.is_empty !cur then w else !cur ^ " " ^ w in
      if String.length candidate <= inner_w
      then cur := candidate
      else (
        acc := !cur :: !acc;
        cur := w));
    if not (String.is_empty !cur) then acc := !cur :: !acc;
    List.rev !acc
  in
  let body_lines =
    List.take lines (Int.max 0 (inner_h - 1))
    |> List.map ~f:(fun line ->
      View.text ~attrs:[ fg Color.Subtext0 ] (pad_or_crop line ~width:inner_w))
  in
  let pad =
    let missing = inner_h - 1 - List.length body_lines in
    List.init (Int.max 0 missing) ~f:(fun _ -> View.text (String.make inner_w ' '))
  in
  Bonsai_term_border_box.view
    ~line_type:Round_corners
    ~title:"demo"
    ~title_attrs:[ fg Color.Lavender; Attr.bold ]
    (View.vcat (title_line :: (body_lines @ pad)))
;;

let event_box ~(frame : Frame.t) ~width ~height =
  let inner_w = Int.max 8 (width - 2) in
  let inner_h = Int.max 2 (height - 2) in
  let event_line =
    View.text
      ~attrs:[ Attr.bold; fg Color.Green ]
      (pad_or_crop ("> " ^ Types.Event.to_string frame.event) ~width:inner_w)
  in
  let expl =
    View.text
      ~attrs:[ fg Color.Text ]
      (pad_or_crop frame.explanation ~width:inner_w)
  in
  let pad =
    List.init (Int.max 0 (inner_h - 2)) ~f:(fun _ -> View.text (String.make inner_w ' '))
  in
  Bonsai_term_border_box.view
    ~line_type:Round_corners
    ~title:"event"
    ~title_attrs:[ fg Color.Green; Attr.bold ]
    (View.vcat (event_line :: expl :: pad))
;;

let runtime_box ~(frame : Frame.t) ~width ~height =
  let inner_w = Int.max 8 (width - 2) in
  let inner_h = Int.max 2 (height - 2) in
  let lines =
    String.split_lines frame.state_panel
    @
    match frame.running with
    | None when String.is_empty frame.state_panel -> [ "idle" ]
    | _ -> []
  in
  let rows =
    List.take lines inner_h
    |> List.map ~f:(fun line ->
      View.text ~attrs:[ fg Color.Sapphire ] (pad_or_crop line ~width:inner_w))
  in
  let pad =
    List.init
      (Int.max 0 (inner_h - List.length rows))
      ~f:(fun _ -> View.text (String.make inner_w ' '))
  in
  Bonsai_term_border_box.view
    ~line_type:Round_corners
    ~title:"runtime"
    ~title_attrs:[ fg Color.Sapphire; Attr.bold ]
    (View.vcat (rows @ pad))
;;

let console_box ~(frame : Frame.t) ~width ~height =
  let inner_w = Int.max 8 (width - 2) in
  let inner_h = Int.max 2 (height - 2) in
  let shown =
    if String.is_empty frame.console
    then "(empty)"
    else sprintf "%S" frame.console
  in
  let row =
    View.text ~attrs:[ Attr.bold; fg Color.Yellow ] (pad_or_crop shown ~width:inner_w)
  in
  let pad =
    List.init (Int.max 0 (inner_h - 1)) ~f:(fun _ -> View.text (String.make inner_w ' '))
  in
  Bonsai_term_border_box.view
    ~line_type:Round_corners
    ~title:"console"
    ~title_attrs:[ fg Color.Yellow; Attr.bold ]
    (View.vcat (row :: pad))
;;

let timeline_box ~(demo : Demo.t) ~(ui : Ui_state.t) ~width ~height =
  let inner_w = Int.max 8 (width - 2) in
  let inner_h = Int.max 2 (height - 2) in
  let glyphs =
    List.map demo.frames ~f:(fun frame -> Types.Event.glyph frame.event) |> String.concat
  in
  let cursor =
    String.init (String.length glyphs) ~f:(fun i -> if i = ui.step then 'O' else '.')
  in
  let trim s =
    if String.length s <= inner_w
    then pad_or_crop s ~width:inner_w
    else (
      let start = Int.max 0 (ui.step - (inner_w / 2)) in
      let start = Int.min start (String.length s - inner_w) in
      String.sub s ~pos:start ~len:inner_w)
  in
  let g_line = View.text ~attrs:[ fg Color.Overlay1 ] (trim glyphs) in
  let c_line = View.text ~attrs:[ fg Color.Peach; Attr.bold ] (trim cursor) in
  let legend =
    View.text
      ~attrs:[ fg Color.Overlay1 ]
      (pad_or_crop " F fork  Y yield  R resume  S say  G/T get/set  ></ send/recv " ~width:inner_w)
  in
  let rows = [ g_line; c_line; legend ] in
  let pad =
    List.init
      (Int.max 0 (inner_h - List.length rows))
      ~f:(fun _ -> View.text (String.make inner_w ' '))
  in
  Bonsai_term_border_box.view
    ~line_type:Round_corners
    ~title:"timeline"
    ~title_attrs:[ fg Color.Peach; Attr.bold ]
    (View.vcat (rows @ pad))
;;

let footer ~width =
  let text =
    " space/l:step  h:back  p:play  r:reset  [/]:demo  1-4:jump  q:quit "
  in
  View.text
    ~attrs:[ fg Color.Overlay1; Attr.bg (color Color.Surface0) ]
    (pad_or_crop text ~width)
;;

let view ~(demos : Demo.t list) ~(ui : Ui_state.t) ~(dimensions : Dimensions.t) =
  let { Dimensions.width; height } = dimensions in
  let width = Int.max 40 width in
  let height = Int.max 16 height in
  let demo = Demos.by_index demos ui.demo_index in
  let frame = Demo.frame demo ~step:ui.step in
  let header_h = 1 in
  let footer_h = 1 in
  let blurb_h = 5 in
  let event_h = 4 in
  let timeline_h = 5 in
  let mid_h =
    Int.max 4 (height - header_h - footer_h - blurb_h - event_h - timeline_h)
  in
  let left_w = (width * 3) / 5 in
  let right_w = width - left_w in
  let body =
    View.vcat
      [ header ~demo ~ui ~width
      ; blurb_box ~demo ~width ~height:blurb_h
      ; event_box ~frame ~width ~height:event_h
      ; View.hcat
          [ runtime_box ~frame ~width:left_w ~height:mid_h
          ; console_box ~frame ~width:right_w ~height:mid_h
          ]
      ; timeline_box ~demo ~ui ~width ~height:timeline_h
      ; footer ~width
      ]
  in
  let body =
    let h = View.height body in
    if h < height
    then View.vcat [ body; View.rectangle ~width ~height:(height - h) () ]
    else if h > height
    then View.crop ~b:(h - height) body
    else body
  in
  let w = View.width body in
  if w < width
  then View.hcat [ body; View.rectangle ~width:(width - w) ~height () ]
  else if w > width
  then View.crop ~r:(w - width) body
  else body
;;
