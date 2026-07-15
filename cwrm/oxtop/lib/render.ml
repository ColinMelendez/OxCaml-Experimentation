open! Core
open Bonsai_term

module Color = Bonsai_term_color_scheme

let flavor = Color.Mocha.flavor
let color c = Color.color ~flavor c
let fg c = Attr.fg (color c)

let format_bytes (bytes : int64) =
  let open Float.O in
  let b = Int64.to_float bytes in
  if b >= 1024. * 1024. * 1024.
  then sprintf "%.1fG" (b / (1024. * 1024. * 1024.))
  else if b >= 1024. * 1024.
  then sprintf "%.0fM" (b / (1024. * 1024.))
  else if b >= 1024.
  then sprintf "%.0fK" (b / 1024.)
  else sprintf "%LdB" bytes
;;

let format_rss_kb rss_kb = format_bytes Int64.(of_int rss_kb * 1024L)

let usage_bar ~width ~(fraction : float) ~fill_color =
  let width = Int.max 1 width in
  let fraction = Float.clamp_exn fraction ~min:0. ~max:1. in
  let filled = Int.of_float (Float.round_nearest (fraction *. Float.of_int width)) in
  let filled = Int.clamp_exn filled ~min:0 ~max:width in
  let repeat s n = String.concat (List.init n ~f:(fun _ -> s)) in
  View.hcat
    [ View.text ~attrs:[ fg fill_color ] (repeat "█" filled)
    ; View.text ~attrs:[ fg Color.Overlay0 ] (repeat "░" (width - filled))
    ]
;;

let sparkline values ~width =
  let chars = [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |] in
  let values =
    let len = List.length values in
    if len > width then List.drop values (len - width) else values
  in
  if List.is_empty values
  then View.text ~attrs:[ fg Color.Overlay1 ] (String.make width ' ')
  else (
    let max_v = List.fold values ~init:1. ~f:Float.max in
    let points =
      List.map values ~f:(fun v ->
        let idx =
          if Float.(max_v <= 0.)
          then 0
          else
            Int.clamp_exn
              (Int.of_float (Float.round_nearest (v /. max_v *. 7.)))
              ~min:0
              ~max:7
        in
        View.text ~attrs:[ fg Color.Sapphire ] chars.(idx))
    in
    let pad = Int.max 0 (width - List.length values) in
    View.hcat (points @ [ View.text (String.make pad ' ') ]))
;;

let sorted_processes (snapshot : Types.Snapshot.t) ~(sort_by : Types.Sort_by.t) =
  let procs = snapshot.processes in
  match sort_by with
  | Cpu -> List.sort procs ~compare:(fun a b -> Float.compare b.cpu_pct a.cpu_pct)
  | Mem -> List.sort procs ~compare:(fun a b -> Float.compare b.mem_pct a.mem_pct)
  | Pid -> List.sort procs ~compare:(fun a b -> Int.compare a.pid b.pid)
  | Name -> List.sort procs ~compare:(fun a b -> String.compare a.command b.command)
;;

let header_line (snapshot : Types.Snapshot.t) ~width =
  let { Types.Loadavg.one; five; fifteen } = snapshot.loadavg in
  let text =
    sprintf
      " oxtop  %s  ·  %d cpu  ·  load %0.2f %0.2f %0.2f "
      snapshot.hostname
      snapshot.ncpu
      one
      five
      fifteen
  in
  let text =
    if String.length text > width
    then String.prefix text width
    else text ^ String.make (Int.max 0 (width - String.length text)) ' '
  in
  View.text ~attrs:[ Attr.bold; fg Color.Text; Attr.bg (color Color.Surface0) ] text
;;

let resource_panel (snapshot : Types.Snapshot.t) ~width =
  let bar_width = Int.max 8 (width - 28) in
  let mem_frac = Types.Memory.used_fraction snapshot.memory in
  let cpu_frac = snapshot.cpu_pct /. 100. in
  let cpu_line =
    View.hcat
      [ View.text ~attrs:[ Attr.bold; fg Color.Green ] "CPU "
      ; usage_bar ~width:bar_width ~fraction:cpu_frac ~fill_color:Color.Green
      ; View.text ~attrs:[ fg Color.Subtext0 ] (sprintf " %5.1f%%" snapshot.cpu_pct)
      ]
  in
  let mem_line =
    View.hcat
      [ View.text ~attrs:[ Attr.bold; fg Color.Yellow ] "MEM "
      ; usage_bar ~width:bar_width ~fraction:mem_frac ~fill_color:Color.Yellow
      ; View.text
          ~attrs:[ fg Color.Subtext0 ]
          (sprintf
             " %s/%s"
             (format_bytes snapshot.memory.used_bytes)
             (format_bytes snapshot.memory.total_bytes))
      ]
  in
  let history =
    View.hcat
      [ View.text ~attrs:[ fg Color.Overlay1 ] "hst "
      ; sparkline snapshot.cpu_history ~width:(Int.max 10 (width - 5))
      ]
  in
  View.vcat [ cpu_line; mem_line; history ]
;;

let process_header ~width =
  let line = sprintf "%7s %6s %6s %8s  %s" "PID" "CPU%" "MEM%" "RSS" "COMMAND" in
  let line = if String.length line > width then String.prefix line width else line in
  View.text ~attrs:[ Attr.bold; fg Color.Lavender ] line
;;

let process_row ~(selected : bool) ~(width : int) (proc : Types.Process.t) =
  let marker = if selected then ">" else " " in
  let line =
    sprintf
      "%s%6d %6.1f %6.1f %8s  %s"
      marker
      proc.pid
      proc.cpu_pct
      proc.mem_pct
      (format_rss_kb proc.rss_kb)
      proc.command
  in
  let line =
    if String.length line > width
    then String.prefix line width
    else line ^ String.make (Int.max 0 (width - String.length line)) ' '
  in
  if selected
  then View.text ~attrs:[ Attr.bold; fg Color.Green ] line
  else View.text ~attrs:[ fg Color.Text ] line
;;

let process_table
  ~(snapshot : Types.Snapshot.t)
  ~(ui : Types.Ui_state.t)
  ~width
  ~height
  =
  (* Border box adds 2 columns and 2 rows. *)
  let inner_width = Int.max 10 (width - 2) in
  let inner_height = Int.max 2 (height - 2) in
  let procs = sorted_processes snapshot ~sort_by:ui.sort_by in
  let header = process_header ~width:inner_width in
  let body_height = Int.max 0 (inner_height - 1) in
  let visible = List.drop procs ui.scroll_offset |> fun xs -> List.take xs body_height in
  let rows =
    List.mapi visible ~f:(fun i proc ->
      let absolute = ui.scroll_offset + i in
      process_row ~selected:(absolute = ui.selected) ~width:inner_width proc)
  in
  let padding =
    let missing = body_height - List.length rows in
    List.init (Int.max 0 missing) ~f:(fun _ -> View.text (String.make inner_width ' '))
  in
  let title = sprintf "processes · sort=%s" (Types.Sort_by.to_string ui.sort_by) in
  Bonsai_term_border_box.view
    ~line_type:Round_corners
    ~title
    ~title_attrs:[ fg Color.Mauve; Attr.bold ]
    (View.vcat (header :: (rows @ padding)))
;;

let footer ~width =
  let text = " q:quit  j/k:move  g/G:top/bot  c/m/p/n:sort  s:cycle " in
  let text =
    if String.length text > width
    then String.prefix text width
    else text ^ String.make (Int.max 0 (width - String.length text)) ' '
  in
  View.text ~attrs:[ fg Color.Overlay1; Attr.bg (color Color.Surface0) ] text
;;

type layout =
  { header : int
  ; resources : int
  ; process_total : int
  ; footer : int
  }

let layout_heights ~total_height =
  let header = 1 in
  let resources = 3 in
  let footer = 1 in
  let process_total = Int.max 5 (total_height - header - resources - footer) in
  { header; resources; process_total; footer }
;;

let view
  ~(snapshot : Types.Snapshot.t)
  ~(ui : Types.Ui_state.t)
  ~(dimensions : Dimensions.t)
  =
  let { Dimensions.width; height } = dimensions in
  let width = Int.max 20 width in
  let height = Int.max 10 height in
  let { process_total; _ } = layout_heights ~total_height:height in
  let body =
    View.vcat
      [ header_line snapshot ~width
      ; resource_panel snapshot ~width
      ; process_table ~snapshot ~ui ~width ~height:process_total
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
