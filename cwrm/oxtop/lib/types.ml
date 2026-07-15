open! Core

module Process = struct
  type t =
    { pid : int
    ; cpu_pct : float
    ; mem_pct : float
    ; rss_kb : int
    ; command : string
    }
  [@@deriving sexp, equal, fields ~getters]
end

module Loadavg = struct
  type t =
    { one : float
    ; five : float
    ; fifteen : float
    }
  [@@deriving sexp, equal]
end

module Memory = struct
  type t =
    { total_bytes : int64
    ; used_bytes : int64
    }
  [@@deriving sexp, equal]

  let used_fraction { total_bytes; used_bytes } =
    if Int64.(total_bytes <= 0L)
    then 0.
    else Int64.to_float used_bytes /. Int64.to_float total_bytes
  ;;
end

module Snapshot = struct
  type t =
    { hostname : string
    ; ncpu : int
    ; loadavg : Loadavg.t
    ; memory : Memory.t
    ; cpu_pct : float
    (** Overall CPU utilization in [0., 100. * float ncpu] (sum of per-process
        percentages, clamped for display). *)
    ; cpu_history : float list
    (** Recent overall CPU percentages (0–100 normalized by ncpu), oldest first. *)
    ; processes : Process.t list
    }
  [@@deriving sexp, equal]
end

module Sort_by = struct
  type t =
    | Cpu
    | Mem
    | Pid
    | Name
  [@@deriving sexp, equal, enumerate, compare]

  let to_string = function
    | Cpu -> "cpu"
    | Mem -> "mem"
    | Pid -> "pid"
    | Name -> "name"
  ;;

  let cycle = function
    | Cpu -> Mem
    | Mem -> Pid
    | Pid -> Name
    | Name -> Cpu
  ;;
end

module Ui_state = struct
  type t =
    { selected : int
    ; sort_by : Sort_by.t
    ; scroll_offset : int
    }
  [@@deriving sexp, equal]

  let initial = { selected = 0; sort_by = Cpu; scroll_offset = 0 }

  type action =
    | Select_next
    | Select_prev
    | Page_down
    | Page_up
    | Goto_top
    | Goto_bottom
    | Set_sort of Sort_by.t
    | Cycle_sort
    | Clamp_to of { num_processes : int; visible_rows : int }
  [@@deriving sexp]

  let clamp_selected selected ~num_processes =
    if num_processes <= 0 then 0 else Int.clamp_exn selected ~min:0 ~max:(num_processes - 1)
  ;;

  let ensure_selected_visible t ~visible_rows ~num_processes =
    let selected = clamp_selected t.selected ~num_processes in
    let max_offset = Int.max 0 (num_processes - visible_rows) in
    let scroll_offset =
      if selected < t.scroll_offset
      then selected
      else if selected >= t.scroll_offset + visible_rows
      then selected - visible_rows + 1
      else t.scroll_offset
    in
    { t with
      selected
    ; scroll_offset = Int.clamp_exn scroll_offset ~min:0 ~max:max_offset
    }
  ;;

  let apply t action ~num_processes ~visible_rows =
    let t =
      match action with
      | Select_next -> { t with selected = t.selected + 1 }
      | Select_prev -> { t with selected = t.selected - 1 }
      | Page_down -> { t with selected = t.selected + visible_rows }
      | Page_up -> { t with selected = t.selected - visible_rows }
      | Goto_top -> { t with selected = 0; scroll_offset = 0 }
      | Goto_bottom -> { t with selected = Int.max 0 (num_processes - 1) }
      | Set_sort sort_by -> { sort_by; selected = 0; scroll_offset = 0 }
      | Cycle_sort ->
        { sort_by = Sort_by.cycle t.sort_by; selected = 0; scroll_offset = 0 }
      | Clamp_to { num_processes = n; visible_rows = v } ->
        ensure_selected_visible t ~visible_rows:v ~num_processes:n
    in
    ensure_selected_visible t ~visible_rows ~num_processes
  ;;
end
