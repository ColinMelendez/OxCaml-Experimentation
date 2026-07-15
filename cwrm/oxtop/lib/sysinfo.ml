open! Core
open Async

let history_length = 40

let run_lines ~prog ~args =
  let%map.Deferred result = Process.run_lines ~prog ~args () in
  match result with
  | Ok lines -> lines
  | Error _ -> []
;;

let run_stdout ~prog ~args =
  let%map.Deferred result = Process.run ~prog ~args () in
  match result with
  | Ok stdout -> String.strip stdout
  | Error _ -> ""
;;

let parse_ps_line line =
  match
    String.split line ~on:' '
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  with
  | pid :: cpu :: mem :: rss :: command_parts ->
    (match
       ( Int.of_string_opt pid
       , Float.of_string_opt cpu
       , Float.of_string_opt mem
       , Int.of_string_opt rss )
     with
     | Some pid, Some cpu_pct, Some mem_pct, Some rss_kb ->
       let command =
         match command_parts with
         | [] -> "?"
         | parts ->
           let joined = String.concat ~sep:" " parts in
           (match String.rsplit2 joined ~on:'/' with
            | None -> joined
            | Some (_, base) -> base)
       in
       Some { Types.Process.pid; cpu_pct; mem_pct; rss_kb; command }
     | _ -> None)
  | _ -> None
;;

let processes () =
  let%map.Deferred lines =
    run_lines ~prog:"ps" ~args:[ "-axo"; "pid=,pcpu=,pmem=,rss=,comm=" ]
  in
  List.filter_map lines ~f:parse_ps_line
;;

let loadavg_from_proc () : Types.Loadavg.t option =
  match Sys_unix.file_exists "/proc/loadavg" with
  | `Yes ->
    (match In_channel.read_lines "/proc/loadavg" with
     | line :: _ ->
       (match
          String.split line ~on:' '
          |> List.filter ~f:(fun s -> not (String.is_empty s))
        with
        | one :: five :: fifteen :: _ ->
          Some
            { one = Float.of_string one
            ; five = Float.of_string five
            ; fifteen = Float.of_string fifteen
            }
        | _ -> None)
     | [] -> None)
  | `No | `Unknown -> None
;;

let loadavg_from_sysctl () =
  let%map.Deferred stdout = run_stdout ~prog:"sysctl" ~args:[ "-n"; "vm.loadavg" ] in
  (* macOS: "{ 1.2 1.3 1.4 }" *)
  let cleaned =
    String.filter stdout ~f:(fun c -> Char.is_digit c || Char.equal c '.' || Char.equal c ' ')
  in
  match
    String.split cleaned ~on:' '
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  with
  | one :: five :: fifteen :: _ ->
    { Types.Loadavg.one = Float.of_string one
    ; five = Float.of_string five
    ; fifteen = Float.of_string fifteen
    }
  | _ -> { one = 0.; five = 0.; fifteen = 0. }
;;

let loadavg () =
  match loadavg_from_proc () with
  | Some loadavg -> return loadavg
  | None -> loadavg_from_sysctl ()
;;

let ncpu () =
  match Core_unix.sysconf NPROCESSORS_ONLN with
  | Some n when Int64.(n > 0L) -> Int64.to_int_exn n
  | _ -> 1
;;

let memory_linux () : Types.Memory.t option =
  match Sys_unix.file_exists "/proc/meminfo" with
  | `Yes ->
    let lines = In_channel.read_lines "/proc/meminfo" in
    let find key =
      List.find_map lines ~f:(fun line ->
        match String.lsplit2 line ~on:':' with
        | Some (k, rest) when String.equal (String.strip k) key ->
          let rest = String.strip rest in
          let num =
            String.split rest ~on:' '
            |> List.filter ~f:(fun s -> not (String.is_empty s))
            |> List.hd
          in
          Option.bind num ~f:Int64.of_string_opt
          |> Option.map ~f:(fun kb -> Int64.(kb * 1024L))
        | _ -> None)
    in
    (match find "MemTotal", find "MemAvailable" with
     | Some total_bytes, Some available ->
       Some { total_bytes; used_bytes = Int64.(total_bytes - available) }
     | _ -> None)
  | `No | `Unknown -> None
;;

let memory_darwin () =
  let%bind.Deferred total_s = run_stdout ~prog:"sysctl" ~args:[ "-n"; "hw.memsize" ] in
  let%map.Deferred vm_stat = run_stdout ~prog:"vm_stat" ~args:[] in
  let total = Int64.of_string_opt (String.strip total_s) in
  let lines = String.split_lines vm_stat in
  let page_size =
    match lines with
    | header :: _ ->
      (match String.lsplit2 header ~on:'(' with
       | Some (_, rest) ->
         String.split rest ~on:' '
         |> List.find_map ~f:Int.of_string_opt
         |> Option.value ~default:4096
       | None -> 4096)
    | [] -> 4096
  in
  let find_pages label =
    List.find_map lines ~f:(fun line ->
      if String.is_prefix line ~prefix:label
      then (
        let rest =
          String.chop_prefix_if_exists line ~prefix:label
          |> String.strip
          |> String.chop_suffix_if_exists ~suffix:"."
          |> String.strip
        in
        Int.of_string_opt (String.filter rest ~f:Char.is_digit))
      else None)
  in
  match total with
  | None -> None
  | Some total_bytes ->
    let free = Option.value (find_pages "Pages free:") ~default:0 in
    let speculative = Option.value (find_pages "Pages speculative:") ~default:0 in
    let free_pages = free + speculative in
    let free_bytes = Int64.(of_int free_pages * of_int page_size) in
    Some { Types.Memory.total_bytes; used_bytes = Int64.(total_bytes - free_bytes) }
;;

let memory () =
  match memory_linux () with
  | Some m -> return m
  | None ->
    let%map.Deferred m = memory_darwin () in
    Option.value m ~default:{ Types.Memory.total_bytes = 0L; used_bytes = 0L }
;;

let overall_cpu_pct ~(processes : Types.Process.t list) ~ncpu =
  let sum = List.sum (module Float) processes ~f:Types.Process.cpu_pct in
  let ncpu = Float.of_int (Int.max 1 ncpu) in
  Float.clamp_exn (sum /. ncpu) ~min:0. ~max:100.
;;

let push_history history value =
  let history = history @ [ value ] in
  if List.length history <= history_length
  then history
  else List.drop history (List.length history - history_length)
;;

let collect ~(prev_history : float list) () : Types.Snapshot.t Deferred.t =
  let%bind.Deferred processes = processes () in
  let%bind.Deferred loadavg = loadavg () in
  let%map.Deferred memory = memory () in
  let ncpu = ncpu () in
  let cpu_pct = overall_cpu_pct ~processes ~ncpu in
  { Types.Snapshot.hostname = Core_unix.gethostname ()
  ; ncpu
  ; loadavg
  ; memory
  ; cpu_pct
  ; cpu_history = push_history prev_history cpu_pct
  ; processes
  }
;;
