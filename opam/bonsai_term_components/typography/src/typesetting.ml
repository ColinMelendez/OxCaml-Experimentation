open! Core

module Mutable_segment = struct
  type 'attr t =
    { chars : Uchar.t Vec.t
    ; attr : 'attr
    }
  [@@deriving sexp_of]

  let of_string ~attr s =
    let chars = Vec.of_list (String.Utf8.of_string s |> String.Utf8.to_list) in
    { chars; attr }
  ;;

  let to_segment { chars; attr } = { Text.chars = Vec.to_iarray chars; attr }
end

let is_whitespace =
  let space = Uchar.of_char ' ' in
  fun uchar -> Uchar.equal uchar space
;;

module Chunk = struct
  type 'attr t =
    { segments : 'attr Text.t iarray
    ; total_width : int
        (* The "total width" of the string is how wide the string is (including trailing whitespace). *)
    ; visual_width : int
    (* The "visual width" of the string is how wide the strin is (excluding trailing whitespace)*)
    }
  [@@deriving sexp_of]

  let create_of_segments (segments : 'attr Mutable_segment.t Vec.t) =
    (* Compute widths while converting segments, avoiding extra iteration *)
    let total_width = ref 0 in
    let last_non_ws_total_width = ref 0 in
    let segments =
      Iarray.init (Vec.length segments) ~f:(fun i ->
        let seg = Vec.get segments i in
        let seg_total_width = ref 0 in
        let seg_last_non_ws_width = ref 0 in
        Vec.iteri seg.Mutable_segment.chars ~f:(fun _j c ->
          let c_width = Bonsai_term.View.uchar_tty_width c in
          seg_total_width := !seg_total_width + c_width;
          if not (is_whitespace c) then seg_last_non_ws_width := !seg_total_width);
        total_width := !total_width + !seg_total_width;
        if !seg_last_non_ws_width > 0
        then
          last_non_ws_total_width
          := !total_width - !seg_total_width + !seg_last_non_ws_width;
        Mutable_segment.to_segment seg)
    in
    let visual_width = !last_non_ws_total_width in
    { segments; total_width = !total_width; visual_width }
  ;;
end

let chunkenize : 'attr Mutable_segment.t list -> 'attr Chunk.t list =
  fun segments ->
  let out = Vec.create () in
  let current_chunk_segments = Vec.create () in
  let flush_chunk () =
    if Vec.length current_chunk_segments > 0
    then (
      (* Filter out empty segments before creating chunk *)
      let non_empty_segments = Vec.create () in
      Vec.iter current_chunk_segments ~f:(fun seg ->
        if Vec.length seg.Mutable_segment.chars > 0
        then Vec.push_back non_empty_segments seg);
      if Vec.length non_empty_segments > 0
      then (
        let chunk = Chunk.create_of_segments non_empty_segments in
        Vec.push_back out chunk);
      Vec.clear current_chunk_segments)
  in
  let segments_array = Array.of_list segments in
  Array.iteri segments_array ~f:(fun segment_index segment ->
    let chars = segment.Mutable_segment.chars in
    let len = Vec.length chars in
    let is_all_whitespace = Vec.for_all chars ~f:is_whitespace in
    let next_segment_starts_with_non_ws =
      if segment_index + 1 < Array.length segments_array
      then (
        let next_seg = segments_array.(segment_index + 1) in
        let next_chars = next_seg.Mutable_segment.chars in
        Vec.length next_chars > 0 && not (is_whitespace (Vec.get next_chars 0)))
      else false
    in
    match is_all_whitespace with
    | true ->
      (* Keep all-whitespace segments as a single chunk, unless next segment has non-ws *)
      Vec.push_back current_chunk_segments segment;
      if next_segment_starts_with_non_ws then flush_chunk ()
    | false ->
      let start_idx = ref 0 in
      let i = ref 0 in
      while !i < len do
        let c = Vec.get chars !i in
        if !i < !start_idx
        then incr i (* Skip already processed characters *)
        else if is_whitespace c
        then (
          (* Check if there's any non-whitespace after this space in current segment *)
          let has_non_ws_after_in_segment =
            let rec check idx =
              if idx >= len
              then false
              else if not (is_whitespace (Vec.get chars idx))
              then true
              else check (idx + 1)
            in
            check (!i + 1)
          in
          (* Also check if we're at the end and next segment has non-ws *)
          let is_at_end = !i = len - 1 in
          let should_split =
            has_non_ws_after_in_segment || (is_at_end && next_segment_starts_with_non_ws)
          in
          if should_split
          then (
            (* Include the space and all following whitespace until next non-ws *)
            let end_idx = ref !i in
            while !end_idx + 1 < len && is_whitespace (Vec.get chars (!end_idx + 1)) do
              incr end_idx
            done;
            if !start_idx <= !end_idx
            then (
              let sub_chars = Vec.create () in
              for j = !start_idx to !end_idx do
                Vec.push_back sub_chars (Vec.get chars j)
              done;
              if Vec.length sub_chars > 0
              then (
                Vec.push_back
                  current_chunk_segments
                  { chars = sub_chars; attr = segment.attr };
                flush_chunk ()));
            start_idx := !end_idx + 1;
            i := !end_idx + 1)
          else incr i)
        else incr i
      done;
      (* Handle remaining characters after last space *)
      if !start_idx < len
      then (
        let sub_chars = Vec.create () in
        for j = !start_idx to len - 1 do
          Vec.push_back sub_chars (Vec.get chars j)
        done;
        if Vec.length sub_chars > 0
        then
          Vec.push_back current_chunk_segments { chars = sub_chars; attr = segment.attr }));
  flush_chunk ();
  Vec.to_list out
;;

(* [typeset_line] will "typeset" https://en.wikipedia.org/wiki/Typesetting the text. It
   will break up your text attempting to do "word wrapping" so that each line fits in
   max_width so that the output lines fit within max_width.

   It is important to note that only the "visual" part of the line will be <= max_width.
   If a line has trailing whitespace, that whitespace can exceed [max_width] as long as
   the "visual" width is <= max_width.

   [max_width] is treated as [Int.max 5 max_width] so that the weird unicode characters
   don't get weirder. e.g. some unicode characters can take up 2 chars though this
   behavior is not stable across terminal emulators, as some terminal emulators will make
   some emojis look as wide as 4 chars. *)
let typeset_line ~max_width (line : 'attr Text.t list) : 'attr Text.t list list =
  let max_width = Int.max 5 max_width in
  match line with
  | [] -> []
  | [ single ] when Iarray.for_all single.chars ~f:is_whitespace -> [ line ]
  | _ :: _ ->
    let segments =
      List.map line ~f:(fun { chars; attr } ->
        { Mutable_segment.chars = Vec.of_array (Iarray.to_array chars); attr })
    in
    let chunks = chunkenize segments in
    let lines = Vec.create () in
    let current_line_segments = Vec.create () in
    let current_line_width = ref 0 in
    let flush_line () =
      if Vec.length current_line_segments > 0
      then (
        Vec.push_back lines (Vec.copy current_line_segments);
        Vec.clear current_line_segments;
        current_line_width := 0)
    in
    (* Helper to add segments from a chunk, breaking if necessary *)
    let add_chunk_segments chunk =
      Iarray.iter chunk.Chunk.segments ~f:(fun segment ->
        let chars = Iarray.to_array segment.chars in
        let attr = segment.attr in
        (* Find where trailing whitespace starts *)
        let last_non_ws_idx =
          let rec find idx =
            if idx < 0
            then -1
            else if is_whitespace chars.(idx)
            then find (idx - 1)
            else idx
          in
          find (Array.length chars - 1)
        in
        (* Process visual (non-trailing-whitespace) characters *)
        let char_idx = ref 0 in
        let current_segment_chars = Vec.create () in
        while !char_idx <= last_non_ws_idx do
          let c = chars.(!char_idx) in
          let c_width = Bonsai_term.View.uchar_tty_width c in
          if !current_line_width + c_width <= max_width
          then (
            (* Character fits on current line *)
            Vec.push_back current_segment_chars c;
            current_line_width := !current_line_width + c_width;
            char_idx := !char_idx + 1)
          else (
            (* Character doesn't fit, flush current segment and line *)
            if Vec.length current_segment_chars > 0
            then (
              let seg =
                { Mutable_segment.chars = Vec.copy current_segment_chars; attr }
              in
              Vec.push_back current_line_segments (Mutable_segment.to_segment seg);
              Vec.clear current_segment_chars);
            flush_line ())
        done;
        (* Add trailing whitespace to the last segment without breaking *)
        for i = last_non_ws_idx + 1 to Array.length chars - 1 do
          Vec.push_back current_segment_chars chars.(i)
        done;
        let trailing_ws_width =
          let w = ref 0 in
          for i = last_non_ws_idx + 1 to Array.length chars - 1 do
            w := !w + Bonsai_term.View.uchar_tty_width chars.(i)
          done;
          !w
        in
        current_line_width := !current_line_width + trailing_ws_width;
        (* Flush remaining characters in segment *)
        if Vec.length current_segment_chars > 0
        then (
          let seg = { Mutable_segment.chars = Vec.copy current_segment_chars; attr } in
          Vec.push_back current_line_segments (Mutable_segment.to_segment seg);
          Vec.clear current_segment_chars))
    in
    List.iter chunks ~f:(fun chunk ->
      let chunk_total_width = chunk.Chunk.total_width in
      let chunk_visual_width = chunk.Chunk.visual_width in
      (* Check if chunk fits using visual width for current line + total width for new
         chunk *)
      let would_fit_on_current_line =
        if !current_line_width = 0
        then chunk_total_width <= max_width
        else !current_line_width + chunk_visual_width <= max_width
      in
      if chunk_total_width <= max_width && would_fit_on_current_line
      then (
        (* Chunk fits entirely on current line *)
        Iarray.iter chunk.Chunk.segments ~f:(fun seg ->
          Vec.push_back current_line_segments seg);
        current_line_width := !current_line_width + chunk_total_width)
      else if chunk_total_width <= max_width
      then (
        (* Chunk fits on a line by itself, but not on current line *)
        flush_line ();
        Iarray.iter chunk.Chunk.segments ~f:(fun seg ->
          Vec.push_back current_line_segments seg);
        current_line_width := chunk_total_width)
      else (
        (* Chunk is too big, need to break it *)
        flush_line ();
        add_chunk_segments chunk));
    (* Flush remaining segments *)
    flush_line ();
    Vec.to_list lines |> List.map ~f:Vec.to_list
;;

module For_testing = struct
  module Mutable_segment = Mutable_segment
  module Chunk = Chunk

  let chunkenize = chunkenize
end
