open! Core
open Bonsai_term
open Bonsai.Let_syntax
module For_mocking = For_mocking
module Tmux_cursor = Tmux_cursor
module Tmux_lib = Tmux

module Persistence = struct
  type t =
    | Kill_command_when_component_deactivates
    | Keep_command_alive_if_component_deactivates
end

module Mouse = struct
  type t =
    | Classic
    | Scrollback
end

module Real_tmux = struct
  type t = Tmux.t

  let create ?extra_args ?env ?mouse ?working_dir ~width ~height ~command () =
    Tmux.create ?env ?extra_args ?mouse ?working_dir ~size:{ width; height } ~command ()
  ;;

  let attach = Tmux.attach
  let close = Tmux.close
  let closed = Tmux.closed

  let get_cursor t =
    let%map.Async.Deferred.Or_error { Tmux.Cursor.x; y; cursor_character } =
      Tmux.get_cursor t
    in
    { Tmux_cursor.position = { x; y }; cursor_character }
  ;;

  let dump_screen t =
    Tmux.dump_screen ~preserve_trailing_spaces:() ~dump_escape_sequences:() t
  ;;

  let resize t { Dimensions.height; width } = Tmux.resize t { height; width }
  let send_key = Tmux.send_key
  let send_keys = Tmux.send_keys
end

module Backend = struct
  type t =
    | T :
        { impl : (module For_mocking.S with type t = 'tmux)
        ; dump_lines : 'tmux -> height:int -> string list Async.Deferred.Or_error.t
        ; scroll_wheel :
            ('tmux -> [ `Up | `Down ] -> unit Async.Deferred.Or_error.t) option
        }
        -> t

  let for_mocking (module Tmux : For_mocking.S) =
    T
      { impl = (module Tmux)
      ; dump_lines = (fun tmux ~height:_ -> Tmux.dump_screen tmux)
      ; scroll_wheel = None
      }
  ;;

  let real =
    T
      { impl = (module Real_tmux)
      ; dump_lines =
          (fun tmux ~height ->
            Tmux.dump_visible_screen
              ~preserve_trailing_spaces:()
              ~dump_escape_sequences:()
              ~height
              tmux)
      ; scroll_wheel = Some Tmux.scroll
      }
  ;;
end

module Without_instantitate = struct
  type%delta_knot t =
    { last_view : View.t Pending_or_error.t
    ; handler : Event.t -> unit Effect.t
    ; last_cursor : Tmux_cursor.t Pending_or_error.t
    ; is_closed : bool
    ; close : unit Effect.t
    }
end

type%delta t = { reinstantiate_command : unit Effect.t }

(* Encode a mouse event using the classic xterm encoding. This format is used because
   tmux sets TERM=screen by default, and ncurses with TERM=screen uses the classic
   encoding, not SGR.

   Classic format: \033[M <button_byte> <column_byte> <row_byte>
   where each byte = value + 32 (to make it printable).

   Returns [None] when either coordinate doesn't fit in a single byte (i.e. column or
   row >= 223). Clamping to the last representable cell would silently retarget the
   click, so we'd rather drop the event than land it on the wrong cell. *)
let mouse_escape_sequence_classic
  ~(kind : Event.mouse_kind)
  ~(position : Position.t)
  ~(mods : Event.Modifier.t list)
  =
  let button_base =
    match kind with
    | Left -> 0
    | Middle -> 1
    | Right -> 2
    | Scroll `Up -> 64
    | Scroll `Down -> 65
    | Drag -> 32
    | Hover -> 35
    | Release -> 3
  in
  let modifier_bits =
    List.sum (module Int) mods ~f:(fun mod_ ->
      match mod_ with
      | Shift -> 4
      | Meta -> 8
      | Ctrl -> 16)
  in
  let button_byte = Char.of_int_exn (button_base + modifier_bits + 32) in
  (* mouse coordinates are 1-based, then add 32 for the encoding offset *)
  let coord_byte coord =
    let byte = coord + 1 + 32 in
    match byte >= 0 && byte <= Char.to_int Char.max_value with
    | true -> Some (Char.of_int_exn byte)
    | false -> None
  in
  let%map.Option column_byte = coord_byte position.x
  and row_byte = coord_byte position.y in
  sprintf "\027[M%c%c%c" button_byte column_byte row_byte
;;

(** Convert an [Event.Key.t] to the corresponding [Jane_term_types.Key.basic] value, which
    is the subset of tmux keys that can appear bare, inside an [`Alt] wrapper, etc.
    Returns [None] for keys that have no [basic] counterpart (e.g. [Uchar]). *)
let basic_of_event_key : Event.Key.t -> Jane_term_types.Key.basic option = function
  | ASCII c -> Some (`Char c)
  | Escape -> Some `Esc
  | Arrow dir -> Some (dir :> Jane_term_types.Key.basic)
  | Backspace -> Some `Backspace
  | Enter -> Some `Enter
  | Tab -> Some `Tab
  | Home -> Some `Home
  | End -> Some `End
  | Insert -> Some `Insert
  | Delete -> Some `Delete
  | Page `Up -> Some `Page_up
  | Page `Down -> Some `Page_down
  | Function n -> Some (`F n)
  | Uchar _ -> None
;;

let key_of_event : mouse:Mouse.t option -> Event.t -> Tmux.Key.t option =
  fun ~mouse event ->
  match event with
  (* -- No modifier: bare key ------------------------------------------------ *)
  | Key_press { key = Uchar uchar; mods = [] } ->
    Some (`Raw_escape (Uchar.Utf8.to_string uchar))
  | Key_press { key; mods = [] } ->
    basic_of_event_key key |> Option.map ~f:(fun basic -> (basic :> Tmux.Key.t))
  (* -- Shift ---------------------------------------------------------------- *)
  | Key_press { key = Arrow `Up; mods = [ Shift ] } -> Some `Shift_up
  | Key_press { key = Arrow `Down; mods = [ Shift ] } -> Some `Shift_down
  | Key_press { key = Arrow `Left; mods = [ Shift ] } -> Some `Shift_left
  | Key_press { key = Arrow `Right; mods = [ Shift ] } -> Some `Shift_right
  | Key_press { key = Tab; mods = [ Shift ] } -> Some `Shift_tab
  (* -- Ctrl + Arrow (xterm modifyOtherKeys sequences) ----------------------- *)
  (* [Jane_term_types.Key] has no [Ctrl_left]/etc. constructors, so emit the raw
     xterm-compatible escape sequences that most terminal programs recognize (e.g.
     readline's word-movement, aide-cli). *)
  | Key_press { key = Arrow `Up; mods = [ Ctrl ] } -> Some (`Raw_escape "\027[1;5A")
  | Key_press { key = Arrow `Down; mods = [ Ctrl ] } -> Some (`Raw_escape "\027[1;5B")
  | Key_press { key = Arrow `Right; mods = [ Ctrl ] } -> Some (`Raw_escape "\027[1;5C")
  | Key_press { key = Arrow `Left; mods = [ Ctrl ] } -> Some (`Raw_escape "\027[1;5D")
  (* -- Ctrl ----------------------------------------------------------------- *)
  | Key_press { key = ASCII 'A'; mods = [ Ctrl ] } -> Some (`Ctrl `a)
  | Key_press { key = ASCII 'B'; mods = [ Ctrl ] } -> Some (`Ctrl `b)
  | Key_press { key = ASCII 'C'; mods = [ Ctrl ] } -> Some (`Ctrl `c)
  | Key_press { key = ASCII 'D'; mods = [ Ctrl ] } -> Some (`Ctrl `d)
  | Key_press { key = ASCII 'E'; mods = [ Ctrl ] } -> Some (`Ctrl `e)
  | Key_press { key = ASCII 'F'; mods = [ Ctrl ] } -> Some (`Ctrl `f)
  | Key_press { key = ASCII 'G'; mods = [ Ctrl ] } -> Some (`Ctrl `g)
  | Key_press { key = ASCII 'J'; mods = [ Ctrl ] } -> Some (`Ctrl `j)
  | Key_press { key = ASCII 'K'; mods = [ Ctrl ] } -> Some (`Ctrl `k)
  | Key_press { key = ASCII 'L'; mods = [ Ctrl ] } -> Some (`Ctrl `l)
  | Key_press { key = ASCII 'N'; mods = [ Ctrl ] } -> Some (`Ctrl `n)
  | Key_press { key = ASCII 'O'; mods = [ Ctrl ] } -> Some (`Ctrl `o)
  | Key_press { key = ASCII 'P'; mods = [ Ctrl ] } -> Some (`Ctrl `p)
  | Key_press { key = ASCII 'Q'; mods = [ Ctrl ] } -> Some (`Ctrl `q)
  | Key_press { key = ASCII 'R'; mods = [ Ctrl ] } -> Some (`Ctrl `r)
  | Key_press { key = ASCII 'S'; mods = [ Ctrl ] } -> Some (`Ctrl `s)
  | Key_press { key = ASCII 'T'; mods = [ Ctrl ] } -> Some (`Ctrl `t)
  | Key_press { key = ASCII 'U'; mods = [ Ctrl ] } -> Some (`Ctrl `u)
  | Key_press { key = ASCII 'V'; mods = [ Ctrl ] } -> Some (`Ctrl `v)
  | Key_press { key = ASCII 'W'; mods = [ Ctrl ] } -> Some (`Ctrl `w)
  | Key_press { key = ASCII 'X'; mods = [ Ctrl ] } -> Some (`Ctrl `x)
  | Key_press { key = ASCII 'Y'; mods = [ Ctrl ] } -> Some (`Ctrl `y)
  | Key_press { key = ASCII 'Z'; mods = [ Ctrl ] } -> Some (`Ctrl `z)
  (* -- Meta (Alt) ----------------------------------------------------------- *)
  | Key_press { key; mods = [ Meta ] } ->
    basic_of_event_key key |> Option.map ~f:(fun basic -> `Alt basic)
  (* -- Mouse ---------------------------------------------------------------- *)
  | Mouse { kind; position; mods } ->
    (match mouse with
     | Some Mouse.Classic ->
       mouse_escape_sequence_classic ~kind ~position ~mods
       |> Option.map ~f:(fun s -> `Raw_escape s)
     | Some Mouse.Scrollback | None -> None)
  (* -- Other ---------------------------------------------------------------- *)
  | Paste paste -> Some (`Paste paste)
  | Key_press _ -> None
;;

module Tmux_id : sig
  type t

  include Comparable.S with type t := t

  val get : unit -> t
end = struct
  include Int

  let count = ref 0

  let get () =
    incr count;
    !count
  ;;
end

module Opaque_tmux = struct
  type t = { close : unit -> unit Async.Deferred.Or_error.t } [@@unboxed]
end

let tmuxes_to_close_upon_shutdown = ref Tmux_id.Map.empty

let maybe_register_cleanup =
  (* NOTE: When the program exists, we want to close all of the tmux'es that weren't
     already closed. To do that we register this [at_shutdown] handler and keep a global
     ref for all of the already closed tmuxes. *)
  let registered = ref false in
  let actually_register () =
    let open! Async in
    Async.Shutdown.at_shutdown (fun () ->
      Deferred.Map.iter
        !tmuxes_to_close_upon_shutdown
        ~how:(`Max_concurrent_jobs 20)
        ~f:(fun { Opaque_tmux.close } -> Deferred.ignore_m (close ())));
    registered := true
  in
  fun () -> if !registered then () else actually_register ()
;;

module Tmux_dynamic_args = struct
  (* NOTE: This is [Core_unix.env], but since we add [compare] to it. *)
  type env =
    [ `Replace of (string * string) list
    | `Extend of (string * string) list
    | `Override of (string * string option) list
    | `Replace_raw of string list
    ]
  [@@deriving compare, sexp]

  let () =
    (* [env] and [Core_unix.env] are type_equal. *)
    let Type_equal.T : (env, Core_unix.env) Type_equal.t = Type_equal.T in
    ()
  ;;

  type t =
    { command : string
    ; env : env option
    ; working_dir : string option
    }
  [@@deriving compare, sexp]

  include functor Comparable.Make
end

let scope_model ~command ~env ~working_dir ~persistence (local_ graph) f =
  let scoped_by_command (local_ graph) =
    Bonsai.scope_model
      (module Tmux_dynamic_args)
      ~on:
        (let%arr command and env and working_dir in
         { Tmux_dynamic_args.command; env; working_dir })
      ~for_:(fun (local_ graph) -> f graph)
      graph
  in
  match persistence with
  | Persistence.Keep_command_alive_if_component_deactivates -> scoped_by_command graph
  | Kill_command_when_component_deactivates ->
    let out, reset = Bonsai.with_model_resetter ~f:scoped_by_command graph in
    Bonsai.Edge.lifecycle ~on_deactivate:reset graph;
    out
;;

let register_on_close
  (type tmux)
  (module Tmux : For_mocking.S with type t = tmux)
  ~tmux
  ~set_is_closed
  =
  Effect.Expert.handle
    ~on_exn:raise
    (* NOTE: this is effectively a `don't_wait_for` on handling the closing of the tmux
       instance. *)
    (match tmux with
     | Error _error ->
       (* NOTE: We ignore the error case in this "on-close" handler. We still update tmux later on. *)
       Effect.return ()
     | Ok tmux ->
       let tmux_id = Tmux_id.get () in
       let () =
         tmuxes_to_close_upon_shutdown
         := Map.set
              !tmuxes_to_close_upon_shutdown
              ~key:tmux_id
              ~data:{ Opaque_tmux.close = (fun () -> Tmux.close tmux) }
       in
       let%bind.Effect () = set_is_closed false in
       let%bind.Effect () =
         Effect.of_deferred_thunk (fun () ->
           let%bind.Async.Deferred () = Tmux.closed tmux in
           let () =
             tmuxes_to_close_upon_shutdown
             := Map.remove !tmuxes_to_close_upon_shutdown tmux_id
           in
           Async.Deferred.return ())
       in
       let%bind.Effect () = set_is_closed true in
       Effect.return ())
;;

let tmux_component
  (type tmux)
  (module Tmux : For_mocking.S with type t = tmux)
  ~(persistence : Persistence.t)
  ~(command : string Bonsai.t)
  ~(env : Core_unix.env option Bonsai.t)
  ~(working_dir : string option Bonsai.t)
  ~dimensions
  ?extra_tmux_args
  ?mouse
  (local_ graph)
  =
  maybe_register_cleanup ();
  let%sub ( ~close
          , ~last_lines
          , ~set_last_lines
          , ~last_cursor
          , ~set_last_cursor
          , ~tmux
          , ~is_closed )
    =
    scope_model ~command ~env ~working_dir ~persistence graph
    @@ fun (local_ graph) ->
    let tmux, set_tmux = Bonsai.state Pending_or_error.Pending graph in
    let is_closed, set_is_closed = Bonsai.state true graph in
    let last_lines, set_last_lines = Bonsai.state Pending_or_error.Pending graph in
    let last_cursor, set_last_cursor = Bonsai.state Pending_or_error.Pending graph in
    let instantiate_new_command =
      let%arr set_tmux
      and set_is_closed
      and set_last_lines
      and { Dimensions.width; height } = dimensions
      and command
      and env
      and working_dir in
      let%bind.Effect () = set_is_closed true
      and () = set_tmux Pending_or_error.Pending
      and () = set_last_lines Pending_or_error.Pending in
      let%bind.Effect tmux =
        Effect.of_deferred_thunk (fun () ->
          Tmux.create
            ?env
            ?extra_args:extra_tmux_args
            ?mouse:(Option.map mouse ~f:(fun (_ : Mouse.t) -> ()))
            ?working_dir
            ~width
            ~height
            ~command
            ())
      in
      register_on_close (module Tmux) ~tmux ~set_is_closed;
      set_tmux (Pending_or_error.of_or_error tmux)
    in
    let close =
      match%arr tmux with
      | Error _ | Pending -> Effect.Ignore
      | Ok tmux -> Effect.ignore_m (Effect.of_deferred_thunk (fun () -> Tmux.close tmux))
    in
    let ~on_activate, ~on_deactivate =
      match persistence with
      | Persistence.Kill_command_when_component_deactivates ->
        ~on_activate:instantiate_new_command, ~on_deactivate:(Some close)
      | Keep_command_alive_if_component_deactivates ->
        let has_activated, set_has_activated = Bonsai.state false graph in
        let on_activate =
          let%arr instantiate_new_command
          and has_activated
          and set_has_activated
          and is_closed in
          let should_reinstantiate = (not has_activated) || is_closed in
          match should_reinstantiate with
          | false -> Effect.Ignore
          | true ->
            let%bind.Effect () = set_has_activated true in
            instantiate_new_command
        in
        ~on_activate, ~on_deactivate:None
    in
    Bonsai.Edge.lifecycle ~on_activate ?on_deactivate graph;
    let%arr close
    and last_lines
    and set_last_lines
    and last_cursor
    and set_last_cursor
    and tmux
    and is_closed in
    ( ~close
    , ~last_lines
    , ~set_last_lines
    , ~last_cursor
    , ~set_last_cursor
    , ~tmux
    , ~is_closed )
  in
  ~tmux, ~is_closed, ~close, ~last_lines, ~set_last_lines, ~last_cursor, ~set_last_cursor
;;

let poll_tmux_state ~active ~tmux ~input ~set_state ~poll_tmux (local_ graph) =
  let%sub () =
    match%sub tmux with
    | Pending_or_error.Pending -> Bonsai.return ()
    | Error error ->
      let callback =
        let%arr set_state in
        fun error -> set_state (Pending_or_error.Error error)
      in
      Bonsai.Edge.on_change
        ~trigger:`Before_display
        ~equal:[%equal: Error.t]
        ~callback
        error
        graph;
      Bonsai.return ()
    | Ok tmux ->
      let effect =
        let%arr tmux and input and set_state and active in
        match active with
        | false -> Effect.Ignore
        | true ->
          let%bind.Effect lines =
            Effect.of_deferred_fun
              (fun () ->
                let%bind.Async.Deferred result = poll_tmux tmux input in
                let result = Pending_or_error.of_or_error result in
                Async.Deferred.return result)
              ()
          in
          set_state lines
      in
      let effect =
        let%arr effect =
          Bonsai.Effect_throttling.poll
            (let%arr effect in
             fun () -> effect)
            graph
        in
        match%bind.Effect effect () with
        | Aborted -> Effect.Ignore
        | Finished () -> Effect.Ignore
      in
      Bonsai.Edge.before_display effect graph;
      Bonsai.return ()
  in
  ()
;;

let sync_tmux_dimensions ~tmux ~dimensions ~set_dimensions (local_ graph) =
  let%sub () =
    match%sub tmux with
    | Pending_or_error.Error _ | Pending -> Bonsai.return ()
    | Ok tmux ->
      let callback =
        let%arr tmux in
        fun dimensions ->
          let%bind.Effect _ : unit Or_error.t =
            Effect.of_deferred_fun (fun () -> set_dimensions tmux dimensions) ()
          in
          Effect.return ()
      in
      Bonsai.Edge.on_change
        ~equal:[%equal: Dimensions.t]
        ~trigger:`Before_display
        dimensions
        ~callback
        graph;
      Bonsai.return ()
  in
  ()
;;

let build_tmux_ui
  (type tmux)
  (module Tmux_impl : For_mocking.S with type t = tmux)
  ?do_not_render_cursor
  ?mouse
  ~scroll_wheel
  ~recolor_to_flavor
  ~(tmux : tmux Pending_or_error.t Bonsai.t)
  ~is_closed
  ~close
  ~last_lines
  ~(last_cursor : Tmux_cursor.t Pending_or_error.t Bonsai.t)
  ~dimensions
  (local_ graph)
  =
  let flavor = Bonsai_term_color_scheme.flavor graph in
  let send_event_to_tmux =
    match%sub tmux with
    | Error _ | Pending -> Bonsai.return (fun _ -> Effect.Ignore)
    | Ok tmux ->
      let%arr tmux in
      fun (event : Event.t) ->
        let effect =
          match scroll_wheel, mouse, event with
          | ( Some scroll_wheel
            , Some Mouse.Scrollback
            , Mouse { kind = Scroll direction; position = _; mods = _ } ) ->
            Effect.of_deferred_fun (fun () -> scroll_wheel tmux direction) ()
          | _ ->
            (match key_of_event ~mouse event with
             | None -> Effect.return (Ok ())
             | Some key ->
               Effect.of_deferred_fun (fun () -> Tmux_impl.send_key tmux key) ())
        in
        let%bind.Effect _ : unit Or_error.t = effect in
        Effect.Ignore
  in
  let flush_paste_buffer =
    match%sub tmux with
    | Error _ | Pending -> Bonsai.return (fun _ -> Effect.Ignore)
    | Ok tmux ->
      let%arr tmux in
      fun (buffered_keys : [ Jane_term_types.Key.t | `Raw_escape of string ] list) ->
        let%bind.Effect () = Effect.return () in
        let keys : Tmux_lib.Key.t list =
          (`Paste `Start :: List.map buffered_keys ~f:(fun key -> (key :> Tmux_lib.Key.t)))
          @ [ `Paste `End ]
        in
        let%bind.Effect _ : unit Or_error.t =
          Effect.of_deferred_fun (fun () -> Tmux_impl.send_keys tmux keys) ()
        in
        Effect.Ignore
  in
  let effect_sequencer = Effect_sequencer.create graph in
  let handler =
    let open struct
      module Paste_buffer_model = struct
        type t =
          { inside_paste : bool
          ; paste_buffer :
              [ Jane_term_types.Key.t | `Raw_escape of string ] Reversed_list.t
          }
      end

      module Paste_buffer_input = struct
        type t =
          { send_event_to_tmux : Event.t -> unit Effect.t
          ; flush_paste_buffer :
              [ Jane_term_types.Key.t | `Raw_escape of string ] list -> unit Effect.t
          ; effect_sequencer : Effect_sequencer.t
          }
      end

      let recv
        (_ : _ Bonsai.Apply_action_context.t)
        (input : Paste_buffer_input.t Bonsai.Computation_status.t)
        (model : Paste_buffer_model.t)
        (event : Event.t)
        : Paste_buffer_model.t * unit Effect.t
        =
        match event with
        | Paste `Start ->
          ( { Paste_buffer_model.inside_paste = true; paste_buffer = Reversed_list.[] }
          , Effect.Ignore )
        | Paste `End ->
          let effect =
            match input with
            | Inactive -> Effect.Ignore
            | Active { flush_paste_buffer; effect_sequencer; _ } ->
              let buffered = Reversed_list.rev model.paste_buffer in
              Effect_sequencer.run
                effect_sequencer
                ~this_effect_doesn't_call_run:(flush_paste_buffer buffered)
          in
          { inside_paste = false; paste_buffer = Reversed_list.[] }, effect
        | _ when model.inside_paste ->
          let key = key_of_event ~mouse:None event in
          let paste_buffer =
            match key with
            | None | Some (`Paste _) -> model.paste_buffer
            | Some (#Jane_term_types.Key.t as key) -> key :: model.paste_buffer
            | Some (`Raw_escape _ as key) -> key :: model.paste_buffer
          in
          { model with paste_buffer }, Effect.Ignore
        | _ ->
          let effect =
            match input with
            | Inactive -> Effect.Ignore
            | Active { send_event_to_tmux; effect_sequencer; _ } ->
              (* NOTE: The call to [tmux send-keys] may resolve at arbitrary times in an
                 arbitrary order. As such, we use [effect_sequencer] to ensure that we
                 wait for these effects to finish. *)
              Effect_sequencer.run
                effect_sequencer
                ~this_effect_doesn't_call_run:(send_event_to_tmux event)
          in
          model, effect
      ;;
    end in
    let _model, inject =
      Bonsai.actor_with_input
        ~default_model:
          { Paste_buffer_model.inside_paste = false; paste_buffer = Reversed_list.[] }
        ~recv
        (let%arr send_event_to_tmux and flush_paste_buffer and effect_sequencer in
         { Paste_buffer_input.send_event_to_tmux; flush_paste_buffer; effect_sequencer })
        graph
    in
    let%arr inject in
    fun event -> Effect.join (inject event)
  in
  let last_view =
    let%arr last_lines
    and recolor_to_flavor
    and { Dimensions.width; height } = dimensions in
    let%map.Pending_or_error last_lines in
    let string = String.concat_lines last_lines in
    let%pattern_bind.Option ~flavor, ~bg = recolor_to_flavor in
    let view =
      Bonsai_term_ansi_text_renderer.render ?flavor ?bg ~fill_width:No
      @@ Ansi_text.parse string
    in
    match recolor_to_flavor with
    | Some _ -> view
    | None ->
      let backdrop =
        View.rectangle ~attrs:[ Attr.bg Attr.Color.Expert.default ] ~width ~height ()
      in
      View.zcat
        [ View.with_colors
            ~bg:Attr.Color.Expert.default
            ~fg:Attr.Color.Expert.default
            view
        ; backdrop
        ]
  in
  let last_view =
    match do_not_render_cursor with
    | Some () -> last_view
    | None ->
      let%arr last_view
      and flavor
      and last_cursor
      and { width; height } = dimensions in
      (match last_cursor with
       | Error _ | Pending -> last_view
       | Ok { position = { y; x }; cursor_character } ->
         let%map.Pending_or_error last_view in
         (match x < width && y < height with
          | false -> last_view
          | true ->
            View.zcat
              [ View.pad
                  ~l:x
                  ~t:y
                  (View.text
                     ~attrs:
                       [ Attr.bg (Bonsai_term_color_scheme.color ~flavor Text)
                       ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Crust)
                       ]
                     cursor_character)
              ; last_view
              ]))
  in
  let%arr is_closed and last_view and handler and last_cursor and close in
  { Without_instantitate.is_closed; last_view; last_cursor; handler; close }
;;

let poll_and_sync_tmux
  (type tmux)
  (module Tmux_impl : For_mocking.S with type t = tmux)
  ~dump_lines
  ~active
  ~tmux
  ~set_last_lines
  ~set_last_cursor
  ~dimensions
  (local_ graph)
  =
  poll_tmux_state
    ~active
    ~tmux
    ~input:dimensions
    ~set_state:set_last_lines
    ~poll_tmux:(fun tmux { Dimensions.height; _ } -> dump_lines tmux ~height)
    graph;
  poll_tmux_state
    ~active
    ~tmux
    ~input:(Bonsai.return ())
    ~set_state:set_last_cursor
    ~poll_tmux:(fun tmux () -> Tmux_impl.get_cursor tmux)
    graph;
  sync_tmux_dimensions
    ~tmux
    ~dimensions
    ~set_dimensions:(fun tmux { width; height } ->
      Tmux_impl.resize tmux { width; height })
    graph
;;

let component_impl
  ?for_mocking
  ?(persistence = Persistence.Kill_command_when_component_deactivates)
  ?do_not_render_cursor
  ?extra_tmux_args
  ?mouse
  ?(env = Bonsai.return None)
  ?(working_dir = Bonsai.return None)
  ?(recolor_to_flavor = Bonsai.return None)
  ?(active = Bonsai.return true)
  ~command
  ~dimensions
  (local_ graph)
  =
  let backend =
    match for_mocking with
    | Some for_mocking -> Backend.for_mocking for_mocking
    | None -> Backend.real
  in
  let (Backend.T { impl = (module Tmux); dump_lines; scroll_wheel }) = backend in
  let ( ~tmux
      , ~is_closed
      , ~close
      , ~last_lines
      , ~set_last_lines
      , ~last_cursor
      , ~set_last_cursor )
    =
    tmux_component
      (module Tmux)
      ~persistence
      ~command
      ~env
      ~working_dir
      ~dimensions
      ?extra_tmux_args
      ?mouse
      graph
  in
  let%sub () =
    scope_model ~command ~env ~working_dir ~persistence graph
    @@ fun (local_ graph) ->
    poll_and_sync_tmux
      (module Tmux)
      ~dump_lines
      ~active
      ~tmux
      ~set_last_lines
      ~set_last_cursor
      ~dimensions
      graph;
    Bonsai.return ()
  in
  build_tmux_ui
    (module Tmux)
    ?do_not_render_cursor
    ?mouse
    ~scroll_wheel
    ~recolor_to_flavor
    ~tmux
    ~is_closed
    ~close
    ~last_lines
    ~last_cursor
    ~dimensions
    graph
;;

let component
  ?for_mocking
  ?persistence
  ?do_not_render_cursor
  ?extra_tmux_args
  ?mouse
  ?env
  ?working_dir
  ?recolor_to_flavor
  ?active
  ~command
  ~dimensions
  (local_ graph)
  =
  let t, reset =
    Bonsai.with_model_resetter
      ~f:(fun (local_ graph) ->
        component_impl
          ?for_mocking
          ?persistence
          ?do_not_render_cursor
          ?extra_tmux_args
          ?mouse
          ?env
          ?working_dir
          ?recolor_to_flavor
          ?active
          ~command
          ~dimensions
          graph)
      graph
  in
  let%arr { last_view; handler; last_cursor; is_closed; close } = t
  and reset in
  { last_view; handler; last_cursor; is_closed; close; reinstantiate_command = reset }
;;

module Attached = Without_instantitate

let attach_tmux_component
  (type tmux)
  (module Tmux_impl : For_mocking.S with type t = tmux)
  ~(session_id : Tmux.Session_id.t Bonsai.t)
  (local_ graph)
  =
  maybe_register_cleanup ();
  let tmux, set_tmux = Bonsai.state Pending_or_error.Pending graph in
  let is_closed, set_is_closed = Bonsai.state true graph in
  let last_lines, set_last_lines = Bonsai.state Pending_or_error.Pending graph in
  let last_cursor, set_last_cursor = Bonsai.state Pending_or_error.Pending graph in
  let attach_effect =
    let%arr set_tmux and set_is_closed and set_last_lines and session_id in
    let%bind.Effect () = set_is_closed true
    and () = set_tmux Pending_or_error.Pending
    and () = set_last_lines Pending_or_error.Pending in
    let%bind.Effect tmux =
      Effect.of_deferred_thunk (fun () -> Tmux_impl.attach session_id)
    in
    register_on_close (module Tmux_impl) ~tmux ~set_is_closed;
    set_tmux (Pending_or_error.of_or_error tmux)
  in
  let close =
    match%arr tmux with
    | Error _ | Pending -> Effect.Ignore
    | Ok tmux ->
      Effect.ignore_m (Effect.of_deferred_thunk (fun () -> Tmux_impl.close tmux))
  in
  Bonsai.Edge.lifecycle ~on_activate:attach_effect graph;
  ~tmux, ~is_closed, ~close, ~last_lines, ~set_last_lines, ~last_cursor, ~set_last_cursor
;;

let attach_component
  ?for_mocking
  ?do_not_render_cursor
  ?(recolor_to_flavor = Bonsai.return None)
  ~session_id
  ~dimensions
  (local_ graph)
  =
  let backend =
    match for_mocking with
    | Some for_mocking -> Backend.for_mocking for_mocking
    | None -> Backend.real
  in
  let (Backend.T { impl = (module Tmux_impl); dump_lines; _ }) = backend in
  let ( ~tmux
      , ~is_closed
      , ~close
      , ~last_lines
      , ~set_last_lines
      , ~last_cursor
      , ~set_last_cursor )
    =
    attach_tmux_component (module Tmux_impl) ~session_id graph
  in
  poll_and_sync_tmux
    (module Tmux_impl)
    ~dump_lines
    ~active:(Bonsai.return true)
    ~tmux
    ~set_last_lines
    ~set_last_cursor
    ~dimensions
    graph;
  build_tmux_ui
    (module Tmux_impl)
    ?do_not_render_cursor
    ~scroll_wheel:None
    ~recolor_to_flavor
    ~tmux
    ~is_closed
    ~close
    ~last_lines
    ~last_cursor
    ~dimensions
    graph
;;
