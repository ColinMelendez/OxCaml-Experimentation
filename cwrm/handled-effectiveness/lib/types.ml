open! Core

module Event = struct
  type t =
    | Fork of
        { parent : string
        ; child : string
        }
    | Yield of { fiber : string }
    | Resume of { fiber : string }
    | Say of
        { fiber : string
        ; text : string
        }
    | Complete of { fiber : string }
    | Get of { value : int }
    | Set of
        { old_value : int
        ; new_value : int
        }
    | Send of { value : int }
    | Recv of { value : int }
    | Yield_char of { char : char }
    | Finished
  [@@deriving sexp, equal]

  let to_string = function
    | Fork { parent; child } -> sprintf "Fork  %s -> %s" parent child
    | Yield { fiber } -> sprintf "Yield %s" fiber
    | Resume { fiber } -> sprintf "Resume %s" fiber
    | Say { fiber; text } -> sprintf "Say   %s %S" fiber text
    | Complete { fiber } -> sprintf "Done  %s" fiber
    | Get { value } -> sprintf "Get   -> %d" value
    | Set { old_value; new_value } -> sprintf "Set   %d -> %d" old_value new_value
    | Send { value } -> sprintf "Send  %d" value
    | Recv { value } -> sprintf "Recv  <- %d" value
    | Yield_char { char } -> sprintf "Yield '%c'" char
    | Finished -> "Finished"
  ;;

  let glyph = function
    | Fork _ -> "F"
    | Yield _ -> "Y"
    | Resume _ -> "R"
    | Say _ -> "S"
    | Complete _ -> "."
    | Get _ -> "G"
    | Set _ -> "T"
    | Send _ -> ">"
    | Recv _ -> "<"
    | Yield_char _ -> "*"
    | Finished -> "|"
  ;;
end

module Frame = struct
  type t =
    { step : int
    ; event : Event.t
    ; explanation : string
    ; ready : string list
    ; running : string option
    ; console : string
    ; state_panel : string
    }
  [@@deriving sexp, equal]
end

module Demo = struct
  type t =
    { id : string
    ; title : string
    ; blurb : string
    ; frames : Frame.t list
    }
  [@@deriving sexp, equal]

  let num_steps t = List.length t.frames

  let frame t ~step =
    match List.nth t.frames step with
    | Some frame -> frame
    | None ->
      { Frame.step
      ; event = Finished
      ; explanation = "no frame"
      ; ready = []
      ; running = None
      ; console = ""
      ; state_panel = ""
      }
  ;;
end

module Ui_state = struct
  type t =
    { demo_index : int
    ; step : int
    ; playing : bool
    }
  [@@deriving sexp, equal]

  let initial = { demo_index = 0; step = 0; playing = false }

  type action =
    | Next_step
    | Prev_step
    | Reset
    | Toggle_play
    | Next_demo
    | Prev_demo
    | Set_demo of int
    | Tick
  [@@deriving sexp]

  let clamp_step step ~num_steps =
    if num_steps <= 0 then 0 else Int.clamp_exn step ~min:0 ~max:(num_steps - 1)
  ;;

  let apply t action ~num_demos ~num_steps =
    let num_demos = Int.max 1 num_demos in
    match action with
    | Next_step -> { t with step = clamp_step (t.step + 1) ~num_steps; playing = false }
    | Prev_step -> { t with step = clamp_step (t.step - 1) ~num_steps; playing = false }
    | Reset -> { t with step = 0; playing = false }
    | Toggle_play -> { t with playing = not t.playing }
    | Next_demo ->
      { demo_index = (t.demo_index + 1) % num_demos; step = 0; playing = false }
    | Prev_demo ->
      { demo_index = (t.demo_index - 1 + num_demos) % num_demos; step = 0; playing = false }
    | Set_demo i ->
      { demo_index = Int.clamp_exn i ~min:0 ~max:(num_demos - 1); step = 0; playing = false }
    | Tick ->
      if t.playing
      then (
        let next = t.step + 1 in
        if next >= num_steps
        then { t with step = clamp_step (num_steps - 1) ~num_steps; playing = false }
        else { t with step = next })
      else t
  ;;
end
