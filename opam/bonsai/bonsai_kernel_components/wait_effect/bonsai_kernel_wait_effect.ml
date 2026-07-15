open! Core
open! Bonsai.Let_syntax

module Action = struct
  type t =
    | Add of ((unit, unit) Bonsai.Effect.Private.Callback.t[@sexp.opaque])
    | Trigger
  [@@deriving sexp_of]
end

let upon condition (local_ graph) =
  let callbacks, inject =
    Bonsai.state_machine
      ~sexp_of_action:Action.sexp_of_t
      ~default_model:[]
      ~apply_action:(fun context callbacks action ->
        match action with
        | Add callback -> callback :: callbacks
        | Trigger ->
          List.iter callbacks ~f:(fun callback ->
            Bonsai.Apply_action_context.schedule_event
              context
              (Bonsai.Effect.Private.Callback.respond_to callback ()));
          [])
      graph
  in
  let has_pending_callbacks =
    let%arr callbacks in
    not (List.is_empty callbacks)
  in
  (* Because we don't have a version of [on_change] that allows us to optionally schedule
     an effect, using the lower-level [after_display] should result in fewer effects being
     scheduled. *)
  let after_display =
    let%arr inject and condition and has_pending_callbacks in
    if condition && has_pending_callbacks then Some (inject Trigger) else None
  in
  let () = Bonsai.Edge.lifecycle' ~after_display graph in
  let%arr inject in
  Bonsai.Effect.Private.make ~request:() ~evaluator:(fun callback ->
    Bonsai.Effect.Expert.handle
      (inject (Add callback))
      ~on_exn:(Bonsai.Effect.Private.Callback.on_exn callback))
;;
