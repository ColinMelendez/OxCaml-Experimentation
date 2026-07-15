(** Log messages are stored, starting with V2, as an explicit version followed by the
    message itself. This makes it easier to move the message format forward while still
    allowing older logs to be read by the new code.

    If you make a new version you must add a version to the Version module below and
    should follow the Make_versioned_serializable pattern. *)
module Stable = struct
  open Core.Core_stable

  module T1 = struct
    module V3 = struct
      module T = struct
        module Optional_level = struct
          type t = Level.Stable.V2.t option [@@deriving bin_io, sexp, stable_witness]

          (* There may be yet new log levels we don't know of yet. This just parses them
             as [None] for now. *)
          let t_of_sexp sexp =
            try [%of_sexp: Level.Stable.V2.t option] sexp with
            | (_ : exn) -> None
          ;;
        end

        type 'time t =
          { time : 'time
          ; level : Optional_level.t
          ; message : Sexp_or_string.Stable.V1.t
          ; tags : (string * string) list
          }
        [@@deriving bin_io, sexp, stable_witness] [@@sexp.allow_extra_fields]
      end

      include Versioned.Stable.Make (struct
          type 'time t = 'time T.t [@@deriving bin_io, sexp, stable_witness]

          let%expect_test "bin_digest Message.V3" =
            print_endline [%bin_digest: unit t];
            [%expect {| 32d41e3e7a82b39e6f0b47a7202451e6 |}]
          ;;

          (* Every async log message is serialized with a [version] tag to allow for
             format changes where the type is non-backwards compatible; in this case, you
             can bump the version tag.

             There are other changes that may be backwards compatible (e.g., adding a new
             field or variant), which may necessitate bumping the number in the stable
             module name, but not require all log messages to also use a new tag. As such,
             the [version] tag may differ from the version of the stable module.

             (Breadcrumbs: in the case of [Message.V3] and [Message.V2], we just added a
             warning level. Being a new level, all existing messages written with the [V3]
             serializer would've still been readable by [V2], and bumping the tag would be
             very disruptive, so we didn't bump the tag.) *)
          let version = Versioned.Stable.Version.V2
        end)

      (* this allows for automagical reading of any versioned sexp, so long as we can
         always lift to a Message.t *)
      let t_of_sexp time_of_sexp (sexp : Core.Sexp.t) =
        match sexp with
        | List [ (Atom _ as version); _ ] ->
          (match Versioned.Stable.Version.t_of_sexp version with
           | V2 -> t_of_sexp time_of_sexp sexp)
        | _ ->
          Core.failwithf !"Log.Message.t_of_sexp: malformed sexp: %{Core.Sexp}" sexp ()
      ;;
    end

    module V2 = struct
      module T = struct
        type 'time t =
          { time : 'time
          ; level : Level.Stable.V1.t option
          ; message : Sexp_or_string.Stable.V1.t
          ; tags : (string * string) list
          }
        [@@deriving bin_io, sexp, stable_witness]

        let of_v3 { V3.T.time; level; message; tags } =
          let message : Sexp_or_string.t =
            match level with
            | None | Some (`Info | `Debug | `Error) -> message
            | Some `Warn ->
              (match message with
               | `String msg -> `String ("(WARN) " ^ msg)
               | `Sexp msg -> `Sexp (List [ List [ Atom "WARN" ]; msg ]))
          in
          let level = Core.Option.map level ~f:Level.Stable.V1.of_v2 in
          { time; level; message; tags }
        ;;

        let to_v3 ({ time; level; message; tags } : _ t) =
          { V3.T.time
          ; level = Core.Option.map level ~f:Level.Stable.V1.to_v2
          ; message
          ; tags
          }
        ;;
      end

      include T

      include Versioned.Stable.Make (struct
          type 'time t = 'time T.t [@@deriving bin_io, sexp, stable_witness]

          let%expect_test "bin_digest Message.V2" =
            print_endline [%bin_digest: unit t];
            [%expect {| 26b02919ac3971aaace97169310e9d15 |}]
          ;;

          let version = Versioned.Stable.Version.V2
        end)
    end
  end
end

open! Core
open! Async_kernel
open! Import

module T1 = struct
  type 'time t = 'time Stable.T1.V3.t [@@deriving sexp_of]
end

type t = Time_float.t T1.t

let create_raw ?level ~time ?(tags = []) message : t = { time; level; message; tags }

let create ?level ?time ?tags message =
  let time = Option.value_or_thunk time ~default:Time_float.now in
  create_raw ?level ~time ?tags message
;;

let time (t : t) = t.time
let level (t : t) = t.level
let set_level (t : t) level = { t with level }
let raw_message (t : t) = t.message
let message (t : t) = Sexp_or_string.Stable.V1.to_string (raw_message t)
let tags (t : t) = t.tags
let add_tags (t : t) tags = { t with tags = List.rev_append tags t.tags }

let level_string (t : t) =
  match t.level with
  | None -> ""
  | Some l -> Level.to_string l ^ " "
;;

let format_tags (t : t) =
  match t.tags with
  | [] -> []
  | _ :: _ -> " --" :: List.concat_map t.tags ~f:(fun (t, v) -> [ " ["; t; ": "; v; "]" ])
;;

let to_write_only_text (t : t) zone =
  let prefix = level_string t in
  let formatted_tags = format_tags t in
  let time_string = Time_float.to_string_abs ~zone t.time in
  String.concat ~sep:"" (time_string :: " " :: prefix :: message t :: formatted_tags)
;;

module For_testing = struct
  let to_string
    (t : t)
    (zone : Core_private.Time_zone.t)
    ~(time : [ `Keep | `Omit ])
    ~(tags : [ `Keep | `Omit ])
    ~(level : [ `Keep | `Omit ])
    =
    let prefix =
      match level with
      | `Keep -> level_string t
      | `Omit -> ""
    in
    let formatted_tags =
      match tags with
      | `Keep -> format_tags t
      | `Omit -> []
    in
    let time_string =
      match time with
      | `Keep -> Time_float.to_string_abs ~zone t.time ^ " "
      | `Omit -> ""
    in
    let list_to_print = time_string :: prefix :: message t :: formatted_tags in
    let filtered_list =
      List.filter list_to_print ~f:(fun str -> not (String.is_empty str))
    in
    String.concat ~sep:"" filtered_list
  ;;
end
