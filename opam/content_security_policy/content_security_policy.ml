open! Core

module Source = struct
  module T = struct
    type t =
      | Self
      | Unsafe_inline
      | Unsafe_eval
      | Wasm_unsafe_eval
      | Strict_dynamic
      | Report_sample
      | Inline_content of string
      | Host_or_scheme of string
    [@@deriving compare ~localize, sexp]
  end

  include T
  include Comparable.Make [@mode local] (T)

  let to_string = function
    | Self -> "'self'"
    | Unsafe_inline -> "'unsafe-inline'"
    | Unsafe_eval -> "'unsafe-eval'"
    | Wasm_unsafe_eval -> "'wasm-unsafe-eval'"
    | Strict_dynamic -> "'strict-dynamic'"
    | Report_sample -> "'report-sample'"
    | Inline_content content ->
      let hash =
        Base64.encode_string (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) content)
      in
      [%string "'sha256-%{hash}'"]
    | Host_or_scheme s -> s
  ;;
end

let sources_to_string set =
  if Set.is_empty set
  then "'none'"
  else
    set
    |> Set.map (module String) ~f:Source.to_string
    |> Set.to_list
    |> String.concat ~sep:" "
;;

module Fetch_type = struct
  module T = struct
    type t =
      | Connect
      | Default
      | Font
      | Frame
      | Img
      | Manifest
      | Media
      | Object
      | Prefetch
      | Script
      | Style
      | Worker
    [@@deriving compare ~localize, sexp]
  end

  include T
  include Comparable.Make [@mode local] (T)

  let to_string = function
    | Connect -> "connect-src"
    | Default -> "default-src"
    | Font -> "font-src"
    | Frame -> "frame-src"
    | Img -> "img-src"
    | Manifest -> "manifest-src"
    | Media -> "media-src"
    | Object -> "object-src"
    | Prefetch -> "prefetch-src"
    | Script -> "script-src"
    | Style -> "style-src"
    | Worker -> "worker-src"
  ;;
end

type t =
  { report_uri : string option
  ; fetch_directives : Source.Set.t Fetch_type.Map.t
  ; base_uri : Source.Set.t option
  ; form_action : Source.Set.t option
  ; frame_ancestors : Source.Set.t option
  ; insecure_requests : [ `Block | `Upgrade | `Allow ]
  }
[@@deriving sexp_of]

let create
  ?report_uri
  ?base_uri
  ?form_action
  ?frame_ancestors
  ~insecure_requests
  fetch_directives
  =
  { report_uri
  ; fetch_directives =
      fetch_directives
      |> List.Assoc.map ~f:Source.Set.of_list
      |> Fetch_type.Map.of_alist_reduce ~f:Set.union
  ; base_uri = Option.map base_uri ~f:Source.Set.of_list
  ; form_action = Option.map form_action ~f:Source.Set.of_list
  ; frame_ancestors = Option.map frame_ancestors ~f:Source.Set.of_list
  ; insecure_requests
  }
;;

let sources_based_directive_to_string name sources =
  name ^ " " ^ sources_to_string sources
;;

let sources_based_directive_to_string' name sources =
  Option.map sources ~f:(sources_based_directive_to_string name)
;;

let fetch_directive_to_string (type_, sources) =
  sources_based_directive_to_string (Fetch_type.to_string type_) sources
;;

let insecure_requests_to_string = function
  | `Allow -> None
  | `Block -> Some "block-all-mixed-content"
  | `Upgrade -> Some "upgrade-insecure-requests"
;;

let to_string
  { report_uri
  ; fetch_directives
  ; base_uri
  ; form_action
  ; frame_ancestors
  ; insecure_requests
  }
  =
  [ Option.map report_uri ~f:(fun uri -> "report-uri " ^ uri) |> Option.to_list
  ; sources_based_directive_to_string' "base-uri" base_uri |> Option.to_list
  ; sources_based_directive_to_string' "form-action" form_action |> Option.to_list
  ; sources_based_directive_to_string' "frame-ancestors" frame_ancestors |> Option.to_list
  ; insecure_requests_to_string insecure_requests |> Option.to_list
  ; fetch_directives |> Map.to_alist |> List.map ~f:fetch_directive_to_string
  ]
  |> List.concat
  |> List.map ~f:(fun dir -> dir ^ ";")
  |> String.concat ~sep:" "
;;

let header_name = "Content-Security-Policy"
let header_name_report_only = "Content-Security-Policy-Report-Only"

module Monoid = struct
  let least_restrictive a b =
    match a, b with
    | `Allow, _ | _, `Allow -> `Allow
    | `Upgrade, _ | _, `Upgrade -> `Upgrade
    | `Block, `Block -> `Block
  ;;

  let merge_optional_sets a b =
    match a, b with
    | None, None -> None
    | Some _, None | None, Some _ -> None
    | Some a, Some b -> Some (Set.union a b)
  ;;

  let merge_fetch_directives a b =
    Map.merge_skewed a b ~combine:(fun ~key:_ a b -> Set.union a b)
  ;;

  let empty =
    { report_uri = None
    ; base_uri = Some (Source.Set.singleton Self)
    ; form_action = Some (Source.Set.singleton Self)
    ; frame_ancestors = Some (Source.Set.singleton Self)
    ; insecure_requests = `Block
    ; fetch_directives = Fetch_type.Map.singleton Default (Source.Set.singleton Self)
    }
  ;;

  let combine a b =
    { report_uri = Option.first_some b.report_uri a.report_uri
    ; base_uri = merge_optional_sets a.base_uri b.base_uri
    ; form_action = merge_optional_sets a.form_action b.form_action
    ; frame_ancestors = merge_optional_sets a.frame_ancestors b.frame_ancestors
    ; insecure_requests = least_restrictive a.insecure_requests b.insecure_requests
    ; fetch_directives = merge_fetch_directives a.fetch_directives b.fetch_directives
    }
  ;;

  let reduce = List.fold ~init:empty ~f:combine
  let ( |.| ) a b = combine a b
  let finalize = Fn.id

  include struct
    let report_uri uri = { empty with report_uri = Some uri }

    let base_uri source =
      { empty with base_uri = Some (Source.Set.singleton (Host_or_scheme source)) }
    ;;

    let form_action source =
      { empty with form_action = Some (Source.Set.singleton (Host_or_scheme source)) }
    ;;

    let frame_ancestor source =
      { empty with frame_ancestors = Some (Source.Set.singleton (Host_or_scheme source)) }
    ;;

    let insecure_requests a = { empty with insecure_requests = a }

    let fetch kind source =
      { empty with
        fetch_directives =
          merge_fetch_directives
            empty.fetch_directives
            (Fetch_type.Map.singleton kind (Source.Set.of_list [ Source.Self; source ]))
      }
    ;;
  end
end
