open! Core
open Js_of_ocaml

type t =
  [ `Browser
  | `Browser_test
  | `Browser_benchmark
  | `Node
  | `Node_benchmark
  | `Node_test
  | `Node_jsdom_test
  ]
[@@deriving equal ~localize]

let am_running_how : t =
  let is_in_screenshot_test = Option.is_some (Sys.getenv "AM_RUNNING_SCREENSHOT_TEST") in
  match is_in_screenshot_test with
  | true -> `Browser_test
  | false ->
    let is_in_node =
      Js.Optdef.test (Obj.magic Js_of_ocaml.Js.Unsafe.global##.process : _ Js.Optdef.t)
    in
    let has_document_global =
      Js.Optdef.test (Obj.magic Dom_html.document : _ Js.Optdef.t)
    in
    let is_benchmark =
      match Sys.getenv "BENCHMARKS_RUNNER" with
      | Some "TRUE" -> true
      | _ -> false
    in
    if is_in_node (* Running under NodeJS. *)
    then
      if is_benchmark
      then `Node_benchmark
      else if Core.am_running_test
      then if has_document_global then `Node_jsdom_test else `Node_test
      else `Node
    else if is_benchmark (* Running in chrome. *)
    then `Browser_benchmark
    else if Core.am_running_test
    then `Browser_test
    else `Browser
;;

let am_in_browser t =
  match t with
  | `Browser | `Browser_test | `Browser_benchmark -> true
  | `Node | `Node_benchmark | `Node_test | `Node_jsdom_test -> false
;;

let am_in_browser_like_api t =
  match t with
  | `Browser | `Browser_test | `Browser_benchmark | `Node_jsdom_test -> true
  | `Node | `Node_benchmark | `Node_test -> false
;;
