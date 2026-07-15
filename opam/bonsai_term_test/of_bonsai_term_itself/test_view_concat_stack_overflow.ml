open! Core
open Bonsai_test
open Bonsai_term

(* Regression test: [Notty.I.vcat] / [Notty.I.hcat] used to overflow the stack for
   sufficiently long lists, because [Notty.concatm]'s inner [accum] helper was
   non-tail-recursive:

   {[
     let rec accum ( @ ) = function
       | ([] | [ _ ]) as xs -> xs
       | a :: b :: xs -> (a @ b) :: accum ( @ ) xs
     ;;
   ]}

   With [N] items in the list, the first call to [accum] recurses [N / 2] times, which
   blows the 8 MB stack at around 350k items (and is what produced the original
   ~175k-frame crash). *)

let run_test view =
  let handle =
    Bonsai_term_test.create_handle_without_handler (fun ~dimensions:_ _ ->
      Bonsai.return view)
  in
  Handle.show handle
;;

(* Comfortably past the threshold that used to overflow the stack. *)
let num_items = 5_000_000

let%expect_test "[View.vcat] of a very long list does not overflow the stack" =
  let view = View.vcat (List.init num_items ~f:(fun _ -> View.text "x")) in
  run_test view;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    │x                                                                               │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "[View.hcat] of a very long list does not overflow the stack" =
  let view = View.hcat (List.init num_items ~f:(fun _ -> View.text "x")) in
  run_test view;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx│
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
