open! Core

let%expect_test "run (return x) = x" =
  let result = Trampoline.run (Trampoline.return 42) in
  print_s [%sexp (result : int)];
  [%expect {| 42 |}]
;;

let%expect_test "map" =
  let result = Trampoline.run (Trampoline.map (Trampoline.return 2) ~f:(( + ) 3)) in
  print_s [%sexp (result : int)];
  [%expect {| 5 |}]
;;

let%expect_test "let-syntax bind" =
  let open Trampoline.Let_syntax in
  let t =
    let%bind x = Trampoline.return 2 in
    let%bind y = Trampoline.return 3 in
    return (x + y)
  in
  print_s [%sexp (Trampoline.run t : int)];
  [%expect {| 5 |}]
;;

let%expect_test "lazy_" =
  let t = Trampoline.lazy_ (lazy (Trampoline.return 7)) in
  print_s [%sexp (Trampoline.run t : int)];
  [%expect {| 7 |}]
;;

let%expect_test "all" =
  let result =
    Trampoline.run (Trampoline.all [ Trampoline.return 1; Trampoline.return 2 ])
  in
  print_s [%sexp (result : int list)];
  [%expect {| (1 2) |}]
;;

let%expect_test "all_map" =
  let module M = Int.Map in
  let map = M.of_alist_exn [ 2, Trampoline.return "two"; 1, Trampoline.return "one" ] in
  let result = Trampoline.run (Trampoline.all_map map) in
  print_s [%sexp (result : string M.t)];
  [%expect {| ((1 one) (2 two)) |}]
;;

let%expect_test "all_nonempty_list" =
  let open Trampoline.Let_syntax in
  let l =
    Nonempty_list.of_list_exn [ 1; 2; 3 ]
    |> Nonempty_list.map ~f:(fun i ->
      let%map doubled = Trampoline.return (i * 2) in
      doubled)
  in
  let result = Trampoline.run (Trampoline.all_nonempty_list l) |> Nonempty_list.to_list in
  print_s [%sexp (result : int list)];
  [%expect {| (2 4 6) |}]
;;

let%expect_test "run is stack-safe for large chains when using lazy_" =
  let open Trampoline.Let_syntax in
  let rec loop n =
    if n = 0
    then return 0
    else (
      let%bind x = Trampoline.lazy_ (lazy (loop (n - 1))) in
      return (x + 1))
  in
  let result = Trampoline.run (loop 100_000) in
  print_s [%sexp (result : int)];
  [%expect {| 100000 |}]
;;
