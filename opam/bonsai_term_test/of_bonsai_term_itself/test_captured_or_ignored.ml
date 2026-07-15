open! Core
open! Bonsai_term

module Expect_test_config = struct
  include Expect_test_config

  let sanitize s = Expect_test_helpers_core.hide_positions_in_string (sanitize s)
end

let%expect_test "basic captured and ignored values" =
  let captured_val = Captured_or_ignored.captured ~here:[%here] () in
  let ignored_val = Captured_or_ignored.ignored in
  print_s [%sexp (captured_val : Captured_or_ignored.t)];
  [%expect
    {|
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}];
  print_s [%sexp (ignored_val : Captured_or_ignored.t)];
  [%expect {| Ignored |}]
;;

let%expect_test "any - all ignored" =
  let result =
    Captured_or_ignored.any
      (Nonempty_list.create
         Captured_or_ignored.ignored
         [ Captured_or_ignored.ignored; Captured_or_ignored.ignored ])
  in
  print_s [%sexp (result : Captured_or_ignored.t)];
  [%expect {| Ignored |}]
;;

let%expect_test "any - single captured" =
  let result =
    Captured_or_ignored.any
      (Nonempty_list.create
         (Captured_or_ignored.captured ~here:[%here] ())
         [ Captured_or_ignored.ignored; Captured_or_ignored.ignored ])
  in
  print_s [%sexp (result : Captured_or_ignored.t)];
  [%expect
    {|
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}]
;;

let%expect_test "any - multiple captured" =
  let result =
    Captured_or_ignored.any
      (Nonempty_list.create
         (Captured_or_ignored.captured ~here:[%here] ())
         [ Captured_or_ignored.ignored; Captured_or_ignored.captured ~here:[%here] () ])
  in
  print_s [%sexp (result : Captured_or_ignored.t)];
  [%expect
    {|
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL
       lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}]
;;

let%expect_test "capture effect" =
  let effect =
    let%bind.Effect result =
      Captured_or_ignored.capture
        ~here:[%here]
        (Effect.print_s [%message "running effect"])
    in
    print_s [%sexp (result : Captured_or_ignored.t)];
    Effect.return ()
  in
  Effect.Expert.handle effect ~on_exn:raise;
  [%expect
    {|
    "running effect"
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}]
;;

let%expect_test "ignore effect" =
  let effect =
    let%bind.Effect result = Captured_or_ignored.ignore in
    print_s [%sexp (result : Captured_or_ignored.t)];
    Effect.return ()
  in
  Effect.Expert.handle effect ~on_exn:raise;
  [%expect {| Ignored |}]
;;

let%expect_test "Let_syntax bind - captured short-circuits" =
  let effect =
    let%bind.Effect result =
      let open Captured_or_ignored.Let_syntax in
      let%bind () =
        Captured_or_ignored.capture
          ~here:[%here]
          (Effect.print_s [%message "first effect"])
      in
      Captured_or_ignored.capture
        ~here:[%here]
        (Effect.print_s [%message "second effect should not run"])
    in
    print_s [%sexp (result : Captured_or_ignored.t)];
    Effect.return ()
  in
  Effect.Expert.handle effect ~on_exn:raise;
  [%expect
    {|
    "first effect"
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}]
;;

let%expect_test "Let_syntax bind - ignored continues" =
  let effect =
    let%bind.Effect result =
      let open Captured_or_ignored.Let_syntax in
      let%bind () = Captured_or_ignored.ignore in
      Captured_or_ignored.capture
        ~here:[%here]
        (Effect.print_s [%message "second effect should run"])
    in
    print_s [%sexp (result : Captured_or_ignored.t)];
    Effect.return ()
  in
  Effect.Expert.handle effect ~on_exn:raise;
  [%expect
    {|
    "second effect should run"
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}]
;;

let%expect_test "Let_syntax bind - multiple ignored then captured" =
  let effect =
    let%bind.Effect result =
      let open Captured_or_ignored.Let_syntax in
      let%bind () = Captured_or_ignored.ignore in
      let%bind () = Captured_or_ignored.ignore in
      Captured_or_ignored.capture ~here:[%here] (Effect.print_s [%message "final effect"])
    in
    print_s [%sexp (result : Captured_or_ignored.t)];
    Effect.return ()
  in
  Effect.Expert.handle effect ~on_exn:raise;
  [%expect
    {|
    "final effect"
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}]
;;

let%expect_test "Let_syntax map - captured short-circuits" =
  let effect =
    let%bind.Effect result =
      let open Captured_or_ignored.Let_syntax in
      let%map () =
        Captured_or_ignored.capture
          ~here:[%here]
          (Effect.print_s [%message "first effect"])
      in
      print_s [%message "this should not print"];
      Captured_or_ignored.captured ~here:[%here] ()
    in
    print_s [%sexp (result : Captured_or_ignored.t)];
    Effect.return ()
  in
  Effect.Expert.handle effect ~on_exn:raise;
  [%expect
    {|
    "first effect"
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}]
;;

let%expect_test "Let_syntax map - ignored continues" =
  let effect =
    let%bind.Effect result =
      let open Captured_or_ignored.Let_syntax in
      let%map () = Captured_or_ignored.ignore in
      print_s [%message "this should print"];
      Captured_or_ignored.captured ~here:[%here] ()
    in
    print_s [%sexp (result : Captured_or_ignored.t)];
    Effect.return ()
  in
  Effect.Expert.handle effect ~on_exn:raise;
  [%expect
    {|
    "this should print"
    (Captured
     (here
      (lib/bonsai_term_test/of_bonsai_term_itself/test_captured_or_ignored.ml:LINE:COL)))
    |}]
;;
