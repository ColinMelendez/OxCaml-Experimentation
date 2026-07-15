let%expect_test "Library_name" =
  print_endline [%Library_name];
  [%expect {| Ppx_var_name_test |}]
;;

let%expect_test "library_name" =
  print_endline [%library_name];
  [%expect {| ppx_var_name_test |}]
;;

let%expect_test "LIBRARY_NAME" =
  print_endline [%LIBRARY_NAME];
  [%expect {| PPX_VAR_NAME_TEST |}]
;;

let%expect_test "library_dash_name" =
  print_endline [%library_dash_name];
  [%expect {| ppx-var-name-test |}]
;;

let%expect_test "optional prefix" =
  print_endline [%var_name.library_name];
  print_endline [%var_name.library_dash_name];
  [%expect
    {|
    ppx_var_name_test
    ppx-var-name-test
    |}]
;;
