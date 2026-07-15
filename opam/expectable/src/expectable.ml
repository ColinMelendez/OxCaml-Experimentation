open! Core
include Composition_infix

module Leaf = struct
  type t =
    | Number of
        { str : string
        ; digits_before_decimal : int
        }
    | String of string
    | Empty
  [@@deriving sexp_of]

  let parse_string str =
    let is_num = ref true in
    let i = ref 0 in
    let decimal_index = ref (-1) in
    let len = String.length str in
    let seen_digit = ref false in
    while !is_num && !i < len do
      let c = String.get str !i in
      (match !i, c with
       | 0, '-' -> ()
       | 0, '+' -> ()
       | _, (',' | '_') -> if !seen_digit then seen_digit := false else is_num := false
       | _, '.' ->
         seen_digit := false;
         if !decimal_index = -1
         then decimal_index := !i
         else (* multiple decimals *)
           is_num := false
       | _, c -> if Char.is_digit c then seen_digit := true else is_num := false);
      incr i
    done;
    if !is_num && !i > 0
    then
      Number
        { str
        ; digits_before_decimal =
            (match !decimal_index with
             | -1 -> String.length str
             | index -> index)
        }
    else String str
  ;;

  let parse : Sexp.t -> t = function
    | Atom str -> parse_string str
    | List _ as sexp -> String (Sexp.to_string_hum sexp)
  ;;

  let string : Sexp.t -> t = function
    | Atom str -> String str
    | List _ as sexp -> parse sexp
  ;;

  let%expect_test "parse" =
    List.iter [ "10"; "100_200"; "1,231"; "1.2"; "1."; "-1"; "1.2_3" ] ~f:(fun input ->
      let output = parse_string input in
      print_s [%sexp { input : string; output : t }]);
    [%expect
      {|
      ((input 10) (output (Number (str 10) (digits_before_decimal 2))))
      ((input 100_200) (output (Number (str 100_200) (digits_before_decimal 7))))
      ((input 1,231) (output (Number (str 1,231) (digits_before_decimal 5))))
      ((input 1.2) (output (Number (str 1.2) (digits_before_decimal 1))))
      ((input 1.) (output (Number (str 1.) (digits_before_decimal 1))))
      ((input -1) (output (Number (str -1) (digits_before_decimal 2))))
      ((input 1.2_3) (output (Number (str 1.2_3) (digits_before_decimal 1))))
      |}]
  ;;

  let%expect_test "parse non-numbers" =
    List.iter
      [ ""; " 1"; "1x"; "1.1."; "1-1"; "--1"; "_1"; "1__2"; "1._2" ]
      ~f:(fun input ->
        let output = parse_string input in
        print_s [%sexp { input : string; output : t }]);
    [%expect
      {|
      ((input "") (output (String "")))
      ((input " 1") (output (String " 1")))
      ((input 1x) (output (String 1x)))
      ((input 1.1.) (output (String 1.1.)))
      ((input 1-1) (output (String 1-1)))
      ((input --1) (output (String --1)))
      ((input _1) (output (String _1)))
      ((input 1__2) (output (String 1__2)))
      ((input 1._2) (output (String 1._2)))
      |}]
  ;;
end

let find_key_paths (trees : _ Record_tree.t list) =
  let trie = Ordered_trie.create () in
  let rec helper path sexp =
    match sexp with
    | Record_tree.Choose choices ->
      List.iter choices ~f:(fun (key, value) -> helper (key :: path) value)
    | Record_tree.All trees ->
      Ordered_trie.add trie (List.rev path) ~only_if_empty:true;
      List.iter trees ~f:(helper path)
    | Record_tree.Leaf _ -> Ordered_trie.add trie (List.rev path)
  in
  List.iter trees ~f:(helper []);
  Ordered_trie.depth_first_traversal trie
;;

module%test [@name "find_key_paths"] _ = struct
  let record_trees sexps = List.map sexps ~f:(Record_tree.parse_sexp ~parse_leaf:Fn.id)

  let%expect_test "find key paths" =
    let test sexps =
      print_s [%sexp (find_key_paths (record_trees sexps) : string list list)]
    in
    test [ [%sexp { a = "foo" }] ];
    [%expect {| ((a)) |}];
    test [ [%sexp { a = "foo" }]; [%sexp { a = { x = 1; y = 2 } }] ];
    [%expect {| ((a) (a x) (a y)) |}];
    test [ [%sexp { a = "foo" }]; [%sexp { a = "foo"; b = "bar"; c = "baz"; d = "qux" }] ];
    [%expect {| ((a) (b) (c) (d)) |}];
    test
      [ [%sexp { a = "foo"; b = "bar"; c = "baz"; d = "qux" }] (* alphabetical *)
      ; [%sexp { z = "x"; b = "foo"; c = "bar"; d = "baz"; y = "y" }] (* reverse *)
      ];
    [%expect {| ((a) (b) (c) (d) (z) (y)) |}]
  ;;

  let%expect_test "find key paths preserves order even when an empty list is the first \
                   encountered"
    =
    let test sexps =
      print_s [%sexp (find_key_paths (record_trees sexps) : string list list)]
    in
    (* empty lists count *)
    test [ [%sexp { a = []; b = 1 }] ];
    [%expect {| ((a) (b)) |}];
    test [ [%sexp { a = []; b = 1 }]; [%sexp { a = [ 1 ]; b = 1 }] ];
    [%expect {| ((a) (b)) |}];
    (* empty lists are ignored if there are non-empty children *)
    test [ [%sexp { a = []; b = 1 }]; [%sexp { a = [ { foo = 1 } ]; b = 1 }] ];
    [%expect {| ((a foo) (b)) |}]
  ;;
end

type alist = (string * Sexp.t) list [@@deriving of_sexp]

let string_of_sexp : Sexp.t -> string = function
  | Atom atom -> atom
  | List _ as sexp -> Sexp.to_string_hum sexp
;;

let table_to_string
  ?display
  ?(separate_rows = false)
  ?(limit_width_to = 200)
  ?(prefer_split_on_spaces = true)
  columns
  parsed
  =
  let display =
    Option.value
      display
      ~default:
        (if separate_rows
         then Ascii_table_kernel.Display.tall_box
         else Ascii_table_kernel.Display.short_box)
  in
  Ascii_table_kernel.to_string_noattr
    ~display
    ~limit_width_to
    ~prefer_split_on_spaces
    ~bars:`Unicode
    columns
    parsed
;;

module Column_display = struct
  type simple =
    [ `auto
    | `dotted
    | `stacked
    | `last
    ]
  [@@deriving enumerate, sexp]

  type t =
    [ simple
    | `custom of string list -> string
    ]
  [@@deriving sexp]

  let all ~custom:f = `custom f :: (all_of_simple :> t list)

  let rec render t key_path =
    match t with
    | `last -> Option.value (List.last key_path) ~default:""
    | `dotted -> String.concat key_path ~sep:"."
    | `stacked -> String.concat key_path ~sep:"\n"
    | `auto ->
      let width = List.sum (module Int) key_path ~f:String.length in
      let height = List.length key_path in
      if width / 6 < height then render `dotted key_path else render `stacked key_path
    | `custom f -> f key_path
  ;;
end

let height_of_string string = String.count string ~f:(Char.( = ) '\n') + 1

let vertical_pad string requested_height =
  let padding_lines = requested_height - height_of_string string in
  let padding = String.init padding_lines ~f:(Fn.const '\n') in
  padding ^ string
;;

let trees_to_string
  ?max_column_width
  ?align
  ?display
  ?separate_rows
  ?(drop_prefix = 0)
  ?limit_width_to
  ?prefer_split_on_spaces
  ?(nested_columns = `auto)
  ?(transpose = false)
  trees
  ~use_first_column_as_header_when_transposed
  =
  let align : Ascii_table_kernel.Align.t =
    match align with
    | None | Some (`left | `numbers) -> Left
    | Some `right -> Right
    | Some `center -> Center
  in
  let columns =
    find_key_paths trees
    |> List.map ~f:(fun key_path ->
      key_path, Column_display.render nested_columns (List.drop key_path drop_prefix))
  in
  let rows : Leaf.t list Array.t list =
    List.map trees ~f:(fun tree ->
      List.map columns ~f:(fun (key_path, _) ->
        Record_tree.get_keypath tree key_path
        |> List.map ~f:(function
          | Some (Leaf leaf) -> leaf
          | None | Some (Choose _ | All _) -> Leaf.Empty))
      |> Array.of_list)
  in
  let cell_to_string desired_digits_before_decimal values =
    let value =
      List.map values ~f:(function
        | Leaf.Empty -> ""
        | Leaf.String str -> str
        | Leaf.Number { str; digits_before_decimal } ->
          (match desired_digits_before_decimal with
           | None -> str
           | Some desired_digits_before_decimal ->
             String.pad_left
               str
               ~char:' '
               ~len:
                 (String.length str
                  + desired_digits_before_decimal
                  - digits_before_decimal)))
      |> String.concat ~sep:"\n"
    in
    (* [Ascii_table_kernel] will trim trailing newlines, which can be significant in the
       case of lists containing empty elements, so we add a blank space at the end to stop
       that. *)
    if String.is_suffix value ~suffix:"\n" then value ^ " " else value
  in
  let col_headers, string_rows =
    match transpose with
    | false ->
      let max_opt = Option.map2 ~f:max in
      let max_digits_before_decimal : int option Array.t =
        Array.init (List.length columns) ~f:(fun i ->
          List.fold rows ~init:(Some 0) ~f:(fun best row ->
            max_opt
              best
              (List.fold row.(i) ~init:(Some 0) ~f:(fun best -> function
                 | Number { str = _; digits_before_decimal } ->
                   max_opt best (Some digits_before_decimal)
                 | Empty -> best
                 | String _ -> None))))
      in
      let cell_strings =
        List.map rows ~f:(fun row ->
          List.mapi columns ~f:(fun i _ ->
            cell_to_string max_digits_before_decimal.(i) row.(i)))
      in
      let header_height =
        List.fold columns ~init:1 ~f:(fun acc (_, col) -> max acc (height_of_string col))
      in
      let col_headers =
        List.map columns ~f:(fun (_, display) -> vertical_pad display header_height)
      in
      col_headers, cell_strings
    | true ->
      (* No numeric alignment is applied since transposed columns mix different field
         types. *)
      let cell_to_string values = cell_to_string None values in
      let col_names =
        List.map columns ~f:(fun (key_path, _) ->
          Column_display.render nested_columns (List.drop key_path drop_prefix))
      in
      (match use_first_column_as_header_when_transposed with
       | false ->
         let col_headers = List.init (List.length rows + 1) ~f:(Fn.const "") in
         let string_rows =
           List.mapi col_names ~f:(fun i col_name ->
             col_name :: List.map rows ~f:(fun row -> cell_to_string row.(i)))
         in
         col_headers, string_rows
       | true ->
         (* Column 0 contains row labels (from [print_alist]). Promote them to column
            headers and exclude column 0 from the data rows. *)
         let col_headers = "" :: List.map rows ~f:(fun row -> cell_to_string row.(0)) in
         let string_rows =
           List.filter_mapi col_names ~f:(fun i col_name ->
             if i = 0
             then None
             else Some (col_name :: List.map rows ~f:(fun row -> cell_to_string row.(i))))
         in
         col_headers, string_rows)
  in
  let columns =
    List.mapi col_headers ~f:(fun i header ->
      Ascii_table_kernel.Column.create
        ?max_width:max_column_width
        ~align
        header
        (fun row -> row.(i)))
  in
  let rows = List.map string_rows ~f:Array.of_list in
  table_to_string
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    columns
    rows
;;

let parse_tree ?max_depth ?align sexp =
  Record_tree.parse_sexp
    ?max_depth
    sexp
    ~parse_leaf:
      (match align with
       | None | Some `numbers -> Leaf.parse
       | Some (`left | `right | `center) -> Leaf.string)
;;

let parse_trees ?max_depth ?align sexps = List.map sexps ~f:(parse_tree ?align ?max_depth)

module Format = struct
  let print
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    ?transpose
    =
    parse_trees ?align ?max_depth
    >> trees_to_string
         ?max_column_width
         ?align
         ?display
         ?separate_rows
         ?limit_width_to
         ?prefer_split_on_spaces
         ?nested_columns
         ?transpose
         ~use_first_column_as_header_when_transposed:false
  ;;

  let print_alist
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    ?transpose
    sexp_of_t
    alist
    =
    List.map alist ~f:(fun (name, t) -> [%sexp { name : string; value = (t : t) }])
    |> parse_trees ?align ?max_depth:(Option.map max_depth ~f:succ)
    |> trees_to_string
         ?max_column_width
         ?align
         ?display
         ?separate_rows
         ?limit_width_to
         ?prefer_split_on_spaces
         ?nested_columns
         ?transpose
         ~use_first_column_as_header_when_transposed:true
         ~drop_prefix:1
  ;;

  let print_record
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    ?(transpose = false)
    sexp
    =
    [%of_sexp: (string * Sexp.t) list] sexp
    |> print_alist
         ?max_column_width
         ?max_depth
         ?align
         ?display
         ?separate_rows
         ?limit_width_to
         ?prefer_split_on_spaces
         ?nested_columns
         ~transpose:(not transpose)
         Fn.id
  ;;

  let print_record_transposed
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    sexp
    =
    print_record
      ?max_column_width
      ?max_depth
      ?align
      ?display
      ?separate_rows
      ?limit_width_to
      ?prefer_split_on_spaces
      ?nested_columns
      ~transpose:true
      sexp
  ;;

  let print_cases
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?(separate_cols = false)
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    ~sexp_of_input
    ~sexp_of_output
    ~f
    inputs
    =
    let pair name value = Sexp.List [ Sexp.Atom name; value ] in
    List.map inputs ~f:(fun input ->
      [ Some (pair "in" (sexp_of_input input))
      ; Option.some_if separate_cols (pair "to" (Atom ""))
      ; Some (pair "out" (sexp_of_output (f input)))
      ]
      |> List.filter_opt
      |> Sexp.List)
    |> parse_trees ?align ?max_depth:(Option.map max_depth ~f:succ)
    |> trees_to_string
         ?max_column_width
         ?align
         ~drop_prefix:1
         ?display
         ?separate_rows
         ?limit_width_to
         ?prefer_split_on_spaces
         ?nested_columns
         ~use_first_column_as_header_when_transposed:false
  ;;
end

let print
  ?max_column_width
  ?max_depth
  ?align
  ?display
  ?separate_rows
  ?limit_width_to
  ?prefer_split_on_spaces
  ?nested_columns
  ?transpose
  sexps
  =
  Format.print
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    ?transpose
    sexps
  |> print_endline
;;

let print_alist
  ?max_column_width
  ?max_depth
  ?align
  ?display
  ?separate_rows
  ?limit_width_to
  ?prefer_split_on_spaces
  ?nested_columns
  ?transpose
  sexp_of_t
  alist
  =
  Format.print_alist
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    ?transpose
    sexp_of_t
    alist
  |> print_endline
;;

let print_record
  ?max_column_width
  ?max_depth
  ?align
  ?display
  ?separate_rows
  ?limit_width_to
  ?prefer_split_on_spaces
  ?nested_columns
  ?transpose
  sexp
  =
  Format.print_record
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    ?transpose
    sexp
  |> print_endline
;;

let print_record_transposed
  ?max_column_width
  ?max_depth
  ?align
  ?display
  ?separate_rows
  ?limit_width_to
  ?prefer_split_on_spaces
  ?nested_columns
  sexp
  =
  Format.print_record_transposed
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    sexp
  |> print_endline
;;

let print_cases
  ?max_column_width
  ?max_depth
  ?align
  ?display
  ?separate_rows
  ?separate_cols
  ?limit_width_to
  ?prefer_split_on_spaces
  ?nested_columns
  ~sexp_of_input
  ~sexp_of_output
  ~f
  inputs
  =
  Format.print_cases
    ?max_column_width
    ?max_depth
    ?align
    ?display
    ?separate_rows
    ?separate_cols
    ?limit_width_to
    ?prefer_split_on_spaces
    ?nested_columns
    ~sexp_of_input
    ~sexp_of_output
    ~f
    inputs
  |> print_endline
;;
