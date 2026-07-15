open! Core

module Column_display : sig
  (** How to render column headers for nested records.

      - [`dotted]: [foo.bar.baz]
      - [`stacked]: [foo] [bar] [baz] on separate lines
      - [`last]: just [baz]
      - [`auto]: uses a primitive heuristic based on field name length to choose between
        [`dotted] and [`stacked] on a per-column basis
      - [`custom]: override default rendering behavior and return an arbitrary string. It
        will be called with the path into the sexp for each column. *)
  type t =
    [ `auto
    | `dotted
    | `stacked
    | `last
    | `custom of string list -> string
    ]
  [@@deriving sexp]

  val all : custom:(string list -> string) -> t list
  val render : t -> string list -> string
end

(** {[
      print [ [%sexp { a = "foo"; b = "bar" }]; [%sexp { a = "baz"; b = "qux" }] ]
    ]}

    {v
   ┌─────┬─────┐
   │ a   │ b   │
   ├─────┼─────┤
   │ foo │ bar │
   │ baz │ qux │
   └─────┴─────┘
    v}

    With [~transpose:true], the same input produces:

    {v
   ┌┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┐
   ├┴┴┴┼┴┴┴┴┴┼┴┴┴┴┴┤
   │ a │ foo │ baz │
   │ b │ bar │ qux │
   └───┴─────┴─────┘
    v} *)
val print
  :  ?max_column_width:int
  -> ?max_depth:int
  -> ?align:[< `left | `right | `center | `numbers ]
  -> ?display:Ascii_table_kernel.Display.t
  -> ?separate_rows:bool
  -> ?limit_width_to:int
  -> ?prefer_split_on_spaces:bool
  -> ?nested_columns:Column_display.t (** Defaults to [`auto]. *)
  -> ?transpose:bool (** Defaults to [false]. *)
  -> Sexp.t list
  -> unit

(** [print_alist] is a helper that prints a list of labeled rows. It's useful if you have
    a few different (named) examples that you want to put in a table to compare.

    {[
      print_alist
        [%sexp_of: Point.t]
        [ "top left", { x = 5.0; y = 6.0 }; "bottom right", { x = 0.0; y = 1.2 } ]
    ]}
    {v
   ┌──────────────┬───┬─────┐
   │              │ x │ y   │
   ├──────────────┼───┼─────┤
   │ top left     │ 5 │ 6   │
   │ bottom right │ 0 │ 1.2 │
   └──────────────┴───┴─────┘
    v} *)
val print_alist
  : ('a : value_or_null).
  ?max_column_width:int
  -> ?max_depth:int
  -> ?align:[< `left | `right | `center | `numbers ]
  -> ?display:Ascii_table_kernel.Display.t
  -> ?separate_rows:bool
  -> ?limit_width_to:int
  -> ?prefer_split_on_spaces:bool
  -> ?nested_columns:Column_display.t (** Defaults to [`auto]. *)
  -> ?transpose:bool (** Defaults to [false]. *)
  -> ('a -> Sexp.t)
  -> (string * 'a) list
  -> unit

(** [print_record] gives you a one-level pivot table. If you give it a record-shaped sexp,
    it will split each nested column into a separate row instead of rendering them as
    nested columns.

    {[
      print_record
        [%sexp { top_left = { x = 5; y = 6 }; bottom_right = { x = 0; y = 1.2 } }]
    ]}

    {v
    ┌───┬──────────┬──────────────┐
    │   │ top_left │ bottom_right │
    ├───┼──────────┼──────────────┤
    │ x │ 5        │ 0            │
    │ y │ 6        │ 1.2          │
    └───┴──────────┴──────────────┘
    v}

    With [~transpose:true], the output is similar to [print_alist], treating each field in
    your top-level record as a separate alist entry. This can be convenient for comparing
    similar structures using [ppx_sexp] literal record syntax.

    {[
      print_record
        ~transpose:true
        [%sexp { top_left = { x = 5; y = 6 }; bottom_right = { x = 0; y = 1.2 } }]
    ]}

    {v
    ┌──────────────┬───┬─────┐
    │              │ x │ y   │
    ├──────────────┼───┼─────┤
    │ top_left     │ 5 │ 6   │
    │ bottom_right │ 0 │ 1.2 │
    └──────────────┴───┴─────┘
    v} *)
val print_record
  :  ?max_column_width:int
  -> ?max_depth:int
  -> ?align:[< `left | `right | `center | `numbers ]
  -> ?display:Ascii_table_kernel.Display.t
  -> ?separate_rows:bool
  -> ?limit_width_to:int
  -> ?prefer_split_on_spaces:bool
  -> ?nested_columns:Column_display.t (** Defaults to [`auto]. *)
  -> ?transpose:bool (** Defaults to [false]. *)
  -> Sexp.t
  -> unit

(** [print_record_transposed] is [print_record ~transpose:true]. It exists as a top-level
    function for backwards compatibility and will probably be deleted soon. *)
val print_record_transposed
  :  ?max_column_width:int
  -> ?max_depth:int
  -> ?align:[< `left | `right | `center | `numbers ]
  -> ?display:Ascii_table_kernel.Display.t
  -> ?separate_rows:bool
  -> ?limit_width_to:int
  -> ?prefer_split_on_spaces:bool
  -> ?nested_columns:Column_display.t (** Defaults to [`auto]. *)
  -> Sexp.t
  -> unit

(** [print_cases] will call the provided function on every element in the input list, then
    convert the inputs and outputs to sexps, and print a table with the results.

    If you pass [~separate_cols:true], the output will include a blank column between the
    input columns and the output columns. It's useful for disambiguation when both the
    input and output sexps are records, but it's unnecessary if one or both of them are
    simple values. *)
val print_cases
  : ('input : value_or_null) ('output : value_or_null).
  ?max_column_width:int
  -> ?max_depth:int
  -> ?align:[< `left | `right | `center | `numbers ]
  -> ?display:Ascii_table_kernel.Display.t
  -> ?separate_rows:bool
  -> ?separate_cols:bool
  -> ?limit_width_to:int
  -> ?prefer_split_on_spaces:bool
  -> ?nested_columns:Column_display.t (** Defaults to [`auto]. *)
  -> sexp_of_input:('input -> Sexp.t)
  -> sexp_of_output:('output -> Sexp.t)
  -> f:('input -> 'output)
  -> 'input list
  -> unit

(** These are exactly the same as the top-level functions with the same name, but they
    return the formatted string instead of printing it to stdout. *)
module (Format @@ portable) : sig
  val print
    :  ?max_column_width:int
    -> ?max_depth:int
    -> ?align:[< `left | `right | `center | `numbers ]
    -> ?display:Ascii_table_kernel.Display.t
    -> ?separate_rows:bool
    -> ?limit_width_to:int
    -> ?prefer_split_on_spaces:bool
    -> ?nested_columns:Column_display.t
    -> ?transpose:bool
    -> Sexp.t list
    -> string

  val print_alist
    : ('a : value_or_null).
    ?max_column_width:int
    -> ?max_depth:int
    -> ?align:[< `left | `right | `center | `numbers ]
    -> ?display:Ascii_table_kernel.Display.t
    -> ?separate_rows:bool
    -> ?limit_width_to:int
    -> ?prefer_split_on_spaces:bool
    -> ?nested_columns:Column_display.t
    -> ?transpose:bool
    -> ('a -> Sexp.t)
    -> (string * 'a) list
    -> string

  val print_record
    :  ?max_column_width:int
    -> ?max_depth:int
    -> ?align:[< `left | `right | `center | `numbers ]
    -> ?display:Ascii_table_kernel.Display.t
    -> ?separate_rows:bool
    -> ?limit_width_to:int
    -> ?prefer_split_on_spaces:bool
    -> ?nested_columns:Column_display.t
    -> ?transpose:bool
    -> Sexp.t
    -> string

  val print_record_transposed
    :  ?max_column_width:int
    -> ?max_depth:int
    -> ?align:[< `left | `right | `center | `numbers ]
    -> ?display:Ascii_table_kernel.Display.t
    -> ?separate_rows:bool
    -> ?limit_width_to:int
    -> ?prefer_split_on_spaces:bool
    -> ?nested_columns:Column_display.t
    -> Sexp.t
    -> string

  val print_cases
    : ('input : value_or_null) ('output : value_or_null).
    ?max_column_width:int
    -> ?max_depth:int
    -> ?align:[< `left | `right | `center | `numbers ]
    -> ?display:Ascii_table_kernel.Display.t
    -> ?separate_rows:bool
    -> ?separate_cols:bool
    -> ?limit_width_to:int
    -> ?prefer_split_on_spaces:bool
    -> ?nested_columns:Column_display.t
    -> sexp_of_input:('input -> Sexp.t)
    -> sexp_of_output:('output -> Sexp.t)
    -> f:('input -> 'output)
    -> 'input list
    -> string
end
