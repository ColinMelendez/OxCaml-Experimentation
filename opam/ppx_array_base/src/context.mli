open! Ppxlib

type t =
  | Base
  | Deriving of core_type

(** If your context is [Base], return the label as an ident. Otherwise prepend with
    [Ppx_array_runtime.]. *)
val runtime_ident : t -> label -> Longident.t

(** Same as [runtime_ident], but also converts the ident to a [Pexp_ident] *)
val runtime_fun : t -> location -> label -> expression

val how_to_vary_kinds
  :  t
  -> input:expression
  -> output:expression option
  -> output_separable:bool
  -> How_to_vary_kinds.t
