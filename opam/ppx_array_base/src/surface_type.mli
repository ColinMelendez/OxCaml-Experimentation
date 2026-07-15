(** All implementations use [Array] functions. [Surface_type] just changes the "surface"
    type (i.e. what's exposed to the client). *)

open! Ppxlib
open! Stdppx

type t

val array : t
val iarray : t
val all : t list
val extension_prefix : t -> string
val to_core_type : t -> Context.t -> loc:location -> core_type -> core_type

(** Convert [expr] to have type [t] by writing a conversion like
    {[
      of_array expr
    ]}
    , or just
    {[
      expr
    ]}
    if no conversion if necessary (i.e. [t] is [array]) *)
val wrap : t -> Context.t -> loc:location -> (expression -> expression)

(** Ensure that [ident] has type [_ array] in [expr], by writing a conversion like
    {[
      let ident = to_array ident in
      expr
    ]}
    , or just
    {[
      expr
    ]}
    if no conversion is necessary (i.e. [t] is [array]) *)
val unwrap_in : t -> Context.t -> loc:location -> string -> (expression -> expression)
