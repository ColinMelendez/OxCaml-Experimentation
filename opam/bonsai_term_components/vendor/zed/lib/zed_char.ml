open Core
open Poly

type t = int [@@deriving bin_io]

let%template[@mode m = (local, global)] compare = (Int.compare [@mode m])
let of_int x = if x < 0 || x > 0x10ffff then invalid_arg "Zed_char.of_int" else x
let to_int t = t
let code = to_int
let is_latin1 t = t < 256
let of_char = Char.to_int
let to_char t = if is_latin1 t then Char.of_int_exn t else failwith "Zed_char.to_char"

let escaped t =
  if t <= 127
  then Char.escaped (Char.of_int_exn t)
  else if t <= 0xff
  then Printf.sprintf "U+%02x" t
  else if t <= 0xffff
  then Printf.sprintf "U+%04x" t
  else Printf.sprintf "U+%06x" t
;;

let sexp_of_t t =
  if t <= 127
  then Char.sexp_of_t (Char.of_int_exn t)
  else if t <= 0xff
  then Sexp.Atom (Printf.sprintf "U+%02x" t)
  else if t <= 0xffff
  then Sexp.Atom (Printf.sprintf "U+%04x" t)
  else Sexp.Atom (Printf.sprintf "U+%06x" t)
;;

let invalid_char_sexp s =
  raise (Sexplib.Conv.Of_sexp_error (Failure "invlad char sexp", s))
;;

let t_of_sexp s =
  match s with
  | Sexp.List _ -> invalid_char_sexp s
  | Sexp.Atom str ->
    let len = String.length str in
    if len = 1
    then (
      let t = Char.to_int str.[0] in
      if t > 127 then invalid_char_sexp s else t)
    else if len >= 3 && str.[0] = 'U' && str.[1] = '+'
    then (
      try of_int (Int.of_string ("0x" ^ String.drop_prefix str 2)) with
      | _ -> invalid_char_sexp s)
    else invalid_char_sexp s
;;

let map_latin1 f t = if is_latin1 t then Char.to_int (f (Char.of_int_exn t)) else t
let uppercase x = map_latin1 Char.uppercase x
let lowercase x = map_latin1 Char.lowercase x

let is_space = function
  | 0x0009
  | 0x000A
  | 0x000B
  | 0x000C
  | 0x000D
  | 0x0020
  | 0x0085
  | 0x00A0
  | 0x1680
  | 0x2000
  | 0x2001
  | 0x2002
  | 0x2003
  | 0x2004
  | 0x2005
  | 0x2006
  | 0x2007
  | 0x2008
  | 0x2009
  | 0x200A
  | 0x2028
  | 0x2029
  | 0x202F
  | 0x205F
  | 0x3000 -> true
  | _ -> false
;;
