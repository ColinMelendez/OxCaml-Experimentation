open Base

[%%template
[@@@kind_set.define k_supported = (value & value, value)]

[%%template
[@@@kind.default k = k_supported]

type ('a : k, 'b : k, 'c : k) tag =
  | Ok : (('a, 'b, 'a) tag[@kind k])
  | Error : (('a, 'b, 'b) tag[@kind k])

type ('a : k, 'b : k) t =
  | T : #((('a, 'b, 'c) tag[@kind k]) * 'c) -> (('a, 'b) t[@kind k])
[@@unboxed]]

[%%template
[@@@mode.default m = (global, local)]
[@@@kind.default k = k_supported]

let[@inline] compare
  (type (a : k) (b : k))
  (cmp_a : a @ m -> a @ m -> int)
  (cmp_b : b @ m -> b @ m -> int)
  (t1 : ((a, b) t[@kind k]))
  (t2 : ((a, b) t[@kind k]))
  =
  match #(t1, t2) with
  | #(T #(Ok, a1), T #(Ok, a2)) -> cmp_a a1 a2
  | #(T #(Ok, _), T #(Error, _)) -> -1
  | #(T #(Error, _), T #(Ok, _)) -> 1
  | #(T #(Error, b1), T #(Error, b2)) -> cmp_b b1 b2
;;

let[@inline] equal
  (type (a : k) (b : k))
  (equal_a : a @ m -> a @ m -> bool)
  (equal_b : b @ m -> b @ m -> bool)
  (t1 : ((a, b) t[@kind k]))
  (t2 : ((a, b) t[@kind k]))
  =
  match #(t1, t2) with
  | #(T #(Ok, a1), T #(Ok, a2)) -> equal_a a1 a2
  | #(T #(Ok, _), T #(Error, _)) -> false
  | #(T #(Error, _), T #(Ok, _)) -> false
  | #(T #(Error, b1), T #(Error, b2)) -> equal_b b1 b2
;;]

[@@@kind.default k = k_supported]

let sexp_of_t
  (type (a : k) (b : k))
  (of_a : a @ m -> Sexp.t @ m)
  (of_b : b @ m -> Sexp.t @ m)
  (T #(tag, x) : ((a, b) t[@kind k]))
  =
  match[@exclave_if_stack a] tag with
  | Ok -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Ok"; of_a x ]
  | Error -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Error"; of_b x ]
[@@alloc a @ m = (heap_global, stack_local)]
;;

let t_of_sexp a_of b_of s : ((_, _) t[@kind k]) =
  match (s : Sexp.t) with
  | Sexp.List [ Sexp.Atom "Ok"; a ] | Sexp.List [ Sexp.Atom "ok"; a ] -> T #(Ok, a_of a)
  | Sexp.List [ Sexp.Atom "Error"; b ] | Sexp.List [ Sexp.Atom "error"; b ] ->
    T #(Error, b_of b)
  | _ ->
    (match
       Sexplib0.Sexp_conv_error.unexpected_stag "result_u.ml.t" [ "Ok"; "Error" ] s
     with
     | (_ : Nothing.t) -> .)
;;

let globalize
  (type (a : k) (b : k))
  (globalize_a : a @ local -> a)
  (globalize_b : b @ local -> b)
  (T #(tag, x) : ((a, b) t[@kind k]))
  : ((a, b) t[@kind k])
  =
  match tag with
  | Ok -> T #(tag, globalize_a x)
  | Error -> T #(tag, globalize_b x)
;;

let hash_fold_t
  (type (a : k) (b : k))
  (hash_a : Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state)
  (hash_b : Ppx_hash_lib.Std.Hash.state -> b -> Ppx_hash_lib.Std.Hash.state)
  (hsv : Ppx_hash_lib.Std.Hash.state)
  (T #(tag, x) : ((a, b) t[@kind k]))
  =
  match tag with
  | Ok ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
    let hsv = hsv in
    hash_a hsv x
  | Error ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
    let hsv = hsv in
    hash_b hsv x
;;

let bin_shape_t bin_shape_a bin_shape_b =
  Bin_prot.Shape.(basetype (Uuid.of_string "result_u") [ bin_shape_a; bin_shape_b ])
;;

include struct
  [@@@mode.default m = (global, local)]

  let bin_size_t
    (type (a : k) (b : k))
    (bin_size_a : a @ m -> int)
    (bin_size_b : b @ m -> int)
    (T #(tag, x) : ((a, b) t[@kind k]) @ m)
    =
    match tag with
    | Ok -> 1 + bin_size_a x
    | Error -> 1 + bin_size_b x
  [@@kind k]
  ;;

  let bin_write_t
    (type (a : k) (b : k))
    (bin_write_a : (a Bin_prot.Write.writer[@mode m]))
    (bin_write_b : (b Bin_prot.Write.writer[@mode m]))
    (buf : Bin_prot.Common.buf)
    ~(pos : int)
    (T #(tag, x) : ((a, b) t[@kind k]) @ m)
    =
    match tag with
    | Ok ->
      let next = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
      bin_write_a buf ~pos:next x
    | Error ->
      let next = Bin_prot.Write.bin_write_int_8bit buf ~pos 1 in
      bin_write_b buf ~pos:next x
  [@@kind k]
  ;;
end

let bin_read_t
  (type (a : k) (b : k))
  (bin_read_a : a Bin_prot.Read.reader)
  (bin_read_b : b Bin_prot.Read.reader)
  (buf : Bin_prot.Common.buf)
  ~(pos_ref : int ref @ local)
  : ((a, b) t[@kind k])
  =
  let pos = Bin_prot.Common.safe_get_pos buf pos_ref in
  Bin_prot.Common.assert_pos pos;
  match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
  | 0 -> T #(Ok, bin_read_a buf ~pos_ref)
  | 1 -> T #(Error, bin_read_b buf ~pos_ref)
  | _ ->
    (match
       Bin_prot.Common.raise_read_error (Bin_prot.Common.ReadError.Sum_tag "result_u") pos
     with
     | (_ : Nothing.t) -> .)
;;

let __bin_read_t__ (type (a : k) (b : k)) _bin_read_a _bin_read_b _buf ~pos_ref _n
  : ((a, b) t[@kind k])
  =
  match Bin_prot.Common.raise_variant_wrong_type "result_u" !pos_ref with
  | (_ : Nothing.t) -> .
;;

let bin_writer_t
  (type (a : k) (b : k))
  (bin_writer_a : a Bin_prot.Type_class.writer)
  (bin_writer_b : b Bin_prot.Type_class.writer)
  =
  { Bin_prot.Type_class.size =
      (fun v -> (bin_size_t [@kind k]) bin_writer_a.size bin_writer_b.size v)
  ; write =
      (fun (buf @ local) ~pos v ->
        (bin_write_t [@kind k]) bin_writer_a.write bin_writer_b.write buf ~pos v)
  }
;;

let bin_reader_t
  (type (a : k) (b : k))
  (bin_reader_a : a Bin_prot.Type_class.reader)
  (bin_reader_b : b Bin_prot.Type_class.reader)
  =
  { Bin_prot.Type_class.read =
      (fun buf ~pos_ref ->
        (bin_read_t [@kind k]) bin_reader_a.read bin_reader_b.read buf ~pos_ref)
  ; vtag_read =
      (fun buf ~pos_ref n ->
        (__bin_read_t__ [@kind k])
          bin_reader_a.vtag_read
          bin_reader_b.vtag_read
          buf
          ~pos_ref
          n)
  }
;;

let bin_t bin_a bin_b =
  { Bin_prot.Type_class.shape =
      (bin_shape_t [@kind k])
        bin_a.Bin_prot.Type_class.shape
        bin_b.Bin_prot.Type_class.shape
  ; writer =
      (bin_writer_t [@kind k])
        bin_a.Bin_prot.Type_class.writer
        bin_b.Bin_prot.Type_class.writer
  ; reader =
      (bin_reader_t [@kind k])
        bin_a.Bin_prot.Type_class.reader
        bin_b.Bin_prot.Type_class.reader
  }
;;

[@@@mode.default m = (global, local)]

let[@inline] return (type a : k) x : ((a, _) t[@kind k]) = T #(Ok, x)
let[@inline] fail (type a : k) x : ((_, a) t[@kind k]) = T #(Error, x)]

(* [ppx_template] doesn't mangle this to fit the [Binable] intf so we must by hand. *)
let __bin_read_t__'value_value'__ = (__bin_read_t__ [@kind value & value])

[%%template
[@@@mode.default m = (global, local)]

let[@inline] match_
  (type a b)
  (T #(tag, x) : (a, b) t)
  ~(ok : (a @ m -> 'c @ m) @ local)
  ~(err : (b @ m -> 'c @ m) @ local)
  =
  match tag with
  | Ok -> ok x [@exclave_if_local m]
  | Error -> err x [@exclave_if_local m]
;;

let[@inline] to_result (type a b) (T #(tag, x) : (a, b) t) : (a, b) Result.t =
  match tag with
  | Ok -> Ok x [@exclave_if_local m]
  | Error -> Error x [@exclave_if_local m]
;;

let[@inline] of_result = function
  | Result.Ok x -> T #(Ok, x)
  | Result.Error x -> T #(Error, x)
;;

let[@inline] bind (type a b c) (t : (a, b) t @ m) ~(f : a @ m -> (c, b) t @ m) : (c, b) t =
  match t with
  | T #(Ok, x) -> f x [@exclave_if_local m]
  | T #(Error, _) as t -> t
;;

let ignore_m (type a b) (T #(tag, x) : (a, b) t) : (unit, b) t =
  match tag with
  | Ok -> T #(Ok, ())
  | Error -> T #(Error, x)
;;

let invariant
  (type a b)
  (check_ok : a @ m -> unit)
  (check_error : b @ m -> unit)
  (T #(tag, x) : (a, b) t)
  =
  match tag with
  | Ok -> check_ok x
  | Error -> check_error x
;;

let ok (type a b) (T #(tag, x) : (a, b) t) : a option =
  match tag with
  | Ok -> Some x [@exclave_if_local m]
  | Error -> None
;;

let error (type a b) (T #(tag, x) : (a, b) t) : b option =
  match tag with
  | Ok -> None
  | Error -> Some x [@exclave_if_local m]
;;

let of_option a_opt ~error =
  match a_opt with
  | Some a -> T #(Ok, a)
  | None -> T #(Error, error)
;;

let iter (type a b) (T #(tag, x) : (a, b) t) ~(f : a @ m -> unit) =
  match tag with
  | Ok -> f x
  | Error -> ()
;;

let iter_error (type a b) (T #(tag, x) : (a, b) t) ~(f : b @ m -> unit) =
  match tag with
  | Ok -> ()
  | Error -> f x
;;

let to_either (type a b) (T #(tag, x) : (a, b) t) : (a, b) Either.t =
  match tag with
  | Ok -> First x [@exclave_if_local m]
  | Error -> Second x [@exclave_if_local m]
;;

let of_either : _ Either.t @ m -> _ @ m = function
  | First x -> T #(Ok, x)
  | Second x -> T #(Error, x)
;;

let to_either_u (type a b) (T #(tag, x) : (a, b) t) : (a, b) Either_u.t =
  match tag with
  | Ok -> T #(First, x)
  | Error -> T #(Second, x)
;;

let of_either_u (type a b) (T #(tag, x) : (a, b) Either_u.t) : (a, b) t =
  match tag with
  | First -> T #(Ok, x)
  | Second -> T #(Error, x)
;;

let ok_if_true b ~error = if b then T #(Ok, ()) else T #(Error, error)

(* template end *)]

let[@inline] is_ok (type a b) (T #(tag, _) : (a, b) t) =
  match tag with
  | Ok -> true
  | Error -> false
;;

let[@inline] is_error (type a b) (T #(tag, _) : (a, b) t) =
  match tag with
  | Ok -> false
  | Error -> true
;;

let ok_exn (type a) (T #(tag, x) : (a, exn) t) : a =
  match tag with
  | Ok -> x
  | Error -> raise x
;;

let ok_or_failwith (type a) (T #(tag, x) : (a, string) t) : a =
  match tag with
  | Ok -> x
  | Error -> failwith x
;;

let of_option_or_thunk a_opt ~error =
  match a_opt with
  | Some a -> T #(Ok, a)
  | None -> T #(Error, error ())
;;

let%template[@mode local] of_option_or_thunk a_opt ~error = exclave_
  match a_opt with
  | Some a -> T #(Ok, a)
  | None -> T #(Error, error ())
;;

let map (type a b c) (T #(tag, x) : (a, b) t) ~(f : a -> c) : (c, b) t =
  match tag with
  | Ok -> T #(Ok, f x)
  | Error -> T #(Error, x)
;;

let%template[@mode local] map
  (type a b c)
  (T #(tag, x) : (a, b) t @ local)
  ~(f : a @ local -> c @ local)
  : (c, b) t
  = exclave_
  match tag with
  | Ok -> T #(Ok, f x)
  | Error -> T #(Error, x)
;;

let map_error (type a b c) (T #(tag, x) : (a, b) t) ~(f : b -> c) : (a, c) t =
  match tag with
  | Ok -> T #(Ok, x)
  | Error -> T #(Error, f x)
;;

let%template[@mode local] map_error
  (type a b c)
  (T #(tag, x) : (a, b) t @ local)
  ~(f : b @ local -> c @ local)
  : (a, c) t
  = exclave_
  match tag with
  | Ok -> T #(Ok, x)
  | Error -> T #(Error, f x)
;;

let combine
  (type ok1 ok2 ok3 err)
  (T #(tag1, x1) : (ok1, err) t)
  (T #(tag2, x2) : (ok2, err) t)
  ~(ok : (ok1 -> ok2 -> ok3) @ local)
  ~(err : (err -> err -> err) @ local)
  : (ok3, err) t
  =
  match tag1, tag2 with
  | Ok, Ok -> T #(Ok, ok x1 x2)
  | Error, Ok -> T #(Error, x1)
  | Ok, Error -> T #(Error, x2)
  | Error, Error -> T #(Error, err x1 x2)
;;

let%template[@mode local] combine
  (type ok1 ok2 ok3 err)
  (T #(tag1, x1) : (ok1, err) t)
  (T #(tag2, x2) : (ok2, err) t)
  ~(ok : ok1 @ local -> ok2 @ local -> ok3 @ local)
  ~(err : err @ local -> err @ local -> err @ local)
  : (ok3, err) t
  = exclave_
  match tag1, tag2 with
  | Ok, Ok -> T #(Ok, ok x1 x2)
  | Error, Ok -> T #(Error, x1)
  | Ok, Error -> T #(Error, x2)
  | Error, Error -> T #(Error, err x1 x2)
;;

let try_with f =
  try T #(Ok, f ()) with
  | exn -> T #(Error, exn)
;;

let[@inline] ( >>| ) a f = map a ~f
let[@inline] ( >>= ) a f = bind a ~f

module Let_syntax = struct
  let return = [%eta1 return]
  let ( >>| ) = ( >>| )
  let ( >>= ) = ( >>= )

  module Let_syntax = struct
    let return = [%eta1 return]
    let bind = bind
    let map = map
    let both = combine ~ok:(fun x y -> x, y) ~err:(fun x _ -> x)

    module Open_on_rhs = struct end
  end
end

module Local = struct
  module Let_syntax = struct
    let return = [%eta1 exclave_ return [@mode local]]
    let[@inline] ( >>= ) a f = exclave_ (bind [@mode local]) a ~f
    let[@inline] ( >>| ) a f = exclave_ (map [@mode local]) a ~f

    module Let_syntax = struct
      let return = [%eta1 exclave_ return [@mode local]]
      let bind = (bind [@mode local])
      let map = (map [@mode local])

      let both =
        (combine [@mode local]) ~ok:(fun x y -> exclave_ x, y) ~err:(fun x _ -> x)
      ;;

      module Open_on_rhs = struct end
    end
  end
end

module Monad_infix = struct
  let ( >>| ) = ( >>| )
  let ( >>= ) = ( >>= )
end
