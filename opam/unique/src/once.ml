open! Import

type 'a t = 'a or_null Ref.t

let[@inline] make v = Ref.make (This v)
let[@inline] take_or_null t = Ref.exchange t Null

let already_accessed fname =
  match failwith (Format.sprintf "Once.%s failed: already accessed" fname) with
  | (_ : nothing) -> .
;;

let[@inline] take_exn t =
  match Ref.exchange t Null with
  | Null -> already_accessed "take_exn"
  | This v -> v
;;

(* We implement is_taken twice: once using mutation, to prove it can be done safely, and
   once using magic, to avoid the overhead of the extra [caml_modify] calls for the
   mutation *)

module type Is_taken = sig
  val is_taken : 'a t @ local -> bool
end

module Is_taken__slow : Is_taken = struct
  let is_taken t =
    match take_or_null t with
    | Null -> true
    | This a ->
      Ref.set t (This a);
      false
  ;;
end

module Is_taken__fast : Is_taken = struct
  let is_taken t =
    match t |> magic_unique |> Ref.get with
    | Null -> true
    | This _ -> false
  ;;
end

module _ = Is_taken__slow
include Is_taken__fast

let borrow_or_null (type a : value mod many) (t : a t) ~f =
  match take_or_null t with
  | Null -> Null
  | This a ->
    (match f (borrow_ a) with
     | res ->
       Ref.set t (This a);
       This res
     | exception exn ->
       let bt = Stdlib.Printexc.get_raw_backtrace () in
       Ref.set t (This a);
       Stdlib.Printexc.raise_with_backtrace exn bt)
;;

let borrow_exn (type a : value mod many) (t : a t) ~f =
  match take_or_null t with
  | Null -> already_accessed "borrow_exn"
  | This a ->
    (match f (borrow_ a) with
     | res ->
       Ref.set t (This a);
       res
     | exception exn ->
       let bt = Stdlib.Printexc.get_raw_backtrace () in
       Ref.set t (This a);
       (match Stdlib.Printexc.raise_with_backtrace exn bt with
        | (_ : nothing) -> .))
;;

module Atomic = struct
  type 'a t = 'a or_null Atomic.t

  let[@inline] make v = Atomic.make (This v)
  let[@inline] take_or_null t = Atomic.exchange t Null

  let[@inline] take_exn t =
    match Atomic.exchange t Null with
    | Null -> already_accessed "Atomic.take_exn"
    | This v -> v
  ;;

  (* We implement is_taken twice: once using mutation, to prove it can be done safely, and
     once using magic, to avoid the overhead of the extra [caml_modify] calls for the
     mutation *)

  module type Is_taken = sig
    val is_taken : 'a t @ local -> bool
  end

  module Is_taken__slow : Is_taken = struct
    let is_taken t =
      match take_or_null t with
      | Null -> true
      | This a ->
        Atomic.set t (This a);
        false
    ;;
  end

  module Is_taken__fast : Is_taken = struct
    let is_taken t =
      match t |> magic_unique |> Atomic.get with
      | Null -> true
      | This _ -> false
    ;;
  end

  module _ = Is_taken__slow
  include Is_taken__fast

  let borrow_or_null (type a : value mod many) (t : a t) ~f =
    match take_or_null t with
    | Null -> Null
    | This a ->
      (match f (borrow_ a) with
       | res ->
         Atomic.set t (This a);
         This res
       | exception exn ->
         let bt = Stdlib.Printexc.get_raw_backtrace () in
         Atomic.set t (This a);
         Stdlib.Printexc.raise_with_backtrace exn bt)
  ;;

  let borrow_exn (type a : value mod many) (t : a t) ~f =
    match take_or_null t with
    | Null -> already_accessed "Atomic.borrow_exn"
    | This a ->
      (match f (borrow_ a) with
       | res ->
         Atomic.set t (This a);
         res
       | exception exn ->
         let bt = Stdlib.Printexc.get_raw_backtrace () in
         Atomic.set t (This a);
         Stdlib.Printexc.raise_with_backtrace exn bt)
  ;;
end

module Local = struct
  type 'a t = 'a or_null Ref.Local.t

  let make v = exclave_ Ref.Local.make (This v)
  let take_or_null t = exclave_ Ref.Local.exchange_global t Null

  let take_exn t = exclave_
    match take_or_null t with
    | Null -> already_accessed "Local.take_exn"
    | This v -> v
  ;;
end
