open! Import
module Atomic = Basement.Portable_atomic
module Backoff = Basement.Stdlib_shim.Backoff

(* SAFETY: This data structure must not expose operations that would duplicate elems. *)

type 'a t = 'a list Atomic.t

let create ?padded () = Atomic.make ?padded []

let rec push t x backoff =
  let xs = Atomic.get t in
  let xxs = x :: xs in
  match Atomic.compare_and_set t xs xxs with
  | true -> ()
  | false -> push t x (Backoff.once backoff)
;;

let rec pop_or_null t backoff =
  let xxs = Atomic.get t in
  match xxs with
  | [] -> Null
  | x :: xs ->
    (match Atomic.compare_and_set t xxs xs with
     | true -> This x
     | false -> pop_or_null t (Backoff.once backoff))
;;

let[@inline] exchange t xs = Atomic.exchange t xs
let[@inline] push t x = push t (magic_many x) Backoff.default
let[@inline] pop_or_null t = magic_unique__portended (pop_or_null t Backoff.default)
let[@inline] is_empty t = phys_equal [] (Atomic.get t)
let[@inline] exchange t xs = magic_unique__portended (exchange t (magic_many xs))
let[@inline] pop_all t = exchange t []
