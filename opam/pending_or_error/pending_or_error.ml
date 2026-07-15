module Stable = struct
  open Core.Core_stable

  module V1 = struct
    type 'a t =
      | Pending
      | Error of Error.V1.t
      | Ok of 'a
    [@@deriving sexp, bin_io, stable_witness]

    let%expect_test _ =
      print_endline [%bin_digest: unit t];
      [%expect {| e334ce2b399db41f0b4dfff798073875 |}]
    ;;
  end
end

open! Core

module Error = struct
  include Error

  let quickcheck_generator = Quickcheck.Generator.return (Error.of_string "error")
  let quickcheck_observer = Quickcheck.Observer.of_hash (module Error)
  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end

module T = struct
  type 'a t = 'a Stable.V1.t =
    | Pending
    | Error of Error.t
    | Ok of 'a [@quickcheck.weight 10.]
  [@@deriving
    bin_io, compare ~localize, diff, equal ~localize, quickcheck, sexp, variants]

  let of_or_error : 'a Or_error.t -> 'a t = function
    | Ok x -> Ok x
    | Error e -> Error e
  ;;

  let of_or_error_option : 'a Or_error.t option -> 'a t = function
    | None -> Pending
    | Some (Ok x) -> Ok x
    | Some (Error e) -> Error e
  ;;

  let to_or_error : 'a t -> 'a Or_error.t = function
    | Pending -> Or_error.error_string "Pending"
    | Error e -> Error e
    | Ok x -> Ok x
  ;;

  let to_or_error_option : 'a t -> 'a Or_error.t option = function
    | Pending -> None
    | Error e -> Some (Error e)
    | Ok x -> Some (Ok x)
  ;;

  let to_option : 'a t -> 'a option = function
    | Pending -> None
    | Error _ -> None
    | Ok x -> Some x
  ;;

  let error_s sexp = Error (Error.create_s sexp)

  let tag_error t ~tag =
    match t with
    | Pending -> Pending
    | Error error -> Error (Error.tag error ~tag)
    | Ok x -> Ok x
  ;;

  let map t ~f =
    match t with
    | Pending -> Pending
    | Error e -> Error e
    | Ok x -> Ok (f x)
  ;;

  let bind t ~f =
    match t with
    | Pending -> Pending
    | Error e -> Error e
    | Ok x -> f x
  ;;

  let return x = Ok x

  let value t ~default =
    match t with
    | Ok x -> x
    | Pending | Error _ -> default
  ;;

  let value_map t ~f ~default =
    match t with
    | Ok x -> f x
    | Pending | Error _ -> default
  ;;

  let merge a b ~f =
    match a, b with
    | Error a, Error b -> Error (Error.of_list [ a; b ])
    | Error a, (Ok _ | Pending) -> Error a
    | (Ok _ | Pending), Error b -> Error b
    | Ok a, Ok b -> Ok (f a b)
    | Ok a, Pending -> Ok a
    | Pending, Ok b -> Ok b
    | Pending, Pending -> Pending
  ;;

  let map2 a b ~f =
    match a, b with
    | Error a, Error b -> Error (Error.of_list [ a; b ])
    | Error a, (Ok _ | Pending) -> Error a
    | (Ok _ | Pending), Error b -> Error b
    | Ok a, Ok b -> Ok (f a b)
    | Ok _, Pending -> Pending
    | Pending, Ok _ -> Pending
    | Pending, Pending -> Pending
  ;;

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let map = `Custom map
      let bind = bind
      let return = return
    end)

  include Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let map2 = map2
      let map = `Custom map
      let return = return
    end)

  let map4 x1 x2 x3 x4 ~f =
    map2 (map3 x1 x2 x3 ~f:Tuple3.create) x4 ~f:(fun (x1, x2, x3) x4 -> f x1 x2 x3 x4)
  ;;

  let map5 x1 x2 x3 x4 x5 ~f =
    map3 (map3 x1 x2 x3 ~f:Tuple3.create) x4 x5 ~f:(fun (x1, x2, x3) x4 x5 ->
      f x1 x2 x3 x4 x5)
  ;;

  let map6 x1 x2 x3 x4 x5 x6 ~f =
    map2
      (map3 x1 x2 x3 ~f:Tuple3.create)
      (map3 x4 x5 x6 ~f:Tuple3.create)
      ~f:(fun (x1, x2, x3) (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)
  ;;

  let map7 x1 x2 x3 x4 x5 x6 x7 ~f =
    map3
      (map3 x1 x2 x3 ~f:Tuple3.create)
      (map3 x4 x5 x6 ~f:Tuple3.create)
      x7
      ~f:(fun (x1, x2, x3) (x4, x5, x6) x7 -> f x1 x2 x3 x4 x5 x6 x7)
  ;;

  let join_or_error t = t >>= fun t' -> of_or_error t'
end

include T

let of_map t =
  Map.fold
    t
    ~init:(Ok (Map.empty (Map.comparator_s t)))
    ~f:(fun ~key ~data accum ->
      let%map.T accum and data in
      Map.set accum ~key ~data)
;;
