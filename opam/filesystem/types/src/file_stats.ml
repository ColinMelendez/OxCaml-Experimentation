open! Core
include File_stats_intf

module Int64_u = struct
  type t = int64#

  external of_int64
    :  (int64[@local_opt])
    -> (int64#[@unboxed])
    @@ portable
    = "%unbox_int64"

  external to_int64 : (int64#[@unboxed]) -> (int64[@local_opt]) @@ portable = "%box_int64"

  let sexp_of_t t = Int64.sexp_of_t (to_int64 t)

  [%%template
    let equal (x @ m) (y @ m) =
      (Int64.equal [@mode m]) (to_int64 x) (to_int64 y) [@nontail]
    [@@mode m = local]
    ;;]

  open Base_quickcheck

  let quickcheck_generator =
    (Generator.Via_thunk.map [@mode portable]) Generator.int64 ~f:(fun f () ->
      of_int64 (f ()))
  ;;

  let quickcheck_observer =
    (Observer.Via_thunk.unmap [@mode portable]) Observer.int64 ~f:(fun f () ->
      to_int64 (f ()))
  ;;

  let quickcheck_shrinker = Shrinker.atomic
end

module Or_testing = struct
  (** Hide nondeterministic or host-specific values during expect tests. *)
  type 'a t = 'a [@@deriving equal ~portable ~localize, quickcheck ~portable]

  let sexp_of_t sexp_of_a a =
    if am_running_test then Sexp.Atom "<hidden>" else sexp_of_a a
  ;;
end

type t =
  { host_device : int Or_testing.t
  ; inode : int Or_testing.t
  ; kind : File_kind.t
  ; permissions : File_permissions.t
  ; hard_links : int
  ; user_id : int Or_testing.t
  ; group_id : int Or_testing.t
  ; file_device : int Or_testing.t
  ; size : Int64_u.t
  ; access_time : Time_ns.t Or_testing.t
  ; modify_time : Time_ns.t Or_testing.t
  ; status_time : Time_ns.t Or_testing.t
  }
[@@deriving equal ~portable ~localize, quickcheck ~portable, sexp_of ~portable]

let size_in_byte_units_exn t = Byte_units.of_bytes_int64_exn (Int64_u.to_int64 t.size)
