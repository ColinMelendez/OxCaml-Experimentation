open Base

(** An unboxed version of {!Base.Result} with layout [immediate & value].

    - Most functions in this module also have local versions defined using, and accessible
      by, [ppx_template].

    - Let syntax is exposed in {!Let_syntax}.

    - Local let syntax is exposed in {!Local.Let_syntax}. E.g.:
      {[
        let%template local_example ~hello ~world = exclave_
          let open Result_u.Local.Let_syntax in
          let%bindl hello =
            match hello with
            | None -> Result_u.fail "missing hello"
            | Some x -> Result_u.return x
          in
          let%bindl world =
            match world with
            | None -> Result_u.fail "missing world"
            | Some x -> Result_u.return x
          in
          (Result_u.return [@mode local]) (hello, world)
        ;;
      ]} *)

[%%template:
[@@@kind.default k = (value & value, value)]

type ('ok : k, 'err : k, 'payload : k) tag =
  | Ok : (('ok, 'err, 'ok) tag[@kind k])
  | Error : (('ok, 'err, 'err) tag[@kind k])

type ('ok : k, 'err : k) t =
  | T : #((('ok, 'err, 'payload) tag[@kind k]) * 'payload) -> (('ok, 'err) t[@kind k])
[@@unboxed]
[@@deriving
  bin_io ~localize, compare ~localize, equal ~localize, globalize, hash, sexp ~stackify]

[@@@mode.default m = (global, local)]

val return : ('ok : k). 'ok @ m -> (('ok, _) t[@kind k]) @ m [@@zero_alloc]
val fail : ('err : k). 'err @ m -> ((_, 'err) t[@kind k]) @ m [@@zero_alloc]]

[%%template:
[@@@mode.default m = (global, local)]

(** Explicit replacement for pattern matching, as the representation of [t] is cumbersome.
    If [t] is [Ok x], this returns [ok x], otherwise [t] is [Error x] and this returns
    [err x]. *)
val match_
  :  ('ok, 'err) t @ m
  -> ok:('ok @ m -> 'a @ m) @ local
  -> err:('err @ m -> 'a @ m) @ local
  -> 'a @ m

val to_result : ('ok, 'err) t @ m -> ('ok, 'err) Result.t @ m
val of_result : ('ok, 'err) Result.t @ m -> ('ok, 'err) t @ m [@@zero_alloc]

val bind
  :  ('ok1, 'err) t @ m
  -> f:('ok1 @ m -> ('ok2, 'err) t @ m) @ local
  -> ('ok2, 'err) t @ m

val ignore_m : (_, 'err) t @ m -> (unit, 'err) t @ m [@@zero_alloc]

val invariant
  :  ('ok @ m -> unit) @ local
  -> ('err @ m -> unit) @ local
  -> ('ok, 'err) t @ m
  -> unit

val ok : ('ok, _) t @ m -> 'ok option @ m
val error : (_, 'err) t @ m -> 'err option @ m
val of_option : 'ok option @ m -> error:'err @ m -> ('ok, 'err) t @ m [@@zero_alloc]

val of_option_or_thunk
  :  'ok option @ m
  -> error:(unit -> 'err @ m) @ local
  -> ('ok, 'err) t @ m

val iter : ('ok, _) t @ m -> f:('ok @ m -> unit) @ local -> unit
val iter_error : (_, 'err) t @ m -> f:('err @ m -> unit) @ local -> unit
val map : ('ok, 'err) t @ m -> f:('ok @ m -> 'c @ m) @ local -> ('c, 'err) t @ m
val map_error : ('ok, 'err) t @ m -> f:('err @ m -> 'c @ m) @ local -> ('ok, 'c) t @ m

val combine
  :  ('ok1, 'err) t @ m
  -> ('ok2, 'err) t @ m
  -> ok:('ok1 @ m -> 'ok2 @ m -> 'ok3 @ m) @ local
  -> err:('err @ m -> 'err @ m -> 'err @ m) @ local
  -> ('ok3, 'err) t @ m

val to_either : ('ok, 'err) t @ m -> ('ok, 'err) Either.t @ m
val of_either : ('ok, 'err) Either.t @ m -> ('ok, 'err) t @ m [@@zero_alloc]
val to_either_u : ('ok, 'err) t @ m -> ('ok, 'err) Either_u.t @ m [@@zero_alloc]
val of_either_u : ('ok, 'err) Either_u.t @ m -> ('ok, 'err) t @ m [@@zero_alloc]
val ok_if_true : bool -> error:'err @ m -> (unit, 'err) t @ m [@@zero_alloc]

(* template end *)]

val is_ok : (_, _) t @ local -> bool [@@zero_alloc]
val is_error : (_, _) t @ local -> bool [@@zero_alloc]
val ok_exn : ('ok, exn) t -> 'ok [@@zero_alloc]
val ok_or_failwith : ('ok, string) t -> 'ok [@@zero_alloc]
val try_with : (unit -> 'a) @ local -> ('a, exn) t
val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) @ local -> ('b, 'e) t
val ( >>| ) : ('a, 'e) t -> ('a -> 'b) @ local -> ('b, 'e) t

module Let_syntax : sig
  val return : 'a -> ('a, _) t [@@zero_alloc]
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) @ local -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) @ local -> ('b, 'e) t

  module Let_syntax : sig
    val return : 'a -> ('a, _) t [@@zero_alloc]
    val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) @ local -> ('b, 'e) t
    val map : ('a, 'e) t -> f:('a -> 'b) @ local -> ('b, 'e) t
    val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

    module Open_on_rhs : sig end
  end
end

module Local : sig
  module Let_syntax : sig
    val return : 'a @ local -> ('a, _) t @ local [@@zero_alloc]

    val ( >>= )
      :  ('a, 'e) t @ local
      -> ('a @ local -> ('b, 'e) t @ local) @ local
      -> ('b, 'e) t @ local

    val ( >>| )
      :  ('a, 'e) t @ local
      -> ('a @ local -> 'b @ local) @ local
      -> ('b, 'e) t @ local

    module Let_syntax : sig
      val return : 'a @ local -> ('a, _) t @ local [@@zero_alloc]

      val bind
        :  ('a, 'e) t @ local
        -> f:('a @ local -> ('b, 'e) t @ local) @ local
        -> ('b, 'e) t @ local

      val map
        :  ('a, 'e) t @ local
        -> f:('a @ local -> 'b @ local) @ local
        -> ('b, 'e) t @ local

      val both : ('a, 'e) t @ local -> ('b, 'e) t @ local -> ('a * 'b, 'e) t @ local

      module Open_on_rhs : sig end
    end
  end
end

module Monad_infix : sig
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) @ local -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) @ local -> ('b, 'e) t
end
