open Base

[%%template
[@@@kind k_triple = (bits64 & bits64 & value_or_null)]

(* unary *)
include struct
  type none : k = int [@@kind k = value]
  type none : k = int8# [@@kind k = bits8]
  type none : k = int16# [@@kind k = bits16]
  type none : k = int32# [@@kind k = bits32]
  type none : k = int64# [@@kind k = bits64]
  type none : k = nativeint# [@@kind k = word]
  type none : k = float32# [@@kind k = float32]
  type none : k = float# [@@kind k = float64]
  type none : void mod everything [@@kind k = void]

  let[@kind k = value] none () = 0
  let[@kind k = bits8] none () = #0s
  let[@kind k = bits16] none () = #0S
  let[@kind k = bits32] none () = #0l
  let[@kind k = bits64] none () = #0L
  let[@kind k = word] none () = #0n
  let[@kind k = float32] none () = #0.0s
  let[@kind k = float64] none () = #0.0

  external void : unit -> (none[@kind void]) @@ portable = "%unbox_unit"

  let[@kind k = void] none () = void ()
end

(* binary *)
include struct
  [@@@kind_set
    (k1s, k2s)
    = ( (base_or_null, base_or_null)
      , (base_or_null, value_or_null & value_or_null)
      , (value_or_null & base_or_null, base_or_null)
      , (value_or_null & base_or_null, value_or_null & value_or_null)
      , (value_or_null & bits64, (value_or_null & bits64) & word)
      , (bits64, bits64 & word) )]

  [@@@kind k1 = k1s, k2 = k2s]

  type none : k1 & k2 = #((none[@kind k1]) * (none[@kind k2])) [@@kind k = (k1 & k2)]

  let[@kind k = (k1 & k2)] none () : (none[@kind k]) =
    let left = (none [@kind k1]) () in
    let right = (none [@kind k2]) () in
    #(left, right)
  ;;
end

include struct
  [@@@kind kl = (k_triple, value_or_null, float64, bits64)]

  open struct
    type none : k =
      #((none[@kind bits64]) * (none[@kind bits64]) * (none[@kind value_or_null]))
    [@@kind k = k_triple]
  end

  type none : k = #((none[@kind kl]) * (none[@kind k_triple]))
  [@@kind k = (kl & k_triple)]

  let[@kind k = k_triple] none () : (none[@kind k]) =
    #((none [@kind bits64]) (), (none [@kind bits64]) (), (none [@kind value_or_null]) ())
  ;;

  let[@kind k = (kl & k_triple)] none () : (none[@kind k]) =
    let left = (none [@kind kl]) () in
    let right = (none [@kind k_triple]) () in
    #(left, right)
  ;;
end

include struct
  [@@@kind.default
    k
    = ( base_or_null
      , bits8
      , bits16
      , void
      , base_or_null & base_or_null
      , (value_or_null & base_or_null) & base_or_null
      , (_ : (_ : base_or_null & (value_or_null & value_or_null)))
      , (_ : (_ : (value_or_null & base_or_null) & (value_or_null & value_or_null)))
      , (value_or_null, float64, bits64, k_triple) & k_triple
      , (_ : (_ : (value_or_null & bits64) & ((value_or_null & bits64) & word)))
      , (_ : (_ : bits64 & (bits64 & word))) )]

  type ('a : k, 'b : k) tag =
    | None : ('a : k). (('a, (none[@kind k])) tag[@kind k])
    | Some : ('a : k). (('a, 'a) tag[@kind k])

  type ('a : k) t : immediate & k =
    | T : ('a : k) ('b : k). #((('a, 'b) tag[@kind k]) * 'b) -> ('a t[@kind k])
  [@@unboxed]

  let%template some (x @ m) = T #(Some, x) [@exclave_if_local m]
  [@@kind k] [@@mode m = (global, local)]
  ;;

  let%template none () = T #(None, (none [@kind k]) ()) [@@kind k]

  let is_some (type a : k) t =
    match (t : (a t[@kind k])) with
    | T #(Some, _) -> true
    | T #(None, _) -> false
  ;;

  let is_none (type a : k) t =
    match (t : (a t[@kind k])) with
    | T #(Some, _) -> false
    | T #(None, _) -> true
  ;;

  let globalize (type a : k) (globalize_v : a @ local -> a) t =
    match (t : (a t[@kind k])) with
    | T #(None, _) -> (none [@kind k]) ()
    | T #(Some, v) -> (some [@kind k]) (globalize_v v)
  ;;

  (* Based on [lib/base/src/option.ml] *)
  let%template[@kind k = k] [@alloc a @ m = (heap @ global, stack @ local)] sexp_of_t
    (type a : k)
    (sexp_of_a : a @ m -> Sexp.t @ m)
    t
    : Sexplib0.Sexp.t
    =
    let write_old_option_format = Dynamic.get Sexplib.Conv.write_old_option_format in
    match[@exclave_if_stack a] (t : (a t[@kind k])) with
    | T #(Some, x) when write_old_option_format -> List [ sexp_of_a x ]
    | T #(Some, x) -> List [ Atom "some"; sexp_of_a x ]
    | T #(None, _) when write_old_option_format -> List []
    | T #(None, _) -> Atom "none"
  ;;

  (* Based on [lib/base/src/option.ml] *)
  let t_of_sexp (type a : k) a_of_sexp (sexp : Sexplib0.Sexp.t) : (a t[@kind k]) =
    if Dynamic.get Sexplib.Conv.read_old_option_format
    then (
      match sexp with
      | List [] | Atom ("none" | "None") -> (none [@kind k]) ()
      | List [ el ] | List [ Atom ("some" | "Some"); el ] ->
        (some [@kind k]) (a_of_sexp el)
      | List _ ->
        (match
           Sexplib.Conv.of_sexp_error
             "option_u_of_sexp: list must represent optional value"
             sexp
         with
         | (_ : Nothing.t) -> .)
      | Atom _ ->
        (match
           Sexplib.Conv.of_sexp_error "option_u_of_sexp: only none can be atom" sexp
         with
         | (_ : Nothing.t) -> .))
    else (
      match sexp with
      | Atom ("none" | "None") -> (none [@kind k]) ()
      | List [ Atom ("some" | "Some"); el ] -> (some [@kind k]) (a_of_sexp el)
      | Atom _ ->
        (match
           Sexplib.Conv.of_sexp_error "option_u_of_sexp: only none can be atom" sexp
         with
         | (_ : Nothing.t) -> .)
      | List _ ->
        (match
           Sexplib.Conv.of_sexp_error "option_u_of_sexp: list must be (some el)" sexp
         with
         | (_ : Nothing.t) -> .))
  ;;
end

include struct
  [@@@kind.default k = base_or_null]
  [@@@mode.default c = (uncontended, shared, contended)]

  let box (type a : k) (t : (a t[@kind k])) : (a Option.t[@kind k]) =
    match t with
    | T #(None, _) -> None
    | T #(Some, x) -> Some x
  ;;

  let unbox (type a : k) (t : (a Option.t[@kind k])) : (a t[@kind k]) =
    match t with
    | None -> (none [@kind k]) ()
    | Some x -> T #(Some, x)
  ;;
end]
