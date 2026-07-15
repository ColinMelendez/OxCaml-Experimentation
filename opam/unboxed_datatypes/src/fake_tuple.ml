module%template T2 = struct
  [@@@kind.default ka = value]
  [@@@kind.default kb = (value, float64, immediate64)]

  type ('a : ka, 'b : kb) t =
    { fst : 'a
    ; snd : 'b
    }
  [@@deriving equal ~localize, compare ~localize, globalize]

  let create a b = { fst = a; snd = b } [@exclave_if_stack a] [@@alloc a = (heap, stack)]

  (* manually derive sexp as if the type definition is the following (but it can't
     actually be, because we don't support non-values in tuples): type ('a : ka, 'b : kb)
     t = 'a * 'b [@@deriving_inline sexp]
  *)

  let t_of_sexp
    : ('a : ka) ('b : kb).
    (Sexplib0.Sexp.t -> 'a)
    -> (Sexplib0.Sexp.t -> 'b)
    -> Sexplib0.Sexp.t
    -> (('a, 'b) t[@kind ka kb])
    =
    let error_source__009_ = "fake_tuple.ml.t" in
    fun _of_a__001_ _of_b__002_ -> function
      | Sexplib0.Sexp.List [ arg0__004_; arg1__005_ ] ->
        let res0__006_ = _of_a__001_ arg0__004_
        and res1__007_ = _of_b__002_ arg1__005_ in
        { fst = res0__006_; snd = res1__007_ }
      | sexp__008_ ->
        Sexplib0.Sexp_conv_error.tuple_of_size_n_expected error_source__009_ 2 sexp__008_
  ;;

  let sexp_of_t
    : ('a : ka) ('b : kb).
    ('a -> Sexplib0.Sexp.t)
    -> ('b -> Sexplib0.Sexp.t)
    -> (('a, 'b) t[@kind ka kb])
    -> Sexplib0.Sexp.t
    =
    fun _of_a__010_ _of_b__011_ { fst = arg0__012_; snd = arg1__013_ } ->
    let res0__014_ = _of_a__010_ arg0__012_
    and res1__015_ = _of_b__011_ arg1__013_ in
    Sexplib0.Sexp.List [ res0__014_; res1__015_ ]
  ;;

  [@@@mode.default m = (global, local)]

  let get1 t = t.fst
  let get2 t = t.snd
end

module%template T3 = struct
  [@@@kind.default ka = value]
  [@@@kind.default kb = value]
  [@@@kind.default kc = (value, float64, immediate64)]

  type ('a : ka, 'b : kb, 'c : kc) t =
    { fst : 'a
    ; snd : 'b
    ; trd : 'c
    }
  [@@deriving equal ~localize, compare ~localize, globalize]

  let create a b c = { fst = a; snd = b; trd = c } [@exclave_if_stack a]
  [@@alloc a = (heap, stack)]
  ;;

  (* manually derive sexp as if the type definition is the following (but it can't
     actually be, because we don't support non-values in tuples): type ('a : ka, 'b : kb,
     'c : kc) t = 'a * 'b * 'c [@@deriving_inline sexp]
  *)

  let t_of_sexp
    : ('a : ka) ('b : kb) ('c : kc).
    (Sexplib0.Sexp.t -> ('a : ka))
    -> (Sexplib0.Sexp.t -> ('b : kb))
    -> (Sexplib0.Sexp.t -> ('c : kc))
    -> Sexplib0.Sexp.t
    -> (('a : ka, 'b : kb, 'c : kc) t[@kind ka kb kc])
    =
    let error_source__087_ = "fake_tuple.ml.t" in
    fun _of_a__076_ _of_b__077_ _of_c__078_ -> function
      | Sexplib0.Sexp.List [ arg0__080_; arg1__081_; arg2__082_ ] ->
        let res0__083_ = _of_a__076_ arg0__080_
        and res1__084_ = _of_b__077_ arg1__081_
        and res2__085_ = _of_c__078_ arg2__082_ in
        { fst = res0__083_; snd = res1__084_; trd = res2__085_ }
      | sexp__086_ ->
        Sexplib0.Sexp_conv_error.tuple_of_size_n_expected error_source__087_ 3 sexp__086_
  ;;

  let sexp_of_t
    : ('a : ka) ('b : kb) ('c : kc).
    (('a : ka) -> Sexplib0.Sexp.t)
    -> (('b : kb) -> Sexplib0.Sexp.t)
    -> (('c : kc) -> Sexplib0.Sexp.t)
    -> (('a : ka, 'b : kb, 'c : kc) t[@kind ka kb kc])
    -> Sexplib0.Sexp.t
    =
    fun _of_a__088_
      _of_b__089_
      _of_c__090_
      { fst = arg0__091_; snd = arg1__092_; trd = arg2__093_ } ->
    let res0__094_ = _of_a__088_ arg0__091_
    and res1__095_ = _of_b__089_ arg1__092_
    and res2__096_ = _of_c__090_ arg2__093_ in
    Sexplib0.Sexp.List [ res0__094_; res1__095_; res2__096_ ]
  ;;

  [@@@mode.default m = (global, local)]

  let get1 t = t.fst
  let get2 t = t.snd
  let get3 t = t.trd
end
