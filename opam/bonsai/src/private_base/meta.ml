open! Core
open! Import

let unit_var_id = Var_id.create ()
let nothing_var_id = Var_id.create ()

module type Type_id = sig
  type 'a t [@@deriving sexp_of]

  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val to_var_id : 'a t -> 'a Var_id.t
  val nothing : Nothing.t t
  val unit : unit t
end

module Model = struct
  type 'a id =
    | Leaf :
        { var_id : 'a Var_id.t
        ; name : string
        }
        -> 'a id
    | Tuple :
        { a : 'a id
        ; b : 'b id
        }
        -> ('a * 'b) id
    | Map :
        { k : 'k Var_id.t
        ; cmp : 'cmp Var_id.t
        ; by : 'result id
        }
        -> ('k, 'result, 'cmp) Map.t id
    | Map_on :
        { k_model : 'k_model Var_id.t
        ; k_io : 'k_io Var_id.t
        ; cmp : 'cmp_model Var_id.t
        ; by : 'result id
        }
        -> ('k_model, 'k_io * 'result, 'cmp_model) Map.t id
    | Multi_model : { multi_model : hidden Int.Map.t } -> hidden Int.Map.t id

  and 'a t =
    { default : 'a
    ; equal : 'a -> 'a -> bool
    ; type_id : 'a id
    ; sexp_of : 'a -> Sexp.t
    }

  and hidden =
    | T :
        { model : 'm
        ; info : 'm t
        }
        -> hidden

  module Type_id = struct
    type 'a t = 'a id

    let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
      fun _sexp_of_a -> function
      | Leaf { name; _ } -> [%sexp (name : string)]
      | Tuple { a; b } -> [%sexp (a : opaque t), (b : opaque t)]
      | Map { by; _ } -> [%sexp (by : opaque t)]
      | Map_on { by; _ } -> [%sexp (by : opaque t)]
      | Multi_model { multi_model } ->
        let sexp_of_hidden (T { info = { type_id; _ }; _ }) =
          [%sexp (type_id : opaque t)]
        in
        [%sexp (multi_model : hidden Int.Map.t)]
    ;;

    exception Fail

    let var_id_same_witness = Var_id.same_witness

    let rec same_witness : type a b. a t -> b t -> (a, b) Type_equal.t option =
      fun a b ->
      match a, b with
      | Leaf a, Leaf b -> var_id_same_witness a.var_id b.var_id
      | Tuple a, Tuple b ->
        let%bind.Option T = same_witness a.a b.a in
        let%bind.Option T = same_witness a.b b.b in
        Some (Type_equal.T : (a, b) Type_equal.t)
      | Map a, Map b ->
        let%bind.Option T = var_id_same_witness a.k b.k in
        let%bind.Option T = var_id_same_witness a.cmp b.cmp in
        let%bind.Option T = same_witness a.by b.by in
        Some (Type_equal.T : (a, b) Type_equal.t)
      | Map_on a, Map_on b ->
        let%bind.Option T = var_id_same_witness a.k_io b.k_io in
        let%bind.Option T = var_id_same_witness a.k_model b.k_model in
        let%bind.Option T = var_id_same_witness a.cmp b.cmp in
        let%bind.Option T = same_witness a.by b.by in
        Some (Type_equal.T : (a, b) Type_equal.t)
      | Multi_model a, Multi_model b ->
        if Map.equal
             (fun (T a) (T b) ->
               match same_witness a.info.type_id b.info.type_id with
               | None -> false
               | Some T -> true)
             a.multi_model
             b.multi_model
        then Some Type_equal.T
        else None
      | Leaf _, Tuple _
      | Leaf _, Map _
      | Leaf _, Map_on _
      | Leaf _, Multi_model _
      | Tuple _, Leaf _
      | Tuple _, Map _
      | Tuple _, Map_on _
      | Tuple _, Multi_model _
      | Map _, Leaf _
      | Map _, Tuple _
      | Map _, Map_on _
      | Map _, Multi_model _
      | Map_on _, Leaf _
      | Map_on _, Tuple _
      | Map_on _, Map _
      | Map_on _, Multi_model _
      | Multi_model _, Leaf _
      | Multi_model _, Tuple _
      | Multi_model _, Map _
      | Multi_model _, Map_on _ -> None
    ;;

    let same_witness_exn a b =
      match same_witness a b with
      | None -> raise_notrace Fail
      | Some proof -> proof
    ;;

    let to_var_id _ = Var_id.create ()
    let unit = Leaf { var_id = unit_var_id; name = "unit" }
    let nothing = Leaf { var_id = nothing_var_id; name = "Nothing.t" }
  end

  let unit =
    { type_id = Type_id.unit; default = (); equal = equal_unit; sexp_of = sexp_of_unit }
  ;;

  let both model1 model2 =
    let sexp_of = Tuple2.sexp_of_t model1.sexp_of model2.sexp_of in
    let type_id = Tuple { a = model1.type_id; b = model2.type_id } in
    let default = model1.default, model2.default in
    let equal = Tuple2.equal ~eq1:model1.equal ~eq2:model2.equal in
    { type_id; default; equal; sexp_of }
  ;;

  let map
    (type k cmp)
    (module M : Comparator.S with type t = k and type comparator_witness = cmp)
    k
    cmp
    model
    =
    let sexp_of_model = model.sexp_of in
    let module M = struct
      include M

      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let sexp_of_map_model = [%sexp_of: model Map.M(M).t] in
    let model_map_type_id = Map { k; cmp; by = model.type_id } in
    { type_id = model_map_type_id
    ; default = Map.empty (module M)
    ; equal = Map.equal model.equal
    ; sexp_of = sexp_of_map_model
    }
  ;;

  let map_on
    (type k cmp k_io cmp_io)
    (module M : Comparator.S with type t = k and type comparator_witness = cmp)
    (module M_io : Comparator.S with type t = k_io and type comparator_witness = cmp_io)
    k_model
    k_io
    cmp
    model
    =
    let module M = struct
      include M

      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let module M_io = struct
      include M_io

      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let sexp_of_model = model.sexp_of in
    let sexp_of_map_model = [%sexp_of: (M_io.t * model) Map.M(M).t] in
    let model_map_type_id = Map_on { k_model; k_io; cmp; by = model.type_id } in
    let io_equal a b = (Comparator.compare M_io.comparator) a b = 0 in
    { type_id = model_map_type_id
    ; default = Map.empty (module M)
    ; equal = Map.equal (Tuple2.equal ~eq1:io_equal ~eq2:model.equal)
    ; sexp_of = sexp_of_map_model
    }
  ;;

  let of_module ~sexp_of_model ~equal ~default ~name =
    let equal = Option.value ~default:[%eta2 phys_equal] equal in
    let var_id = Var_id.create () in
    { type_id = Leaf { var_id; name = sprintf "%s-model" name }
    ; default
    ; equal
    ; sexp_of = sexp_of_model
    }
  ;;

  module Hidden = struct
    type 'a model = 'a t

    type t = hidden =
      | T :
          { model : 'm
          ; info : 'm model
          }
          -> t

    let sexp_of_t (T { model; info = { sexp_of; _ }; _ }) = sexp_of model

    let equal
      (T { model = m1; info = { type_id = t1; equal; _ }; _ })
      (T { model = m2; info = { type_id = t2; _ }; _ })
      =
      match Type_id.same_witness t1 t2 with
      | Some T -> equal m1 m2
      | None -> false
    ;;

    let create (info : _ model) =
      let wrap m = T { model = m; info } in
      wrap
    ;;

    let lazy_ =
      { default = None
      ; equal = [%equal: t option]
      ; type_id = Leaf { var_id = Var_id.create (); name = "lazy-model" }
      ; sexp_of = [%sexp_of: t option]
      }
    ;;
  end
end

module Multi_model = struct
  type t = Model.Hidden.t Int.Map.t

  let sexp_of_t (type k) (sexp_of_k : k -> Sexp.t) =
    let module K = struct
      type t = k [@@deriving sexp_of]
    end
    in
    [%sexp_of: Model.Hidden.t Map.M(K).t]
  ;;

  let find_exn t key = Map.find_exn t key
  let set = Map.set
  let to_models, of_models = Fn.id, Fn.id

  let model_info default =
    let sexp_of = [%sexp_of: int t] in
    let type_id = Model.Multi_model { multi_model = default } in
    ({ default; type_id; equal = [%equal: Model.Hidden.t Int.Map.t]; sexp_of }
     : t Model.t)
  ;;
end

module Input = struct
  module Type_id = Model.Type_id

  type 'a t = 'a Type_id.t [@@deriving sexp_of]

  let same_witness = Type_id.same_witness
  let same_witness_exn = Type_id.same_witness_exn
  let unit = Type_id.unit
  let create ?(name = "input") () = Model.Leaf { var_id = Var_id.create (); name }
  let both a b = Model.Tuple { a; b }
  let map k cmp by = Model.Map { k; cmp; by }

  module Hidden = struct
    type 'a input = 'a t

    type 'key t =
      | T :
          { input : 'input
          ; type_id : 'input input
          ; key : 'key
          }
          -> 'key t

    let unit : unit t input = Leaf { var_id = Var_id.create (); name = "lazy input" }
    let int : int t input = Leaf { var_id = Var_id.create (); name = "enum input" }
  end
end
