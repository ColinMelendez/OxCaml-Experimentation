open! Core
open! Option.Let_syntax
include Insertion_ordered_map_intf

type ('key, 'a, 'cmp) t =
  { insertion_ordered_map : ('key * 'a) Int.Map.t
      (* Represented as a map (as opposed to a list) to enable O(log N) deletion (see
         [canonical_map]).

         It contains both keys and values to keep O(N) iteration complexity (as opposed to
         referring to [canonical_map] on each iteration, making complexity O(N log(N))).
      *)
  ; canonical_map : ('key, int * 'a, 'cmp) Map.t
      (* Used for insertion-order-invariant accessors (i.e. to ensure O(log N) lookup,
         addition, and deletion).

         It contains insertion indices to enable O(N) deletion from
         [insertion_ordered_map]. In particular, the first element of a key's tuple refers
         to its key in [insertion_ordered_map]. *)
  ; latest_index : int
  (* Contains the index of the most recently added element in [t]

     Each time a new element is added, [latest_index] is incremented, and the new value
     used as the element's index. [latest_index] is only ever incremented to avoid key
     clashes in [insertion_ordered_map]. *)
  }

module type S_plain =
  S_plain with type ('key, 'a, 'cmp) insertion_ordered_map := ('key, 'a, 'cmp) t

module type S = S with type ('key, 'a, 'cmp) insertion_ordered_map := ('key, 'a, 'cmp) t

module type S_binable =
  S_binable with type ('key, 'a, 'cmp) insertion_ordered_map := ('key, 'a, 'cmp) t

let empty comparator_module =
  { insertion_ordered_map = Map.empty (module Int)
  ; canonical_map = Map.empty comparator_module
  ; latest_index = 0 (* Read note in [of_alist_exn]. *)
  }
;;

let singleton comparator_module key data =
  { insertion_ordered_map = Int.Map.singleton 1 (key, data)
  ; canonical_map = Map.singleton comparator_module key (1, data)
  ; latest_index = 1 (* Read note in [of_alist_exn]. *)
  }
;;

let is_empty t = Map.is_empty t.insertion_ordered_map
let length t = Map.length t.insertion_ordered_map

let add { insertion_ordered_map; canonical_map; latest_index } ~key ~data =
  let next_index = latest_index + 1 in
  match Map.add canonical_map ~key ~data:(next_index, data) with
  | `Duplicate -> `Duplicate
  | `Ok canonical_map ->
    `Ok
      { insertion_ordered_map =
          Map.add_exn insertion_ordered_map ~key:next_index ~data:(key, data)
      ; canonical_map
      ; latest_index = next_index
      }
;;

let add_exn t ~key ~data =
  match add t ~key ~data with
  | `Ok t -> t
  | `Duplicate ->
    failwiths "duplicate key" key (Comparator.sexp_of_t (Map.comparator t.canonical_map))
;;

let find' t key = Map.find t.canonical_map key
let find t key = find' t key >>| snd

let find_exn t key =
  match find t key with
  | Some data -> data
  | None ->
    failwiths "key not found" key (Comparator.sexp_of_t (Map.comparator t.canonical_map))
;;

let remove ({ insertion_ordered_map; canonical_map; latest_index = _ } as t) key =
  match find' t key with
  | None -> t
  | Some (index, (_ : _)) ->
    let insertion_ordered_map = Map.remove insertion_ordered_map index in
    let canonical_map = Map.remove canonical_map key in
    { t with insertion_ordered_map; canonical_map }
;;

let set ({ insertion_ordered_map; canonical_map; latest_index = _ } as t) ~key ~data =
  match find' t key with
  | None -> add_exn t ~key ~data
  | Some (index, (_ : _)) ->
    let insertion_ordered_map =
      Map.set insertion_ordered_map ~key:index ~data:(key, data)
    in
    let canonical_map = Map.set canonical_map ~key ~data:(index, data) in
    { t with insertion_ordered_map; canonical_map }
;;

let update t key ~f =
  match find t key with
  | None -> add_exn t ~key ~data:(f None)
  | Some data -> set t ~key ~data:(f (Some data))
;;

let change t key ~f =
  let value = find t key in
  match value, f value with
  | None, None -> t
  | Some _, None -> remove t key
  | (None | Some _), Some data -> set t ~key ~data
;;

let mem t key = Map.mem t.canonical_map key

let exists t ~f =
  Map.exists t.canonical_map ~f:(fun ((_ : int), data) -> f data) [@nontail]
;;

let existsi t ~f =
  Map.existsi t.canonical_map ~f:(fun ~key ~data:((_ : int), data) -> f ~key ~data)
  [@nontail]
;;

let insertion_ordered_iter t ~(local_ f) =
  Map.iter t.insertion_ordered_map ~f:(fun (key, data) -> f key data) [@nontail]
;;

let iter_keys t ~f = insertion_ordered_iter t ~f:(fun key (_ : _) -> f key) [@nontail]
let iter t ~f = insertion_ordered_iter t ~f:(fun (_ : 'key) data -> f data) [@nontail]
let iteri t ~f = insertion_ordered_iter t ~f:(fun key data -> f ~key ~data) [@nontail]

let iteri_until t ~f =
  Map.iteri_until t.insertion_ordered_map ~f:(fun ~key:(_ : int) ~data:(key, data) ->
    f ~key ~data)
  [@nontail]
;;

let map ({ insertion_ordered_map; canonical_map; latest_index = _ } as t) ~f =
  let insertion_ordered_map =
    Map.map insertion_ordered_map ~f:(fun (key, data) -> key, f data)
  in
  let canonical_map = Map.map canonical_map ~f:(fun (index, data) -> index, f data) in
  { t with insertion_ordered_map; canonical_map }
;;

let filter ({ insertion_ordered_map; canonical_map; latest_index = _ } as t) ~(local_ f) =
  let insertion_ordered_map =
    Map.filter insertion_ordered_map ~f:(fun ((_ : 'key), data) -> f data)
  in
  let canonical_map = Map.filter canonical_map ~f:(fun ((_ : int), data) -> f data) in
  { t with insertion_ordered_map; canonical_map }
;;

let filteri ({ insertion_ordered_map; canonical_map; latest_index = _ } as t) ~(local_ f) =
  let insertion_ordered_map =
    Map.filteri insertion_ordered_map ~f:(fun ~key:(_ : int) ~data:(key, data) ->
      f ~key ~data)
  in
  let canonical_map =
    Map.filteri canonical_map ~f:(fun ~key ~data:((_ : int), data) -> f ~key ~data)
  in
  { t with insertion_ordered_map; canonical_map }
;;

let filter_map
  ({ insertion_ordered_map; canonical_map; latest_index = _ } as t)
  ~(local_ f)
  =
  let insertion_ordered_map =
    Map.filter_map insertion_ordered_map ~f:(fun (key, data) ->
      f data >>| Tuple2.create key)
  in
  let canonical_map =
    Map.filter_map canonical_map ~f:(fun (index, data) -> f data >>| Tuple2.create index)
  in
  { t with insertion_ordered_map; canonical_map }
;;

let fold t ~init ~f =
  Map.fold t.insertion_ordered_map ~init ~f:(fun ~key:(_ : int) ~data:(key, data) acc ->
    f ~key ~data acc)
  [@nontail]
;;

let fold_until t ~init ~f ~finish =
  Map.fold_until
    t.insertion_ordered_map
    ~init
    ~f:(fun ~key:(_ : int) ~data:(key, data) acc -> f ~key ~data acc)
    ~finish [@nontail]
;;

let fold_right t ~init ~f =
  Map.fold_right
    t.insertion_ordered_map
    ~init
    ~f:(fun ~key:(_ : int) ~data:(key, data) acc -> f ~key ~data acc)
  [@nontail]
;;

let min_elt t = Map.min_elt t.insertion_ordered_map >>| snd

let min_elt_exn t =
  match min_elt t with
  | None -> invalid_arg "[min_elt_exn] called with an empty map"
  | Some x -> x
;;

let max_elt t = Map.max_elt t.insertion_ordered_map >>| snd

let max_elt_exn t =
  match max_elt t with
  | None -> invalid_arg "[max_elt_exn] called with an empty map"
  | Some x -> x
;;

let nth t n = Map.nth t.insertion_ordered_map n >>| snd

let nth_exn t n =
  match nth t n with
  | None -> invalid_arg "[nth_exn] called with a map of size less than [n]"
  | Some x -> x
;;

let rank { insertion_ordered_map; canonical_map; latest_index = _ } key =
  Map.find canonical_map key >>| fst >>= Map.rank insertion_ordered_map
;;

let to_alist t = Map.data t.insertion_ordered_map
let keys t = t |> to_alist |> List.map ~f:fst
let data t = t |> to_alist |> List.map ~f:snd
let to_map t = Map.map t.canonical_map ~f:snd

let of_insertion_ordered_map comparator_s insertion_ordered_map =
  let canonical_map =
    insertion_ordered_map
    |> Map.to_alist
    |> List.map ~f:(fun (index, (key, data)) -> key, (index, data))
    |> Map.of_alist_exn comparator_s
  in
  { insertion_ordered_map; canonical_map; latest_index = Map.length canonical_map }
;;

let of_alist_exn comparator_s alist =
  let insertion_ordered_map =
    alist
    |> List.mapi ~f:(fun index (key, data) ->
      (* NOTE: We start indexing [t]s at 1 because empty [t]s have a [latest_index] of 0.

         This is also why we set [latest_index] to [Map.length canonical_map], instead of
         [Map.length canonical_map - 1], in [of_insertion_ordered_map]. *)
      index + 1, (key, data))
    |> Map.of_alist_exn (module Int)
  in
  of_insertion_ordered_map comparator_s insertion_ordered_map
;;

let of_map map = of_alist_exn (Map.comparator_s map) (Map.to_alist map)

module%template.portable
  [@modality p] [@mode m = (local, global)] Make_common (Key : sig
    type t [@@deriving (compare [@mode.explicit m]), sexp_of]

    include Comparable.S_plain [@modality p] with type t := t
  end) =
struct
  module For_sexp = struct
    type 'a t = (Key.t, 'a) List.Assoc.t [@@deriving sexp_of]
  end

  type nonrec 'a t = (Key.t, 'a, Key.comparator_witness) t

  let sexp_of_t sexp_of_v t =
    For_sexp.sexp_of_t sexp_of_v (Map.data t.insertion_ordered_map)
  ;;

  module Semantic_equal = struct
    type nonrec 'a t = 'a t

    (* NOTE: [equal] is [true] iff the two [t]s are semantically equivalent.

       Semantic equivalence means the two [t]s have equal keys and values in the same
       relative order.

       The absolute insertion order (i.e. the index of each key value pair, and the value
       of [latest_index]) is not considered in semantic equivalence. *)
    let equal equal_v t t' =
      List.equal
        (Tuple2.equal ~eq1:([%compare.equal: Key.t] [@mode.explicit m]) ~eq2:equal_v)
        (Map.data t.insertion_ordered_map)
        (Map.data t'.insertion_ordered_map)
    ;;
  end

  module Semantic_compare = struct
    type nonrec 'a t = 'a t

    let compare compare_v t t' =
      List.compare
        (Tuple2.compare ~cmp1:([%compare: Key.t] [@mode.explicit m]) ~cmp2:compare_v)
        (Map.data t.insertion_ordered_map)
        (Map.data t'.insertion_ordered_map)
    ;;
  end

  let empty =
    { insertion_ordered_map = Int.Map.empty
    ; canonical_map = Key.Map.empty
    ; latest_index = 0 (* Read note in [of_alist_exn]. *)
    }
  ;;

  let singleton key data = singleton (module Key) key data
  let of_alist_exn alist = of_alist_exn (module Key) alist
end

module%template.portable
  [@modality p] Provide_of_sexp (Key : sig
    type t [@@deriving of_sexp]

    include Comparator.S [@modality p] with type t := t
  end) =
struct
  module For_sexp = struct
    type 'a t = (Key.t, 'a) List.Assoc.t [@@deriving of_sexp]
  end

  let t_of_sexp v_of_sexp sexp =
    For_sexp.t_of_sexp v_of_sexp sexp |> of_alist_exn (module Key)
  ;;
end

module%template.portable [@modality p] Provide_bin_io (Key : Stable [@modality p]) =
Bin_prot.Utils.Make_binable1_with_uuid [@modality p] (struct
    let caller_identity =
      (* Do not copy-paste this to other calls. *)
      Bin_prot.Shape.Uuid.of_string "da3844f1-0525-8a0d-8b4b-4503171cae1e"
    ;;

    module Binable = struct
      type 'a t = (Key.t, 'a) List.Assoc.t [@@deriving bin_io]
    end

    type nonrec 'a t = (Key.t, 'a, Key.comparator_witness) t

    let to_binable t = Map.data t.insertion_ordered_map
    let of_binable binable = of_alist_exn (module Key) binable
  end)

module Provide_sexp_grammar (Key : sig
    type t [@@deriving sexp_grammar]
    type comparator_witness
  end) =
struct
  let t_sexp_grammar v_grammar
    : (Key.t, 'a, Key.comparator_witness) t Sexplib.Sexp_grammar.t
    =
    Sexplib.Sexp_grammar.coerce (List.Assoc.t_sexp_grammar Key.t_sexp_grammar v_grammar)
  ;;
end

module%template.portable
  [@modality p] [@mode m = (local, global)] Make_plain (Key : sig
    type t [@@deriving (compare [@mode.explicit m]), sexp_of]

    include Comparator.S [@modality p] with type t := t
  end) =
struct
  include Make_common [@modality p] [@mode m] (struct
      include Key
      include Comparable.Make_plain_using_comparator [@mode m] [@modality p] (Key)
    end)
end

module%template.portable
  [@modality p] [@mode m = (local, global)] Make (Key : sig
    type t [@@deriving (compare [@mode.explicit m]), sexp]

    include Comparator.S [@modality p] with type t := t
  end) =
struct
  include Make_common [@modality p] [@mode m] (struct
      include Key
      include Comparable.Make_plain_using_comparator [@mode m] [@modality p] (Key)
    end)

  include Provide_of_sexp [@modality p] (Key)
end

module%template.portable
  [@modality p] [@mode m = (local, global)] Make_binable (Key : sig
    type t [@@deriving bin_io, (compare [@mode.explicit m]), sexp]

    include Comparator.S [@modality p] with type t := t
  end) =
struct
  include Make_common [@modality p] [@mode m] (struct
      include Key
      include Comparable.Make_plain_using_comparator [@mode m] [@modality p] (Key)
    end)

  include Provide_of_sexp [@modality p] (Key)
  include Provide_bin_io [@modality p] (Key)
end
