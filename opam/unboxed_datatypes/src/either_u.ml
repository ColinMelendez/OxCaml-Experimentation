open Base

(* Define all templated versions of [t] *)
[%%template
[@@@kind.default
  k
  = ( value
    , immediate
    , immediate64
    , float64
    , bits32
    , bits64
    , vec128
    , word
    , value_or_null & value_or_null
    , value_or_null & float64
    , value_or_null & bits32
    , value_or_null & bits64
    , value_or_null & vec128
    , value_or_null & word
    , value & value
    , value & float64
    , value & bits32
    , value & bits64
    , value & vec128
    , value & word
    , immediate & immediate
    , float64 & float64
    , bits32 & bits32
    , bits64 & bits64
    , vec128 & vec128
    , word & word
    , (_ : (_ : value_or_null & (value_or_null & value_or_null)))
    , (_ : (_ : (value_or_null & value_or_null) & (value_or_null & value_or_null))) )]

type ('a : k, 'b : k, 'c : k) tag =
  | First : (('a, 'b, 'a) tag[@kind k])
  | Second : (('a, 'b, 'b) tag[@kind k])

type ('a : k, 'b : k) t =
  | T : #((('a, 'b, 'c : k) tag[@kind k]) * ('c : k)) -> (('a, 'b) t[@kind k])
[@@unboxed]

(* template end *)]

(* Only defined for [value]: *)
[%%template
[@@@mode.default m = (global, local)]

let of_either : _ Either.t @ m -> _ t @ m = function
  | First a -> T #(First, a)
  | Second b -> T #(Second, b)
;;

let to_either (type a b) (T #(tag, x) : (a, b) t) : (a, b) Either.t =
  match tag with
  | First -> First x [@exclave_if_local m]
  | Second -> Second x [@exclave_if_local m]
;;
(* template end *)]

(* All functions and submodules below are templated over the kind of [t] *)
[%%template
[@@@kind.default
  k
  = ( value
    , immediate
    , immediate64
    , float64
    , bits32
    , bits64
    , vec128
    , word
    , value_or_null & value_or_null
    , value_or_null & float64
    , value_or_null & bits32
    , value_or_null & bits64
    , value_or_null & vec128
    , value_or_null & word
    , value & value
    , value & float64
    , value & bits32
    , value & bits64
    , value & vec128
    , value & word
    , immediate & immediate
    , float64 & float64
    , bits32 & bits32
    , bits64 & bits64
    , vec128 & vec128
    , word & word
    , (_ : (_ : value_or_null & (value_or_null & value_or_null)))
    , (_ : (_ : (value_or_null & value_or_null) & (value_or_null & value_or_null))) )]

let globalize
  (type (a : k) (b : k))
  (globalize_a : a @ local -> a)
  (globalize_b : b @ local -> b)
  (T #(tag, x) : ((a, b) t[@kind k]))
  : ((a, b) t[@kind k])
  =
  match tag with
  | First -> T #(tag, globalize_a x)
  | Second -> T #(tag, globalize_b x)
;;

let[@alloc a @ m = (heap @ global, stack @ local)] sexp_of_t
  (type (a : k) (b : k))
  (of_a : a @ m -> Sexp.t @ m)
  (of_b : b @ m -> Sexp.t @ m)
  (T #(tag, x) : ((a, b) t[@kind k]))
  =
  match[@exclave_if_stack a] tag with
  | First -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "First"; of_a x ]
  | Second -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Second"; of_b x ]
;;

let is_first (type (a : k) (b : k)) (T #(tag, _) : ((a, b) t[@kind k])) =
  match tag with
  | First -> true
  | Second -> false
;;

let is_second (type (a : k) (b : k)) (T #(tag, _) : ((a, b) t[@kind k])) =
  match tag with
  | First -> false
  | Second -> true
;;

let hash_fold_t
  (type (a : k) (b : k))
  (hash_a : Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state)
  (hash_b : Ppx_hash_lib.Std.Hash.state -> b -> Ppx_hash_lib.Std.Hash.state)
  (hsv : Ppx_hash_lib.Std.Hash.state)
  (T #(tag, x) : ((a, b) t[@kind k]))
  =
  match tag with
  | First ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
    let hsv = hsv in
    hash_a hsv x
  | Second ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
    let hsv = hsv in
    hash_b hsv x
;;

module First = struct
  type nonrec ('a, 'b) t = (('a, 'b) t[@kind k])

  [%%template
  [@@@mode.default m = (global, local)]

  let return x : _ t = T #(First, x)

  let[@inline] bind
    (type (a : k) (b : k) (c : k))
    (t : (a, b) t @ m)
    ~(f : a @ m -> (c, b) t @ m)
    : (c, b) t
    =
    match t with
    | T #(First, x) -> f x [@exclave_if_local m]
    | T #(Second, _) as t -> t
  ;;

  let[@inline] value (type (a : k) (b : k)) (T #(tag, x) : (a, b) t @ m) ~(default : a)
    : a
    =
    match tag with
    | First -> x
    | Second -> default
  ;;

  (* template end *)]

  let map (type (a : k) (b : k) (c : k)) (T #(tag, x) : (a, b) t) ~(f : a -> c) : (c, b) t
    =
    match tag with
    | First -> T #(First, f x)
    | Second -> T #(Second, x)
  ;;

  let%template[@mode local] map
    (type (a : k) (b : k) (c : k))
    (T #(tag, x) : (a, b) t @ local)
    ~(f : a @ local -> c @ local)
    : (c, b) t
    = exclave_
    match tag with
    | First -> T #(First, f x)
    | Second -> T #(Second, x)
  ;;

  let combine
    (type (a : k) (b : k) (c : k) (d : k))
    (T #(tag1, x1) : (a, d) t)
    (T #(tag2, x2) : (b, d) t)
    ~(f : (a -> b -> c) @ local)
    ~(other : (d -> d -> d) @ local)
    : (c, d) t
    =
    match tag1, tag2 with
    | First, First -> T #(First, f x1 x2)
    | Second, First -> T #(Second, x1)
    | First, Second -> T #(Second, x2)
    | Second, Second -> T #(Second, other x1 x2)
  ;;

  let%template[@mode local] combine
    (type (a : k) (b : k) (c : k) (d : k))
    (T #(tag1, x1) : (a, d) t)
    (T #(tag2, x2) : (b, d) t)
    ~(f : (a @ local -> b @ local -> c @ local) @ local)
    ~(other : (d @ local -> d @ local -> d @ local) @ local)
    : (c, d) t
    = exclave_
    match tag1, tag2 with
    | First, First -> T #(First, f x1 x2)
    | Second, First -> T #(Second, x1)
    | First, Second -> T #(Second, x2)
    | Second, Second -> T #(Second, other x1 x2)
  ;;

  module Let_syntax = struct
    let return = [%eta1 return] [@@zero_alloc]
    let[@inline] ( >>| ) a f = map a ~f
    let[@inline] ( >>= ) a f = bind a ~f

    module Let_syntax = struct
      let return = [%eta1 return] [@@zero_alloc]
      let bind = bind
      let map = map

      module Open_on_rhs = struct end
    end
  end

  module Local = struct
    module Let_syntax = struct
      let[@inline] return x = exclave_ (return [@mode local]) x
      let[@inline] ( >>= ) a f = exclave_ (bind [@mode local]) a ~f
      let[@inline] ( >>| ) a f = exclave_ (map [@mode local]) a ~f

      module Let_syntax = struct
        let[@inline] return x = exclave_ (return [@mode local]) x
        let bind = (bind [@mode local])
        let map = (map [@mode local])

        module Open_on_rhs = struct end
      end
    end
  end
end

module Second = struct
  type nonrec ('a, 'b) t = (('b, 'a) t[@kind k])

  [%%template
  [@@@mode.default m = (global, local)]

  let return x : _ t = T #(Second, x)

  let[@inline] bind
    (type (a : k) (b : k) (c : k))
    (t : (a, b) t @ m)
    ~(f : a @ m -> (c, b) t @ m)
    : (c, b) t
    =
    match t with
    | T #(First, _) as t -> t
    | T #(Second, x) -> f x [@exclave_if_local m]
  ;;

  let[@inline] value (type (a : k) (b : k)) (T #(tag, x) : (a, b) t @ m) ~(default : a)
    : a
    =
    match tag with
    | First -> default
    | Second -> x
  ;;

  (* template end *)]

  let map (type (a : k) (b : k) (c : k)) (T #(tag, x) : (a, b) t) ~(f : a -> c) : (c, b) t
    =
    match tag with
    | First -> T #(First, x)
    | Second -> T #(Second, f x)
  ;;

  let%template[@mode local] map
    (type (a : k) (b : k) (c : k))
    (T #(tag, x) : (a, b) t @ local)
    ~(f : a @ local -> c @ local)
    : (c, b) t
    = exclave_
    match tag with
    | First -> T #(First, x)
    | Second -> T #(Second, f x)
  ;;

  let combine
    (type (a : k) (b : k) (c : k) (d : k))
    (T #(tag1, x1) : (a, d) t)
    (T #(tag2, x2) : (b, d) t)
    ~(f : (a -> b -> c) @ local)
    ~(other : (d -> d -> d) @ local)
    : (c, d) t
    =
    match tag1, tag2 with
    | First, First -> T #(First, other x1 x2)
    | Second, First -> T #(First, x2)
    | First, Second -> T #(First, x1)
    | Second, Second -> T #(Second, f x1 x2)
  ;;

  let%template[@mode local] combine
    (type (a : k) (b : k) (c : k) (d : k))
    (T #(tag1, x1) : (a, d) t)
    (T #(tag2, x2) : (b, d) t)
    ~(f : (a @ local -> b @ local -> c @ local) @ local)
    ~(other : (d @ local -> d @ local -> d @ local) @ local)
    : (c, d) t
    = exclave_
    match tag1, tag2 with
    | First, First -> T #(First, other x1 x2)
    | Second, First -> T #(First, x2)
    | First, Second -> T #(First, x1)
    | Second, Second -> T #(Second, f x1 x2)
  ;;

  module Let_syntax = struct
    let return = [%eta1 return] [@@zero_alloc]
    let[@inline] ( >>| ) a f = map a ~f
    let[@inline] ( >>= ) a f = bind a ~f

    module Let_syntax = struct
      let return = [%eta1 return] [@@zero_alloc]
      let bind = bind
      let map = map

      module Open_on_rhs = struct end
    end
  end

  module Local = struct
    module Let_syntax = struct
      let[@inline] return x = exclave_ (return [@mode local]) x
      let[@inline] ( >>= ) a f = exclave_ (bind [@mode local]) a ~f
      let[@inline] ( >>| ) a f = exclave_ (map [@mode local]) a ~f

      module Let_syntax = struct
        let[@inline] return x = exclave_ (return [@mode local]) x
        let bind = (bind [@mode local])
        let map = (map [@mode local])

        module Open_on_rhs = struct end
      end
    end
  end
end

(* Functions templated over locality in addition to [t]'s kind *)
[%%template
[@@@mode.default m = (global, local)]
[@@@kind.default k]

let[@inline] compare
  (type (a : k) (b : k))
  (cmp_a : (a @ m -> a @ m -> int) @ local)
  (cmp_b : (b @ m -> b @ m -> int) @ local)
  (t1 : ((a, b) t[@kind k]))
  (t2 : ((a, b) t[@kind k]))
  =
  match #(t1, t2) with
  | #(T #(First, a1), T #(First, a2)) -> cmp_a a1 a2
  | #(T #(First, _), T #(Second, _)) -> -1
  | #(T #(Second, _), T #(First, _)) -> 1
  | #(T #(Second, b1), T #(Second, b2)) -> cmp_b b1 b2
;;

let[@inline] equal
  (type (a : k) (b : k))
  (equal_a : (a @ m -> a @ m -> bool) @ local)
  (equal_b : (b @ m -> b @ m -> bool) @ local)
  (t1 : ((a, b) t[@kind k]))
  (t2 : ((a, b) t[@kind k]))
  =
  match #(t1, t2) with
  | #(T #(First, a1), T #(First, a2)) -> equal_a a1 a2
  | #(T #(First, _), T #(Second, _)) -> false
  | #(T #(Second, _), T #(First, _)) -> false
  | #(T #(Second, b1), T #(Second, b2)) -> equal_b b1 b2
;;

let swap (type (a : k) (b : k)) (T #(tag, x) : ((a, b) t[@kind k])) : ((b, a) t[@kind k]) =
  match tag with
  | First -> T #(Second, x)
  | Second -> T #(First, x)
;;

let value (type a : k) (T #(tag, x) : ((a, a) t[@kind k])) : a =
  match tag with
  | First -> x
  | Second -> x
;;

let iter
  (type (a : k) (b : k))
  (T #(tag, x) : ((a, b) t[@kind k]))
  ~(first : a @ m -> unit)
  ~(second : b @ m -> unit)
  =
  match tag with
  | First -> first x
  | Second -> second x
;;

let first x : (_ t[@kind k]) = T #(First, x)
let second x : (_ t[@kind k]) = T #(Second, x)

(* template end *)]

(* Functions templated over the return kind in addition to [t]'s kind *)
[%%template
[@@@kind.default k]

(* Instead of adding a new kind here, consider matching on the representation, as a new
   kind will create a new version of this function for every kind that [t] is templated
   over. *)
[@@@kind.default
  k' = (value, immediate, immediate64, float64, bits32, bits64, vec128, word, k)]

let[@inline] map
  (type (a : k) (b : k) (c : k') (d : k'))
  (T #(tag, x) : ((a, b) t[@kind k]))
  ~(first : (a -> c) @ local)
  ~(second : (b -> d) @ local)
  : ((c, d) t[@kind k'])
  =
  match tag with
  | First -> T #(First, first x)
  | Second -> T #(Second, second x)
;;

let%template[@mode local] [@kind k k'] [@inline] map
  (type (a : k) (b : k) (c : k') (d : k'))
  (T #(tag, x) : ((a, b) t[@kind k]))
  ~(first : (a @ local -> c @ local) @ local)
  ~(second : (b @ local -> d @ local) @ local)
  : ((c, d) t[@kind k'])
  = exclave_
  match tag with
  | First -> T #(First, first x)
  | Second -> T #(Second, second x)
;;

let%template[@kind k k'] [@mode m = (global, local)] [@inline] value_map
  (type (a : k) (b : k) (c : k'))
  (T #(tag, x) : ((a, b) t[@kind k]))
  ~(first : (a @ m -> c @ m) @ local)
  ~(second : (b @ m -> c @ m) @ local)
  =
  match tag with
  | First -> first x [@exclave_if_local m]
  | Second -> second x [@exclave_if_local m]
;;

(* template end *)]

(* template end *)]
