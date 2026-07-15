(* Test generated code *)

[@@@disable_unused_warnings]

type f = unit -> unit

(* ppx_fuelproof works by annotating each contained field as crossing the desired mode. If
   this succeeds, then the overall type is allowed to cross.
*)

module%test Portable = struct
  [@@@expand_inline
    type%fuelproof _ t : value mod portable =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { mutable y : int
          ; z : int
          }
          -> unit t]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__001_ = struct
        type _ t : value mod portable =
          | A : int t
          | B : (int as (_ : any mod portable)) -> bool t
          | C : { x : int as (_ : any mod portable) } -> string t
          | D :
              { mutable y : int as (_ : any mod portable)
              ; z : int as (_ : any mod portable)
              }
              -> unit t
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__002_ t : value mod portable = 'a__002_ Check__001_.t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { mutable y : int
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t -> _ t @ portable = fun x -> x
end

module%test Contended = struct
  [@@@expand_inline
    type%fuelproof _ t : value mod contended =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : int
          ; z : int
          }
          -> unit t]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__005_ = struct
        type _ t : value mod contended =
          | A : int t
          | B : (int as (_ : any mod contended)) -> bool t
          | C : { x : int as (_ : any mod contended) } -> string t
          | D :
              { y : int as (_ : any mod contended)
              ; z : int as (_ : any mod contended)
              }
              -> unit t
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__006_ t : value mod contended = 'a__006_ Check__005_.t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : int
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t @ contended -> _ t = fun x -> x
end

module%test Unyielding = struct
  [@@@expand_inline
    type%fuelproof _ t : value mod unyielding =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : f @@ unyielding
          ; z : int
          }
          -> unit t]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__009_ = struct
        type _ t : value mod unyielding =
          | A : int t
          | B : (int as (_ : any mod unyielding)) -> bool t
          | C : { x : int as (_ : any mod unyielding) } -> string t
          | D :
              { y : f @@ unyielding
              ; z : int as (_ : any mod unyielding)
              }
              -> unit t
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__010_ t : value mod unyielding = 'a__010_ Check__009_.t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : f @@ unyielding
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t @ unyielding -> _ t = fun x -> x
end

module%test Many = struct
  [@@@expand_inline
    type%fuelproof _ t : value mod many =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : f @@ many
          ; z : int
          }
          -> unit t]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__013_ = struct
        type _ t : value mod many =
          | A : int t
          | B : (int as (_ : any mod many)) -> bool t
          | C : { x : int as (_ : any mod many) } -> string t
          | D :
              { y : f @@ many
              ; z : int as (_ : any mod many)
              }
              -> unit t
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__014_ t : value mod many = 'a__014_ Check__013_.t =
      | A : int t
      | B : int -> bool t
      | C : { x : int } -> string t
      | D :
          { y : f @@ many
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t @ many -> _ t = fun x -> x
end

(* Modalities discharge the requirement that the field mode-cross. *)

module%test Modalities = struct
  [@@@expand_inline
    type%fuelproof _ t : value mod portable =
      | A : int t
      | B : f @@ portable * int -> bool t
      | C :
          { x : f @@ portable
          ; y : int
          }
          -> string t
      | D :
          { mutable y : int
          ; mutable y' : f @@ portable
          ; z : int
          }
          -> unit t]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__017_ = struct
        type _ t : value mod portable =
          | A : int t
          | B : f @@ portable * (int as (_ : any mod portable)) -> bool t
          | C :
              { x : f @@ portable
              ; y : int as (_ : any mod portable)
              }
              -> string t
          | D :
              { mutable y : int as (_ : any mod portable)
              ; mutable y' : f @@ portable
              ; z : int as (_ : any mod portable)
              }
              -> unit t
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__018_ t : value mod portable = 'a__018_ Check__017_.t =
      | A : int t
      | B : f @@ portable * int -> bool t
      | C :
          { x : f @@ portable
          ; y : int
          }
          -> string t
      | D :
          { mutable y : int
          ; mutable y' : f @@ portable
          ; z : int
          }
          -> unit t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t -> _ t @ portable = fun x -> x
end

module%test Modalities2 = struct
  type mut = { mutable field : int }
  type mut_f = { mutable f_field : unit -> unit }

  [@@@expand_inline
    type%fuelproof _ t : value mod contended portable =
      | A : int t
      | B : f @@ portable * int -> bool t
      | C :
          { x : f @@ portable
          ; y : mut @@ contended
          ; z : mut_f @@ contended portable
          }
          -> string t]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__021_ = struct
        type _ t : value mod contended portable =
          | A : int t
          | B :
              (f as (_ : any mod contended)) @@ portable
              * (int as (_ : any mod contended portable))
              -> bool t
          | C :
              { x : f as (_ : any mod contended) @@ portable
              ; y : mut as (_ : any mod portable) @@ contended
              ; z : mut_f @@ contended portable
              }
              -> string t
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__022_ t : value mod contended portable = 'a__022_ Check__021_.t =
      | A : int t
      | B : f @@ portable * int -> bool t
      | C :
          { x : f @@ portable
          ; y : mut @@ contended
          ; z : mut_f @@ contended portable
          }
          -> string t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t @ contended -> _ t @ portable = fun x -> x
end

(* See also [test_errors.mdx]. *)
module%test Existentials = struct
  [@@@expand_inline
    type%fuelproof t : value mod portable =
      | A : ('a : value mod portable). 'a -> t
      | B : ('a : value mod portable). 'a list -> t
      | C : ('a : value mod portable). 'a iarray -> t]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__025_ = struct
        type t : value mod portable =
          | A : ('a : value mod portable). ('a as (_ : any mod portable)) -> t
          | B :
              ('a : value mod portable).
              ('a as (_ : any mod portable)) Ppx_fuelproof_runtime.list
              -> t
          | C :
              ('a : value mod portable).
              ('a as (_ : any mod portable)) Ppx_fuelproof_runtime.Iarray.t
              -> t
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod portable = Check__025_.t =
      | A : ('a : value mod portable). 'a -> t
      | B : ('a : value mod portable). 'a list -> t
      | C : ('a : value mod portable). 'a iarray -> t
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

module%test Kind_abbreviations = struct
  [@@@expand_inline
    type%fuelproof _ t : immutable_data =
      { x : int
      ; y : int t
      }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__027_ = struct
        type _ t : immutable_data =
          { x :
              int
              as
              (_ :
              any mod contended forkable immutable many portable stateless unyielding)
          ; y :
              int t
              as
              (_ :
              any mod contended forkable immutable many portable stateless unyielding)
          }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__028_ t : immutable_data = 'a__028_ Check__027_.t =
      { x : int
      ; y : int t
      }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t @ contended -> _ t @ portable = fun x -> x
end

module%test Kind_abbreviations2 = struct
  [@@@expand_inline
    type%fuelproof _ t : mutable_data =
      { mutable x : int
      ; mutable y : int t
      }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__031_ = struct
        type _ t : mutable_data =
          { mutable x : int as (_ : any mod forkable many portable stateless unyielding)
          ; mutable y : int t as (_ : any mod forkable many portable stateless unyielding)
          }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type 'a__032_ t : mutable_data = 'a__032_ Check__031_.t =
      { mutable x : int
      ; mutable y : int t
      }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross : _ t -> _ t @ portable = fun x -> x
end

module%test Recursive_unboxed = struct
  [@@@expand_inline
    type%fuelproof t : immutable_data =
      | Nil of int
      | Rec of u

    and u : immutable_data = { more : v } [@@unboxed]
    and v : immutable_data = { x : t }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__035_ = struct
        type t : immutable_data =
          | Nil of
              (int
               as
               (_ :
               any mod contended forkable immutable many portable stateless unyielding))
          | Rec of
              (u
               as
               (_ :
               any mod contended forkable immutable many portable stateless unyielding))
        [@@unsafe_allow_any_mode_crossing]

        and u : immutable_data =
          { more :
              v
              as
              (_ :
              any
              mod
                 contended forkable immutable many non_float portable stateless unyielding)
          }
        [@@unboxed] [@@unsafe_allow_any_mode_crossing]

        and v : immutable_data =
          { x :
              t
              as
              (_ :
              any mod contended forkable immutable many portable stateless unyielding)
          }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : immutable_data = Check__035_.t =
      | Nil of int
      | Rec of u
    [@@unsafe_allow_any_mode_crossing]

    and u : immutable_data = Check__035_.u = { more : v }
    [@@unboxed] [@@unsafe_allow_any_mode_crossing]

    and v : immutable_data = Check__035_.v = { x : t } [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross_portability : t -> t @ portable = fun x -> x
  let cross_contention : t @ contended -> t = fun x -> x
end

(* When recursive uses of a type under definition appear under immutable_data type
   constructors, the constraint is pushed down to the recursive use. This appears to help
   the type-checker infer useful kinds in more places.
*)
module%test Recursive_with_immutable_data = struct
  module Iarray = struct
    type 'a t = 'a iarray
  end

  [@@@expand_inline
    type%fuelproof t : value mod contended portable =
      | Both of (t * t) list Iarray.t
      | Other of (int * t) iarray option]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__037_ = struct
        type t : value mod contended portable =
          | Both of
              ((t as (_ : any mod contended portable))
              * (t as (_ : any mod contended portable)))
                Ppx_fuelproof_runtime.list
                Ppx_fuelproof_runtime.Iarray.t
          | Other of
              ((int as (_ : any mod contended portable))
              * (t as (_ : any mod contended portable)))
                Ppx_fuelproof_runtime.Iarray.t
                Ppx_fuelproof_runtime.option
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod contended portable = Check__037_.t =
      | Both of (t * t) list Iarray.t
      | Other of (int * t) iarray option
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]

  let cross_portability : t -> t @ portable = fun x -> x
  let cross_contention : t @ contended -> t = fun x -> x
end

(* [%fuelproof] doesn't error as long as at least one type in the knot has an annotation.
*)
module%test Recursive_knot = struct
  [@@@expand_inline
    type%fuelproof t = { u : u }
    and u : value mod portable = U : int -> u
    and v
    and w : value mod contended = int]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__039_ = struct
        type t = { u : u }

        and u : value mod portable = U : (int as (_ : any mod portable)) -> u
        [@@unsafe_allow_any_mode_crossing]

        and v
        and w : value mod contended = int
      end
    end

    type t = Check__039_.t = { u : u }

    and u : value mod portable = Check__039_.u = U : int -> u
    [@@unsafe_allow_any_mode_crossing]

    and v = Check__039_.v
    and w : value mod contended = int
  end

  [@@@end]
end

module Deriving_ppxes : sig
  [@@@expand_inline:
    type%fuelproof t : value mod portable =
      | T :
          { x : int u
          ; y : int option
          }
          -> t

    and 'a u : value mod portable = U : ('a : value mod portable). 'a -> 'a u
    [@@deriving sexp_of]]

  type t : value mod portable =
    | T :
        { x : int u as (_ : any mod portable)
        ; y : (int as (_ : any mod portable)) Ppx_fuelproof_runtime.option
        }
        -> t
  [@@unsafe_allow_any_mode_crossing]

  and 'a u : value mod portable =
    | U : ('a : value mod portable). ('a as (_ : any mod portable)) -> 'a u
  [@@deriving sexp_of] [@@unsafe_allow_any_mode_crossing]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val sexp_of_u : ('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  [@@@end]
end = struct
  let sexp_of_int = Base.Int.sexp_of_t

  [@@@expand_inline
    type%fuelproof t : value mod portable =
      | T :
          { x : int u
          ; y : int option [@option]
          }
          -> t

    and 'a u : value mod portable = U : ('a : value mod portable). 'a -> 'a u
    [@@deriving sexp_of]]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__041_ = struct
        type t : value mod portable =
          | T :
              { x : int u as (_ : any mod portable)
              ; y : (int as (_ : any mod portable)) Ppx_fuelproof_runtime.option
              }
              -> t
        [@@unsafe_allow_any_mode_crossing]

        and 'a u : value mod portable =
          | U : ('a : value mod portable). ('a as (_ : any mod portable)) -> 'a u
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod portable = Check__041_.t =
      | T :
          { x : int u
          ; y : int option [@option]
          }
          -> t
    [@@unsafe_allow_any_mode_crossing]

    and 'a u : value mod portable = 'a Check__041_.u =
      | U : ('a : value mod portable). 'a -> 'a u
    [@@deriving sexp_of] [@@unsafe_allow_any_mode_crossing]

    include struct
      let _ = fun (_ : t) -> ()
      let _ = fun (_ : 'a u) -> ()

      let rec sexp_of_t =
        (fun (T { x = x__043_; y = y__045_ }) ->
           let bnds__042_ = ([] : _ Stdlib.List.t) in
           let bnds__042_ =
             match y__045_ with
             | Stdlib.Option.None -> bnds__042_
             | Stdlib.Option.Some v__046_ ->
               let arg__048_ = sexp_of_int v__046_ in
               let bnd__047_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "y"; arg__048_ ] in
               (bnd__047_ :: bnds__042_ : _ Stdlib.List.t)
           in
           let bnds__042_ =
             let arg__044_ = sexp_of_u sexp_of_int x__043_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__044_ ] :: bnds__042_
              : _ Stdlib.List.t)
           in
           Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "T" :: bnds__042_)
         : t -> Sexplib0.Sexp.t)

      and sexp_of_u : 'a. ('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t =
        fun (type a__052_)
          : ((a__052_ -> Sexplib0.Sexp.t) -> a__052_ u -> Sexplib0.Sexp.t) ->
        fun _of_a__049_ (U arg0__050_) ->
        let res0__051_ = _of_a__049_ arg0__050_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "U"; res0__051_ ]
      ;;

      let _ = sexp_of_t
      and _ = sexp_of_u
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end

  [@@@end]
end

module Attributes_preserved : sig
  [@@@expand_inline:
    type%fuelproof t : value mod portable = { mutable x : int [@atomic] }]

  type t : value mod portable = { mutable x : int as (_ : any mod portable) [@atomic] }
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]
end = struct
  [@@@expand_inline type%fuelproof t : value mod portable = { mutable x : int [@atomic] }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__076_ = struct
        type t : value mod portable =
          { mutable x : int as (_ : any mod portable) [@atomic] }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod portable = Check__076_.t = { mutable x : int [@atomic] }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

(* Fuelproof handles recursive knots even when it is not responsible for checking all
   records in the knot (e.g. because it involves [unsafe_allow_any_mode_crossing].
*)
module Manifest : sig
  [@@@expand_inline:
    type%fuelproof t : value mod portable = { f : unit -> unit }
    [@@unsafe_allow_any_mode_crossing]

    and u : value mod portable = { x : t }]

  type t : value mod portable = { f : unit -> unit } [@@unsafe_allow_any_mode_crossing]

  and u : value mod portable = { x : t as (_ : any mod portable) }
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]
end = struct
  [@@@expand_inline
    type%fuelproof t : value mod portable = { f : unit -> unit }
    [@@unsafe_allow_any_mode_crossing]

    and u : value mod portable = { x : t }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__078_ = struct
        type t : value mod portable = { f : unit -> unit }
        [@@unsafe_allow_any_mode_crossing]

        and u : value mod portable = { x : t as (_ : any mod portable) }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod portable = Check__078_.t = { f : unit -> unit }
    [@@unsafe_allow_any_mode_crossing]

    and u : value mod portable = Check__078_.u = { x : t }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

(* Mutable fields are allowed to be part of a record that crosses contention if atomic *)
module Atomic : sig
  [@@@expand_inline:
    type%fuelproof t : value mod contended = { mutable x : int [@atomic] }]

  type t : value mod contended = { mutable x : int as (_ : any mod contended) [@atomic] }
  [@@unsafe_allow_any_mode_crossing]

  [@@@end]
end = struct
  [@@@expand_inline
    type%fuelproof t : value mod contended = { mutable x : int [@atomic] }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__080_ = struct
        type t : value mod contended =
          { mutable x : int as (_ : any mod contended) [@atomic] }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod contended = Check__080_.t = { mutable x : int [@atomic] }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

(* [non_float] is a non-modal axis that only applies to [@@unboxed] types. For boxed
   types, the [non_float] axis is automatically satisfied regardless of field contents. *)
module%test Non_float_boxed_record = struct
  [@@@expand_inline type%fuelproof t : value mod non_float = { x : float }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__082_ = struct
        type t : value mod non_float = { x : float } [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod non_float = Check__082_.t = { x : float }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

module%test Non_float_portable_boxed_record = struct
  [@@@expand_inline type%fuelproof t : value mod non_float portable = { x : float }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__084_ = struct
        type t : value mod non_float portable = { x : float as (_ : any mod portable) }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod non_float portable = Check__084_.t = { x : float }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

module%test Non_float_boxed_variant = struct
  [@@@expand_inline type%fuelproof t : value mod non_float = T of float]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__086_ = struct
        type t : value mod non_float = T of float [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod non_float = Check__086_.t = T of float
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

module%test Non_float_portable_boxed_variant = struct
  [@@@expand_inline type%fuelproof t : value mod non_float portable = T of float]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__088_ = struct
        type t : value mod non_float portable = T of (float as (_ : any mod portable))
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod non_float portable = Check__088_.t = T of float
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end

module%test Non_float_portable_multi_field = struct
  [@@@expand_inline
    type%fuelproof t : value mod non_float portable =
      { x : int
      ; y : int
      }]

  include struct
    open struct
      [@@@warning "-34"]

      module Check__090_ = struct
        type t : value mod non_float portable =
          { x : int as (_ : any mod portable)
          ; y : int as (_ : any mod portable)
          }
        [@@unsafe_allow_any_mode_crossing]
      end
    end

    type t : value mod non_float portable = Check__090_.t =
      { x : int
      ; y : int
      }
    [@@unsafe_allow_any_mode_crossing]
  end

  [@@@end]
end
