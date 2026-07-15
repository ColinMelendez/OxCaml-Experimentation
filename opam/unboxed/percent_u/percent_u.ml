open! Core
open! Import
include Float_u

module O = struct
  include O

  let[@zero_alloc] ( // ) a b = a / b
end

include O

let[@inline] box t = Float_u.to_float t |> Percent.of_mult
let[@zero_alloc] unbox (local_ f) = Percent.to_mult f |> Float_u.of_float

module Serializer_overrides = struct
  let%template[@alloc a = (heap, stack)] sexp_of_t t =
    (box t |> (Percent.sexp_of_t [@alloc a])) [@exclave_if_stack a]
  ;;

  let t_of_sexp s = Percent.t_of_sexp s |> unbox
  let to_string t = box t |> Percent.to_string
  let of_string s = Percent.of_string s |> unbox

  let bin_shape_t =
    Bin_prot.Shape.basetype
      (Bin_prot.Shape.Uuid.of_string "43b0486e-453d-11ef-b26b-c84bd689e9da")
      [ bin_shape_t ]
  ;;
end

include Serializer_overrides

let[@zero_alloc] of_mult f = f
let[@zero_alloc] to_mult t = t
let[@zero_alloc] of_percentage f = f / #100.
let[@zero_alloc] of_percentage_approx f = f * #0.01
let[@zero_alloc] to_percentage t = t * #100.
let[@zero_alloc] of_bp f = f / #10_000.
let[@zero_alloc] of_bp_approx f = f * #0.0001
let[@zero_alloc] to_bp t = t * #10_000.
let[@zero_alloc] of_bp_int i = of_bp (Float_u.of_int i)
let[@zero_alloc] of_bp_int_approx i = of_bp_approx (Float_u.of_int i)
let[@zero_alloc] to_bp_int t = Float_u.to_int (to_bp t)
let zero = #0.
let one_hundred_percent = #1.
let[@zero_alloc] apply x y = x * y
let[@zero_alloc] scale x y = x * y

module Option = struct
  let[@inline] box' t =
    match%optional_u.UFO t with
    | None -> Percent.Option.none
    | Some f -> Percent.Option.some (box f)
  ;;

  let[@zero_alloc] unbox' (local_ f) =
    Percent.Option.to_mult_with_none_as_nan f |> Float_u.unbox |> UFO.of_float_nan_as_none
  ;;

  include UFO
  module O = UFO.O
  include O

  let box = box'
  let none = none
  let[@zero_alloc] unbox (local_ t) = unbox' t
  let[@zero_alloc] of_mult f = f
  let[@zero_alloc] to_mult t = t
  let[@zero_alloc] of_percentage f = f / of_float_nan_as_none #100.
  let[@zero_alloc] of_percentage_approx t = t * of_float_nan_as_none #0.01
  let[@zero_alloc] to_percentage t = t * of_float_nan_as_none #100.
  let[@zero_alloc] of_bp f = f / of_float_nan_as_none #10_000.
  let[@zero_alloc] of_bp_approx t = t * of_float_nan_as_none #0.0001
  let[@zero_alloc] to_bp t = t * of_float_nan_as_none #10_000.
  let[@zero_alloc] apply x y = x * y
  let[@zero_alloc] scale x y = x * y
  let[@zero_alloc] unchecked_some x = of_float_nan_as_none x

  module Serializer_overrides = struct
    let%template[@alloc a = (heap, stack)] sexp_of_t t =
      (box t |> Percent.Stable.Option.V3.sexp_of_t) [@exclave_if_stack a]
    ;;

    let t_of_sexp t = Percent.Stable.Option.V3.t_of_sexp t |> unbox

    let bin_shape_t =
      Bin_prot.Shape.basetype
        (Bin_prot.Shape.Uuid.of_string "f52ce264-453d-11ef-abca-c84bd689e9da")
        [ bin_shape_t ]
    ;;
  end

  include Serializer_overrides

  let%expect_test "sexp roundtrip" =
    let roundtrip x =
      let s = sexp_of_t x in
      let x' = t_of_sexp s in
      print_s [%sexp (x : t)];
      print_s [%sexp (x' : t)];
      assert (UFO.equal x x')
    in
    roundtrip UFO.none;
    [%expect
      {|
      ()
      ()
      |}];
    roundtrip (UFO.of_float_nan_as_none #123.456);
    [%expect
      {|
      (123.456x)
      (123.456x)
      |}];
    roundtrip (UFO.of_float_nan_as_none #0.456);
    [%expect
      {|
      (45.6%)
      (45.6%)
      |}];
    roundtrip (UFO.of_float_nan_as_none #0.00456);
    [%expect
      {|
      (45.6bp)
      (45.6bp)
      |}]
  ;;

  let%expect_test "converters" =
    let module UFO = UFO in
    let converters =
      [ ~of_ufo:of_mult, ~of_ufo_approx:of_mult, ~to_ufo:to_mult, "of_mult"
      ; ( ~of_ufo:of_percentage
        , ~of_ufo_approx:of_percentage_approx
        , ~to_ufo:to_percentage
        , "of_percentage" )
      ; ~of_ufo:of_bp, ~of_ufo_approx:of_bp_approx, ~to_ufo:to_bp, "of_bp"
      ]
    in
    let run_test ufo =
      List.iter converters ~f:(fun (~of_ufo, ~of_ufo_approx, ~to_ufo, name) ->
        let t = of_ufo ufo in
        let approx = of_ufo_approx ufo in
        let s = sexp_of_t t in
        let s_approx = sexp_of_t approx in
        [%string "%{name}: %{s#Sexp}. Approx: %{s_approx#Sexp}"] |> print_endline;
        assert (equal t approx);
        assert (UFO.equal (to_ufo t) ufo);
        assert (UFO.equal (to_ufo approx) ufo))
    in
    run_test UFO.none;
    [%expect
      {|
      of_mult: (). Approx: ()
      of_percentage: (). Approx: ()
      of_bp: (). Approx: ()
      |}];
    run_test UFO.one;
    [%expect
      {|
      of_mult: (1x). Approx: (1x)
      of_percentage: (1%). Approx: (1%)
      of_bp: (1bp). Approx: (1bp)
      |}]
  ;;
end

module Stable = struct
  module V1 = struct
    include Float_u.Stable.V1
    include Serializer_overrides
  end

  module Option = struct
    module V1 = struct
      include UFO.Stable.V1
      include Option.Serializer_overrides
    end
  end
end
