module F64 = struct
  include Float_u

  module Option = struct
    include Option

    let%template[@mode m = (global, local)] [@zero_alloc_if_local m] box t =
      (Packed_float_option.Unboxed.box [@mode m])
        t [@exclave_if_local m ~reasons:[ May_return_local ]]
    ;;

    let[@inline] [@zero_alloc] unbox (t @ local) = Packed_float_option.Unboxed.unbox t
  end

  let to_f32 = to_float32_u
  let of_f32 = of_float32_u
  let to_float32_u = `prefer_to_f32
  let of_float32_u = `prefer_of_f32
end

module UFO = F64.Option

module F32 = struct
  include Float32_u

  let to_f64 = to_float_u
  let of_f64 = of_float_u
  let to_float_u = `prefer_to_f64
  let of_float_u = `prefer_of_f64
  let to_float32 = `prefer_box
  let of_float32 = `prefer_unbox
end

module Pct = Percent_u

module I32 = struct
  include Int32_u

  let to_i64 = to_int64_u
  let of_i64_trunc = of_int64_u_trunc
  let of_i64_exn = of_int64_u_exn
  let to_int64_u = `prefer_to_i64
  let of_int64_u_trunc = `prefer_of_i64_trunc
  let of_int64_u_exn = `prefer_of_i64_exn
  let of_int64_exn = `prefer_of_i64_exn
  let to_int64 = `prefer_to_i64
  let to_int32 = `prefer_box
  let of_int32 = `prefer_unbox
end

module I64 = struct
  include Int64_u

  let of_i32 = I32.to_i64
  let to_i32_trunc = I32.of_i64_trunc
  let to_i32_exn = I32.of_i64_exn
  let of_int32_exn = `prefer_of_i32
  let to_int32_exn = `prefer_to_i32_exn
  let to_int64 = `prefer_box
  let of_int64_exn = `prefer_unbox
end

module Iptr = struct
  include Nativeint_u

  let[@inline] [@zero_alloc] of_i64_trunc t = I64.box t |> of_int64_trunc
  let[@inline] [@zero_alloc] to_i64 t = to_int64 t |> I64.unbox
  let[@inline] [@zero_alloc] of_i32 t = I32.box t |> of_int32
  let[@inline] [@zero_alloc] to_i32_trunc t = box t |> I32.of_nativeint_trunc
  let of_nativeint = `prefer_unbox
  let to_nativeint = `prefer_box
end

type f64 = F64.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]
type f32 = F32.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]

type i64 = I64.t
[@@deriving sexp ~stackify, compare ~localize, equal ~localize, quickcheck]

type i32 = I32.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]
type iptr = Iptr.t [@@deriving sexp, compare ~localize, equal ~localize, quickcheck]

let typerep_of_f64 = F64.typerep_of_t
let typerep_of_i64 = I64.typerep_of_t
let typerep_of_i32 = I32.typerep_of_t
let typerep_of_iptr = Iptr.typerep_of_t
