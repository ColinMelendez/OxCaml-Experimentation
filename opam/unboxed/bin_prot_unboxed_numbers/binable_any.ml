open! Base
include Binable_any_intf

module%template
  [@kind k = (float32, float64, bits32, bits64, word)] Of_binable
    (Boxed : sig
     @@ portable
       type t

       include Bin_prot.Binable.S_only_functions [@mode local] with type t := t
     end)
    (T : sig
     @@ portable
       type t : k

       include S_any with type t := t and type boxed := Boxed.t
     end) =
struct
  open T

  let bin_shape_t = Bin_prot.Shape.basetype (Bin_prot.Shape.Uuid.of_string name) []

  include struct
    [@@@mode.default m = (global, local)]

    let[@inline] bin_size_t t =
      (Boxed.bin_size_t [@mode m] [@inlined hint]) ((box [@mode m]) t) [@nontail]
    ;;

    let[@inline] bin_write_t buf ~pos t =
      (Boxed.bin_write_t [@mode m] [@inlined hint])
        buf
        ~pos
        ((box [@mode m]) t) [@nontail]
    ;;
  end

  let[@inline] bin_read_t buf ~pos_ref =
    unbox (([%bin_read: Boxed.t] [@inlined hint]) buf ~pos_ref)
  ;;

  let[@inline] __bin_read_t__ _buf ~pos_ref _vint : t =
    (* Based on similar operations in [Bin_prot.Std] *)
    match Bin_prot.Common.raise_variant_wrong_type name !pos_ref with
    | (_ : Base.Nothing.t) -> .
  ;;

  let bin_writer_t = [%bin_writer: t]
  let bin_reader_t = [%bin_reader: t]
  let bin_t = [%bin_type_class: t]
end
