open Core

(* See mli for documentation. *)

module Stable = struct
  open Core.Core_stable

  module Tag = struct
    module V1 = struct
      type t =
        { ns : string [@default ""] [@sexp_drop_default.equal.local]
        ; tag : string
        }
      [@@deriving sexp, bin_io, hash, compare ~localize, stable_witness]

      let%expect_test "stability" =
        print_endline [%bin_digest: t];
        [%expect {| 055b751668ab2aa71c9e2efefa2f7c48 |}]
      ;;
    end
  end

  module Attribute = struct
    module V1 = struct
      type t =
        { ns : string [@default ""] [@sexp_drop_default.equal.local]
        ; key : string
        ; value : string
        }
      [@@deriving sexp, bin_io, hash, compare ~localize, stable_witness]

      let%expect_test "stability" =
        print_endline [%bin_digest: t];
        [%expect {| 3a10c8ec468f8aa141e107ba85ac9c0c |}]
      ;;
    end
  end

  module V2 = struct
    type element =
      { tag : Tag.V1.t
      ; attributes : Attribute.V1.t list [@sexp.list]
      ; children : t list [@sexp.list]
      }

    and t =
      | Element of element
      | Text of string
    [@@deriving sexp, bin_io, hash, compare ~localize, stable_witness]

    let%expect_test "stability" =
      print_endline [%bin_digest: element];
      print_endline [%bin_digest: t];
      [%expect
        {|
        127d900df64763c0511cbbe641643caa
        42a2032accfa5cd90e07e506efcbf6a7
        |}]
    ;;
  end

  module V1 = struct
    module Serde = struct
      type element =
        { tag : Tag.V1.t
        ; attributes : Attribute.V1.t list [@sexp.list]
        ; children : t list [@sexp.list]
        }

      and t =
        | Node of element
        | Text of string
      [@@deriving sexp, bin_io, stable_witness]

      let rec to_v2 : t -> V2.t = function
        | Text text -> Text text
        | Node element -> Element (element_to_v2 element)

      and element_to_v2 { tag; attributes; children } =
        { V2.tag; attributes; children = Core.List.map children ~f:to_v2 }
      ;;

      let rec of_v2 : V2.t -> t = function
        | Text text -> Text text
        | Element element -> Node (element_of_v2 element)

      and element_of_v2 { V2.tag; attributes; children } =
        { tag; attributes; children = Core.List.map children ~f:of_v2 }
      ;;
    end

    module Element = struct
      type t = V2.element [@@deriving compare ~localize, hash]

      include
        Sexpable.Of_sexpable.V1
          (struct
            type t = Serde.element [@@deriving sexp]
          end)
          (struct
            type nonrec t = t

            let to_sexpable = Serde.element_of_v2
            let of_sexpable = Serde.element_to_v2
          end)

      include
        (Binable.Of_binable.V1
           (struct
            type t = Serde.element [@@deriving bin_io]
          end))
          (struct
            type nonrec t = t

            let to_binable = Serde.element_of_v2
            let of_binable = Serde.element_to_v2
          end)
      [@alert "-legacy"]
    end

    type t = V2.t [@@deriving compare ~localize, hash]
    type element = Element.t [@@deriving compare ~localize, hash, sexp, bin_io]

    let stable_witness =
      Stable_witness.of_serializable Serde.stable_witness Serde.to_v2 Serde.of_v2
    ;;

    let stable_witness_element =
      Stable_witness.of_serializable
        Serde.stable_witness_element
        Serde.element_to_v2
        Serde.element_of_v2
    ;;

    include
      Sexpable.Of_sexpable.V1
        (Serde)
        (struct
          type nonrec t = t

          let to_sexpable = Serde.of_v2
          let of_sexpable = Serde.to_v2
        end)

    include
      (Binable.Of_binable.V1
         (Serde))
         (struct
           type nonrec t = t

           let of_binable = Serde.to_v2
           let to_binable = Serde.of_v2
         end)
    [@alert "-legacy"]

    let%expect_test "stability" =
      print_endline [%bin_digest: element];
      print_endline [%bin_digest: t];
      [%expect
        {|
        a2b15a7479caa5240e2f2ef2b79756ba
        39ed6f44eba600e14186974e69e9d6f0
        |}]
    ;;
  end
end

(** See test/valid_characters.ml for an explanation of these restrictions. *)
module Printable_string = struct
  type t = string
  [@@deriving
    quickcheck ~shrinker ~observer, sexp_of, compare ~localize, equal ~localize, hash]

  let quickcheck_generator = String.gen' Char.gen_print

  let quickcheck_shrinker =
    Quickcheck.Shrinker.filter quickcheck_shrinker ~f:(String.for_all ~f:Char.is_print)
  ;;
end

(** See test/valid_characters.ml for an explanation of these restrictions. *)
module Name = struct
  type t = string
  [@@deriving
    quickcheck ~shrinker ~observer, sexp_of, compare ~localize, equal ~localize, hash]

  let quickcheck_generator =
    Quickcheck.Generator.filter
      (let%map_open.Quickcheck first =
         weighted_union [ 0.9, char_alpha; 0.1, singleton '_' ]
       and rest =
         String.gen'
           (weighted_union [ 0.9, char_alphanum; 0.1, of_list [ '-'; '_'; '.' ] ])
       in
       String.of_char first ^ rest)
      ~f:(Fn.non (String.Caseless.is_prefix ~prefix:"xml"))
  ;;

  let is_valid_name name =
    (not (String.is_prefix ~prefix:"xml" name))
    && String.for_alli name ~f:(fun i c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
      | '0' .. '9' | '-' | '.' -> i > 0
      | _ -> false)
  ;;

  let quickcheck_shrinker =
    Quickcheck.Shrinker.filter quickcheck_shrinker ~f:is_valid_name
  ;;
end

module Tag = struct
  type t = Stable.Tag.V1.t =
    { ns : Printable_string.t [@default ""] [@sexp_drop_default.equal.local]
    ; tag : Name.t
    }
  [@@deriving sexp_of, compare ~localize, equal ~localize, hash, quickcheck]

  let of_tuple (ns, tag) = { ns; tag }
end

module Attribute = struct
  type t = Stable.Attribute.V1.t =
    { ns : Printable_string.t [@default ""] [@sexp_drop_default.equal.local]
    ; key : Name.t
    ; value : Printable_string.t
    }
  [@@deriving sexp_of, compare ~localize, equal ~localize, hash, quickcheck]

  let of_tuple ((ns, key), value) = { ns; key; value }
end

type element = Stable.V2.element =
  { tag : Tag.t
  ; attributes : Attribute.t list [@sexp.list]
  ; children : t list [@sexp.list]
  }

and t = Stable.V2.t =
  | Element of element
  | Text of Printable_string.t
[@@deriving
  sexp_of, compare ~localize, equal ~localize, hash, quickcheck ~shrinker ~observer]

(* When generating an entire XML element, it needs to be internally consistent. All
   namespaces used should be declared at some point, so we need to do some extra work to
   make sure we only reference namespaces previously defined. *)
module Fully_defined_xml_generator = struct
  open Quickcheck.Generator
  open Let_syntax

  let namespace_generator =
    filter
      Printable_string.quickcheck_generator
      ~f:(Fn.non (String.for_all ~f:Char.is_whitespace))
  ;;

  let namespace_prefix_generator = Name.quickcheck_generator

  let existing_namespace_generator ~bound_namespaces ~default_namespace =
    let options =
      List.concat
        [ [ "" ] (* No namespace *)
        ; Map.data bound_namespaces
        ; Option.to_list default_namespace
        ]
    in
    of_list options
  ;;

  let generic_attribute_generator ~bound_namespaces =
    let%map ns =
      existing_namespace_generator
        ~bound_namespaces
        ~default_namespace:None (* The default namespace does not affect attributes *)
    and key = Name.quickcheck_generator
    and value = Printable_string.quickcheck_generator in
    { Attribute.key; ns; value }
  ;;

  (* Can't generate the same namespace twice, or the same prefix twice. These are xml
     errors.
  *)
  let prefix_namespaces_generator =
    let%bind prefixes = list namespace_prefix_generator in
    let prefixes = List.dedup_and_sort prefixes ~compare:String.compare in
    let%bind namespaces = list_with_length (List.length prefixes) namespace_generator in
    let namespaces = List.dedup_and_sort namespaces ~compare:String.compare in
    let pairs, _remainder = List.zip_with_remainder prefixes namespaces in
    Base_quickcheck.Generator.list_permutations pairs
  ;;

  let rec element_generator namespaces =
    let%bind default_namespace = Option.quickcheck_generator namespace_generator
    and new_namespaces = prefix_namespaces_generator in
    let bound_namespaces =
      List.fold new_namespaces ~init:namespaces ~f:(fun namespaces (prefix, namespace) ->
        (* It's a map so we avoid referencing a namespace that has been shadowed. *)
        Map.set namespaces ~key:prefix ~data:namespace)
    in
    let namespace_prefix_attributes =
      List.map new_namespaces ~f:(fun (key, value) ->
        { Attribute.ns = Xmlm.ns_xmlns; key; value })
    in
    let default_namespace_attribute =
      match default_namespace with
      | None -> []
      | Some namespace ->
        [ { Attribute.ns = Xmlm.ns_xmlns; key = "xmlns"; value = namespace } ]
    in
    let%map ns = existing_namespace_generator ~bound_namespaces ~default_namespace
    and children = list (xml_generator namespaces)
    and attributes = list (generic_attribute_generator ~bound_namespaces)
    and tag = Name.quickcheck_generator in
    let attributes =
      default_namespace_attribute @ namespace_prefix_attributes @ attributes
    in
    { tag = { ns; tag }; attributes; children }

  and xml_generator existing_namespaces =
    union
      [ map Printable_string.quickcheck_generator ~f:(fun text -> Text text)
      ; map (element_generator existing_namespaces) ~f:(fun element -> Element element)
      ]
  ;;
end

let rec all_namespaces_representable
  ?(default_namespace = "")
  ?(bound_namespaces = String.Map.empty)
  = function
  | Element element ->
    let default_namespace, bound_namespaces =
      List.fold
        element.attributes
        ~init:(default_namespace, bound_namespaces)
        ~f:(fun (default_namespace, bound_namespaces) attribute ->
          if String.equal attribute.ns Xmlm.ns_xml
          then (
            match attribute with
            | { ns = _; key = "xmlns"; value } -> value, bound_namespaces
            | { ns = _; key; value = "" } ->
              default_namespace, Map.remove bound_namespaces key
            | { ns = _; key; value } ->
              default_namespace, Map.set bound_namespaces ~key ~data:value)
          else default_namespace, bound_namespaces)
    in
    let bound_to_a_prefix namespace =
      Map.exists bound_namespaces ~f:(fun value -> String.equal value namespace)
    in
    (String.equal element.tag.ns default_namespace || bound_to_a_prefix element.tag.ns)
    && List.for_all element.attributes ~f:(fun attribute ->
      bound_to_a_prefix attribute.ns)
    && List.for_all
         element.children
         ~f:(all_namespaces_representable ~default_namespace ~bound_namespaces)
  | Text _ -> true
;;

let quickcheck_generator_element =
  Fully_defined_xml_generator.element_generator String.Map.empty
;;

let quickcheck_generator = Fully_defined_xml_generator.xml_generator String.Map.empty

let quickcheck_shrinker =
  Quickcheck.Shrinker.filter quickcheck_shrinker ~f:all_namespaces_representable
;;

let quickcheck_shrinker_element =
  Quickcheck.Shrinker.filter quickcheck_shrinker_element ~f:(fun element ->
    all_namespaces_representable (Element element))
;;

let () =
  Sexplib.Conv.Exn_converter.add
    [%extension_constructor Xmlm.Error]
    (Obj.magic_portable (function
      | Xmlm.Error ((line, col), error) ->
        let error = Xmlm.error_message error in
        [%message "Error parsing xml" (line : int) (col : int) (error : string)]
      | _ -> assert false))
;;

let parse_input input =
  Xmlm.input_doc_tree
    input
    ~data:(fun s -> Text s)
    ~el:(fun (tag, attributes) children ->
      let tag = Tag.of_tuple tag in
      let attributes = List.map ~f:Attribute.of_tuple attributes in
      Element { tag; attributes; children })
  |> function
  | _dtd, Text _ -> failwith "malformed xml"
  | _dtd, Element n -> n
;;

type inp =
  [ `Channel of In_channel.t
  | `File of string
  | `String of string
  ]

let rec parse ?(strip = false) = function
  | `String s -> parse_input (Xmlm.make_input ~strip (`String (0, s)))
  | `Channel ic -> parse_input (Xmlm.make_input ~strip (`Channel ic))
  | `File fn -> In_channel.with_file fn ~f:(fun ic -> parse ~strip (`Channel ic :> inp))
;;

let to_string ?decl ?ns_prefix ?buf ?fmt xml =
  let buf =
    match buf with
    | None -> Buffer.create 16
    | Some buf -> buf
  in
  let indent =
    Option.bind fmt ~f:(function
      | `Minified -> None
      | `Newlines_only -> Some 0
      | `Indent n -> Some n)
  in
  Xmlm.output_doc_tree
    (function
      | Text text -> `Data text
      | Element { tag = { tag; ns }; attributes; children } ->
        `El
          ( ( (ns, tag)
            , List.map attributes ~f:(fun { ns; key; value } -> (ns, key), value) )
          , children ))
    (Xmlm.make_output ?decl ?ns_prefix ~indent (`Buffer buf))
    (None, xml);
  Buffer.contents buf
;;

let element_of_string str = parse (`String str)

let map_element t ~f =
  match t with
  | Element element -> Element (f element)
  | Text _ -> t
;;

module Creators = struct
  let element' ?(ns = "") ?(attributes = []) tag children =
    { tag = { tag; ns }; attributes; children }
  ;;

  let element ?ns ?attributes tag children =
    Element (element' ?ns ?attributes tag children)
  ;;

  let text text = Text text

  let leaf_element ?ns ?attributes tag leaf_text =
    element ?ns ?attributes tag [ text leaf_text ]
  ;;

  let attr ?(ns = "") key value = Attribute.{ ns; key; value }
  let default_namespace namespace = attr ~ns:Xmlm.ns_xmlns "xmlns" namespace
  let namespace_prefix ~prefix ~ns = attr ~ns:Xmlm.ns_xmlns prefix ns
end

module Raise_on_namespace = Raise_on_namespace
