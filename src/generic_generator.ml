open! Base
open Ppxlib

(* Generates `type _ t = A : a |  B : b ...` type *)
let gen_t
  (type a)
  ~loc
  ~original_type
  ~original_kind
  ~(elements_to_convert : (a * Type_kind.granularity) list)
  ~generate_constructors
  ~params
  ~upper_name
  =
  let open (val Syntax.builder loc) in
  let core_type_params = List.map params ~f:(fun (core_type_, _) -> core_type_) in
  let upper =
    Type_kind.upper
      ~loc
      ~manifest_type:original_type
      ~original_kind
      ~params
      ~name:upper_name
  in
  let constructor_declarations =
    generate_constructors ~loc ~elements_to_convert ~core_type_params
  in
  let t =
    type_declaration
      ~private_:Public
      ~manifest:None
      ~name:(Located.mk Type_kind.internal_gadt_name)
      ~params:(params @ [ ptyp_any, (NoVariance, Injective) ])
      ~cstrs:[]
      ~kind:(Ptype_variant (List.map constructor_declarations ~f:snd))
  in
  let internal_gadt_rename =
    let unique_id =
      Type_kind.generate_unique_id (Type_kind.generate_core_type_params params)
    in
    let t_params = params @ [ ptyp_var unique_id, (NoVariance, NoInjectivity) ] in
    let core_type_params = List.map t_params ~f:(fun (x, _) -> x) in
    type_declaration
      ~name:(Located.mk "t")
      ~params:t_params
      ~cstrs:[]
      ~private_:Public
      ~kind:Ptype_abstract
      ~manifest:
        (Some
           (ptyp_constr
              (Lident Type_kind.internal_gadt_name |> Located.mk)
              core_type_params))
  in
  let internal_gadt_rename =
    { internal_gadt_rename with
      ptype_attributes =
        [ attribute
            ~name:(Located.mk "ocaml.warning")
            ~payload:(PStr [ pstr_eval (estring "-34") [] ])
        ]
    }
  in
  let result : 'a Type_kind.gen_t_result =
    { gadt_t = t; upper; constructor_declarations; internal_gadt_rename }
  in
  result
;;

let opaque_signature
  (module Specific_deriver : Typed_deriver.S)
  ~loc
  ~manifest_type
  ~original_kind
  ~params
  =
  let open (val Syntax.builder loc) in
  let upper =
    Type_kind.upper ~loc ~manifest_type ~original_kind ~params ~name:Names.derived_on_name
  in
  pmty_signature
    (signature
       ([ psig_type Nonrecursive [ upper ] ]
        @ Specific_deriver.generate_include_signature_for_opaque ~loc ~params))
;;
