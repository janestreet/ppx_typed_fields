open! Base
open Ppxlib

type t = label_declaration

let name _ field = field.pld_name.txt
let to_type field = field.pld_type

(** Attached to label declarations for fields whose type is not of layout value. Fields
    with this attribute are omitted from [get] and [set], since those functions require
    the field type to be representable. *)
let non_value_attribute =
  Attribute.declare_flag "typed_fields.non_value" Attribute.Context.label_declaration
;;

let is_non_value field = Attribute.has_flag ~mark_as_seen:true non_value_attribute field

let get_rhs_expression ~loc ~index:_ ~element:{ pld_name; _ } ~number_of_elements:_ =
  let open (val Syntax.builder loc) in
  pexp_field [%expr record] (Located.mk (Lident pld_name.txt))
;;

let disable_warning_23 ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-23") [] ])
;;

let set_rhs_expression
  ~loc
  ~index:_
  ~element:{ pld_name; _ }
  ~number_of_elements:_
  ~expression_to_set
  =
  let open (val Syntax.builder loc) in
  let rhs =
    pexp_record
      [ Located.mk (Lident pld_name.txt), expression_to_set ]
      (Some [%expr record])
  in
  { rhs with pexp_attributes = [ disable_warning_23 ~loc ] }
;;

let create_expression ~loc ~constructor_declarations ~local =
  let open (val Syntax.builder loc) in
  let create_record =
    pexp_record
      (List.map constructor_declarations ~f:(fun (({ pld_name; _ }, _), _) ->
         Located.mk (Lident pld_name.txt), [%expr [%e evar pld_name.txt]]))
      None
  in
  (* create fields and then create record *)
  List.fold
    (List.rev constructor_declarations)
    ~init:create_record
    ~f:(fun acc (({ pld_name; _ }, granularity), constructor) ->
      let expr =
        match granularity with
        | Type_kind.Shallow ->
          [%expr __ppx_typed_fields_creator_f [%e econstruct constructor None]]
        | Type_kind.Deep _ ->
          let constructor_expression =
            pexp_construct
              (Lident (pld_name.txt |> String.capitalize) |> Located.mk)
              (Some (pexp_ident (Lident "x" |> Located.mk)))
          in
          let subproduct_function =
            let subproduct_module_name =
              pld_name.txt |> String.capitalize |> Type_kind.append_functor_parameter
            in
            match local with
            | false ->
              pexp_ident (Ldot (Lident subproduct_module_name, "create") |> Located.mk)
            | true ->
              pexp_ident
                (Ldot (Lident subproduct_module_name, "create_local") |> Located.mk)
          in
          [%expr
            [%e subproduct_function]
              { f =
                  (fun (x @ local) ->
                    __ppx_typed_fields_creator_f [%e constructor_expression] [@nontail])
              }]
      in
      pexp_let
        Immutable
        Nonrecursive
        [ value_binding ~pat:(pvar pld_name.txt) ~expr ~modes:[] ]
        acc)
;;
