open Base
open Ppxlib

type t = label_declaration

let name _ field = field.pld_name.txt
let to_type field = field.pld_type

let get_rhs_expression ~loc ~index:_ ~element:{ pld_name; _ } ~number_of_elements:_ =
  let open (val Ast_builder.make loc) in
  pexp_field [%expr record] (Located.mk (Lident pld_name.txt))
;;

let disable_warning_23 ~loc =
  let open (val Ast_builder.make loc) in
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
  let open (val Ast_builder.make loc) in
  let rhs =
    pexp_record
      [ Located.mk (Lident pld_name.txt), expression_to_set ]
      (Some [%expr record])
  in
  { rhs with pexp_attributes = [ disable_warning_23 ~loc ] }
;;

let create_expression ~loc ~constructor_declarations =
  let open (val Ast_builder.make loc) in
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
        | Type_kind_intf.Shallow -> [%expr f [%e econstruct constructor None]]
        | Type_kind_intf.Deep _ ->
          let constructor_expression =
            pexp_construct
              (Lident (pld_name.txt |> String.capitalize) |> Located.mk)
              (Some (pexp_ident (Lident "x" |> Located.mk)))
          in
          let subproduct_function =
            let subproduct_module_name =
              pld_name.txt |> String.capitalize |> Type_kind_intf.append_functor_parameter
            in
            pexp_ident (Ldot (Lident subproduct_module_name, "create") |> Located.mk)
          in
          [%expr [%e subproduct_function] { f = (fun x -> f [%e constructor_expression]) }]
      in
      pexp_let Nonrecursive [ value_binding ~pat:(pvar pld_name.txt) ~expr ] acc)
;;
