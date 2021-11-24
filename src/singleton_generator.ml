open Base
open Import
open Ppxlib

type common_items =
  { upper : structure_item
  ; upper_rename : structure_item
  ; t_type_declaration : structure_item
  ; names : structure_item
  ; name : structure_item
  ; path : structure_item
  ; ord : structure_item
  ; type_ids : structure_item
  ; packed : structure_item
  }

let type_ids ~loc ~number_of_parameters ~unique_id =
  let open (val Ast_builder.make loc) in
  let functor_param_names =
    List.init number_of_parameters ~f:(fun i -> [%string "Typed_id_T%{(i + 1)#Int}"])
  in
  let functor_param_constrs =
    List.map functor_param_names ~f:(fun name ->
      ptyp_constr (Ldot (Lident name, "t") |> Located.mk) [])
  in
  let type_equal_type =
    ptyp_constr
      (Ldot (Ldot (Ldot (Lident "Base", "Type_equal"), "Id"), "t") |> Located.mk)
      [ ptyp_constr (Lident derived_on_name |> Located.mk) functor_param_constrs ]
  in
  let type_id =
    let pattern = ppat_var (Located.mk "type_id") in
    let expression =
      let expression =
        let pattern =
          ppat_constraint
            (ppat_construct (Lident "T" |> Located.mk) None)
            (ptyp_constr
               (Lident "t" |> Located.mk)
               (functor_param_constrs
                @ [ ptyp_constr (Lident unique_id |> Located.mk) [] ]))
        in
        let expression =
          pexp_constraint
            (pexp_ident (Lident "type_id" |> Located.mk))
            (ptyp_constr
               (Ldot (Ldot (Ldot (Lident "Base", "Type_equal"), "Id"), "t") |> Located.mk)
               [ ptyp_constr (Lident unique_id |> Located.mk) [] ])
        in
        pexp_fun Nolabel None pattern expression
      in
      pexp_newtype (Located.mk unique_id) expression
    in
    let vb = value_binding ~pat:pattern ~expr:expression in
    pstr_value Nonrecursive [ vb ]
  in
  let initial_expr =
    pmod_structure
      [ [%stri
        let type_id : [%t type_equal_type] =
          Base.Type_equal.Id.create ~name:"this" (fun _ -> Sexplib.Sexp.Atom "<opaque>")
        ;;]
      ; type_id
      ]
  in
  let expr =
    List.fold_right functor_param_names ~init:initial_expr ~f:(fun name acc ->
      pmod_functor
        (Named
           ( Some name |> Located.mk
           , pmty_ident (Ldot (Lident "Base", "T") |> Located.mk) ))
        acc)
  in
  pstr_module (module_binding ~name:(Some "Type_ids" |> Located.mk) ~expr)
;;

let packed ~loc ~core_type_params ~unique_id ~minimum_needed_parameters =
  let open (val Ast_builder.make loc) in
  let t_params = core_type_params @ [ ptyp_var unique_id ] in
  let t_type_constr = ptyp_constr (Lident "t" |> Located.mk) t_params in
  let field_type = ptyp_constr (Lident "field" |> Located.mk) t_params in
  let packed_field =
    let td =
      Typed_deriver_intf.generate_packed_field_type_declaration
        ~loc
        ~params:minimum_needed_parameters
        ~unique_parameter_id:unique_id
        ~t_type_constr
    in
    pstr_type Recursive [ td ]
  in
  let t_prime_type_declaration =
    let td =
      Typed_deriver_intf.generate_packed_t_prime_type_declaration
        ~loc
        ~params:minimum_needed_parameters
        ~core_type_params
        ~field_type
    in
    let td =
      { td with
        ptype_attributes =
          Typed_deriver_intf.disable_warning_37 ~loc :: td.ptype_attributes
      }
    in
    pstr_type Recursive [ td ]
  in
  let t_type_declaration =
    let td =
      Typed_deriver_intf.generate_packed_t_type_declaration ~loc ~core_type_params
    in
    pstr_type Recursive [ td ]
  in
  let compare = [%stri let compare _ _ = 0] in
  let equal = [%stri let equal _ _ = true] in
  let all = [%stri let all = [ { f = T T } ]] in
  let sexp_of_t = [%stri let sexp_of_t _ = Sexplib.Sexp.Atom "this"] in
  let t_of_sexp = [%stri let t_of_sexp _ = { f = T T }] in
  let pack = [%stri let pack _ = { f = T T }] in
  pstr_module
    (module_binding
       ~name:(Some "Packed" |> Located.mk)
       ~expr:
         (pmod_structure
            [ packed_field
            ; t_prime_type_declaration
            ; t_type_declaration
            ; compare
            ; equal
            ; all
            ; sexp_of_t
            ; t_of_sexp
            ; pack
            ]))
;;

let common ~loc ~minimum_needed_parameters ~core_type_params ~ctype ~unique_id =
  let open (val Ast_builder.make loc) in
  let upper =
    let td =
      type_declaration
        ~name:(Located.mk "typed_common_original")
        ~params:minimum_needed_parameters
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some ctype)
    in
    pstr_type Recursive [ td ]
  in
  let constructor =
    constructor_declaration
      ~name:(Located.mk "T")
      ~args:(Pcstr_tuple [])
      ~res:(Some (ptyp_constr (Lident "t" |> Located.mk) (core_type_params @ [ ctype ])))
  in
  let t_type_declaration =
    let td =
      type_declaration
        ~name:("t" |> Located.mk)
        ~params:
          (minimum_needed_parameters @ [ ptyp_var unique_id, (NoVariance, NoInjectivity) ])
        ~cstrs:[]
        ~kind:(Ptype_variant [ constructor ])
        ~private_:Public
        ~manifest:None
    in
    pstr_type Recursive [ td ]
  in
  let upper_rename =
    let td =
      type_declaration
        ~name:(Located.mk derived_on_name)
        ~params:minimum_needed_parameters
        ~cstrs:[]
        ~private_:Public
        ~kind:Ptype_abstract
        ~manifest:
          (Some
             (ptyp_constr (Lident "typed_common_original" |> Located.mk) core_type_params))
    in
    pstr_type Recursive [ td ]
  in
  let names = [%stri let names = [ "this" ]] in
  let name = [%stri let name _ = "this"] in
  let path = [%stri let path _ = []] in
  let ord = [%stri let __ord _ = [ 0 ]] in
  let type_ids =
    type_ids ~loc ~number_of_parameters:(List.length minimum_needed_parameters) ~unique_id
  in
  let packed = packed ~loc ~core_type_params ~unique_id ~minimum_needed_parameters in
  { upper; t_type_declaration; upper_rename; name; path; ord; type_ids; packed; names }
;;
