open! Base
open Ppxlib

type common_items =
  { upper : structure_item
  ; upper_rename : structure_item
  ; t_type_declaration : structure_item
  ; internal_gadt_declaration : structure_item
  ; names : structure_item
  ; name : structure_item
  ; path : structure_item
  ; ord : structure_item
  ; globalize0 : structure_item
  ; globalize : structure_item
  ; type_ids : structure_item
  ; packed : structure_item
  }

let type_ids ~loc ~number_of_parameters ~unique_id =
  let open (val Syntax.builder loc) in
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
      [ ptyp_constr (Lident Names.derived_on_name |> Located.mk) functor_param_constrs ]
  in
  let type_id =
    let pattern = ppat_var (Located.mk "type_id") in
    let expression =
      let expression =
        let pattern =
          ppat_constraint
            (ppat_construct (Lident "T" |> Located.mk) None)
            (Some
               (ptyp_constr
                  (Lident "t" |> Located.mk)
                  (functor_param_constrs
                   @ [ ptyp_constr (Lident unique_id |> Located.mk) [] ])))
            []
        in
        let expression =
          pexp_constraint
            (pexp_ident (Lident "type_id" |> Located.mk))
            (Some
               (ptyp_constr
                  (Ldot (Ldot (Ldot (Lident "Base", "Type_equal"), "Id"), "t")
                   |> Located.mk)
                  [ ptyp_constr (Lident unique_id |> Located.mk) [] ]))
            []
        in
        pexp_fun Nolabel None pattern expression
      in
      pexp_newtype (Located.mk unique_id) None expression
    in
    let vb = value_binding ~pat:pattern ~expr:expression ~modes:[] in
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
           , pmty_ident (Ldot (Lident "Base", "T") |> Located.mk)
           , [] )
         |> Ppxlib_jane.Shim.Functor_parameter.to_parsetree)
        acc)
  in
  pstr_module (module_binding ~name:(Some "Type_ids" |> Located.mk) ~expr)
;;

let packed ~loc ~core_type_params ~unique_id ~minimum_needed_parameters =
  let open (val Syntax.builder loc) in
  let t_params = core_type_params @ [ ptyp_var unique_id ] in
  let t_type_constr = ptyp_constr (Lident "t" |> Located.mk) t_params in
  let field_type = ptyp_constr (Lident "field" |> Located.mk) t_params in
  let packed_field =
    let td =
      Typed_deriver.generate_packed_field_type_declaration
        ~loc
        ~params:minimum_needed_parameters
        ~unique_parameter_id:unique_id
        ~t_type_constr
    in
    pstr_type Recursive [ td ]
  in
  let t_prime_type_declaration =
    let td =
      Typed_deriver.generate_packed_t_prime_type_declaration
        ~loc
        ~params:minimum_needed_parameters
        ~core_type_params
        ~field_type
    in
    let td =
      { td with
        ptype_attributes = Typed_deriver.disable_warning_37 ~loc :: td.ptype_attributes
      }
    in
    pstr_type Recursive [ td ]
  in
  let t_type_declaration =
    let td = Typed_deriver.generate_packed_t_type_declaration ~loc ~core_type_params in
    pstr_type Recursive [ td ]
  in
  let compare = [%stri let compare _ _ = 0] in
  let compare__local = [%stri let compare__local _ _ = 0] in
  let equal = [%stri let equal _ _ = true] in
  let equal__local = [%stri let equal__local _ _ = true] in
  let all = [%stri let all = [ { f = T T } ]] in
  let globalize = [%stri let globalize _ = { f = T T }] in
  let sexp_of_t = [%stri let sexp_of_t _ = Sexplib.Sexp.Atom "this"] in
  let sexp_of_t__local = [%stri let sexp_of_t__local _ = Sexplib.Sexp.Atom "this"] in
  let t_of_sexp = [%stri let t_of_sexp _ = { f = T T }] in
  let pack = [%stri let pack _ = { f = T T }] in
  let pack__local = [%stri let pack__local _ = { f = T T }] in
  pstr_module
    (module_binding
       ~name:(Some "Packed" |> Located.mk)
       ~expr:
         (pmod_structure
            [ packed_field
            ; t_prime_type_declaration
            ; t_type_declaration
            ; compare
            ; compare__local
            ; equal
            ; equal__local
            ; all
            ; globalize
            ; sexp_of_t
            ; sexp_of_t__local
            ; t_of_sexp
            ; pack
            ; pack__local
            ]))
;;

let common ~loc ~minimum_needed_parameters ~core_type_params ~ctype ~unique_id =
  let open (val Syntax.builder loc) in
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
  let t_params =
    minimum_needed_parameters @ [ ptyp_var unique_id, (NoVariance, NoInjectivity) ]
  in
  let t_type_declaration =
    let td =
      type_declaration
        ~name:("t" |> Located.mk)
        ~params:t_params
        ~cstrs:[]
        ~kind:(Ptype_variant [ constructor ])
        ~private_:Public
        ~manifest:None
    in
    pstr_type Recursive [ td ]
  in
  let internal_gadt_declaration =
    let core_type_params = List.map t_params ~f:fst in
    let type_ = ptyp_constr (Lident "t" |> Located.mk) core_type_params in
    let td =
      type_declaration
        ~name:(Type_kind.internal_gadt_name |> Located.mk)
        ~params:t_params
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some type_)
    in
    pstr_type Recursive [ td ]
  in
  let upper_rename =
    let td =
      type_declaration
        ~name:(Located.mk Names.derived_on_name)
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
  let globalize0 =
    let unique_parameter_id = Type_kind.generate_unique_id core_type_params in
    let var_arrow_type =
      ptyp_constr
        (Located.mk (Lident Type_kind.internal_gadt_name))
        (core_type_params @ [ ptyp_var unique_parameter_id ])
    in
    let constr_arrow_type =
      ptyp_constr
        (Located.mk (Lident Type_kind.internal_gadt_name))
        (List.map
           (core_type_params @ [ ptyp_var unique_parameter_id ])
           ~f:(fun core_type ->
             match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
             | Ptyp_var (name, _) -> ptyp_constr (Located.mk (Lident name)) []
             | _ -> core_type))
    in
    Typed_deriver.generate_new_typed_function
      ~loc
      ~function_name:"globalize0"
      ~core_type_params
      ~unique_parameter_id
      ~arg_modes:Ppxlib_jane.Shim.Modes.local
      ~result_modes:[]
      ~var_arrow_type
      ~constr_arrow_type
      ~function_body:
        [%expr
          function
          | T -> T]
      ~name_of_first_parameter:(Lident "t")
      ()
  in
  let globalize =
    let body =
      eabstract (List.map t_params ~f:(fun _ -> ppat_any)) [%expr fun t -> globalize0 t]
    in
    [%stri let globalize = [%e body]]
  in
  let type_ids =
    type_ids ~loc ~number_of_parameters:(List.length minimum_needed_parameters) ~unique_id
  in
  let packed = packed ~loc ~core_type_params ~unique_id ~minimum_needed_parameters in
  { upper
  ; t_type_declaration
  ; internal_gadt_declaration
  ; upper_rename
  ; name
  ; path
  ; ord
  ; globalize0
  ; globalize
  ; type_ids
  ; packed
  ; names
  }
;;
