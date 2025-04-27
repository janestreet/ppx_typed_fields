open! Base
open Ppxlib
include Typed_deriver_intf.Definitions

let generate_packed_field_type_declaration
  ~loc
  ~params
  ~unique_parameter_id
  ~t_type_constr
  =
  let open (val Syntax.builder loc) in
  let field_params =
    params @ [ ptyp_var unique_parameter_id, (NoVariance, NoInjectivity) ]
  in
  type_declaration
    ~name:(Located.mk "field")
    ~params:field_params
    ~cstrs:[]
    ~kind:Ptype_abstract
    ~private_:Public
    ~manifest:(Some t_type_constr)
;;

let generate_packed_t_prime_type_declaration ~loc ~params ~core_type_params ~field_type =
  let open (val Syntax.builder loc) in
  type_declaration
    ~name:(Located.mk "t'")
    ~params
    ~cstrs:[]
    ~private_:Public
    ~manifest:None
    ~kind:
      (Ptype_variant
         [ constructor_declaration
             ~name:(Located.mk "T")
             ~args:
               (Pcstr_tuple
                  [ field_type |> Ppxlib_jane.Shim.Pcstr_tuple_arg.of_core_type ])
             ~res:(Some (ptyp_constr (Lident "t'" |> Located.mk) core_type_params))
         ])
;;

let generate_packed_t_type_declaration ~loc ~core_type_params =
  let open (val Syntax.builder loc) in
  let ty =
    type_declaration
      ~name:(Located.mk "t")
      ~cstrs:[]
      ~private_:Public
      ~manifest:None
      ~params:[]
      ~kind:
        (Ptype_record
           [ label_declaration
               ~name:(Located.mk "f")
               ~mutable_:Immutable
               ~type_:
                 (ptyp_poly
                    (List.filter_map core_type_params ~f:(fun param ->
                       match
                         Ppxlib_jane.Shim.Core_type_desc.of_parsetree param.ptyp_desc
                       with
                       | Ptyp_var (name, _) -> Some (Located.mk name, None)
                       | _ -> None))
                    (ptyp_constr (Lident "t'" |> Located.mk) core_type_params))
               ~modalities:[]
           ])
  in
  let attribute = attribute ~name:(Located.mk "unboxed") ~payload:(PStr []) in
  { ty with ptype_attributes = [ attribute ] }
;;

let disable_warning_37 ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-37") [] ])
;;

let generate_new_typed_function
  ~loc
  ~function_name
  ~core_type_params
  ~unique_parameter_id
  ~var_arrow_type
  ~constr_arrow_type
  ~function_body
  ~name_of_first_parameter
  =
  let open (val Syntax.builder loc) in
  let parameter_names =
    List.filter_map core_type_params ~f:(fun { ptyp_desc; _ } ->
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
      | Ptyp_var (name, _) -> Some (Located.mk name, None)
      | _ -> None)
  in
  let t_type_parameters = parameter_names @ [ Located.mk unique_parameter_id, None ] in
  let function_type =
    ptyp_poly
      t_type_parameters
      (ptyp_arrow
         { arg_label = Nolabel
         ; arg_type =
             ptyp_constr
               (Located.mk name_of_first_parameter)
               (core_type_params @ [ ptyp_var unique_parameter_id ])
         ; arg_modes = []
         }
         { result_type = var_arrow_type; result_modes = [] })
  in
  let function_expression =
    let parameters_as_constrs =
      List.map core_type_params ~f:(fun type_ ->
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree type_.ptyp_desc with
        | Ptyp_var (name, _) -> ptyp_constr (Lident name |> Located.mk) []
        | _ -> type_)
    in
    let inner_new_type =
      pexp_newtype
        (Located.mk unique_parameter_id)
        None
        (pexp_constraint
           function_body
           (Some
              (ptyp_arrow
                 { arg_label = Nolabel
                 ; arg_type =
                     ptyp_constr
                       (Located.mk name_of_first_parameter)
                       (parameters_as_constrs
                        @ [ ptyp_constr (Located.mk (Lident unique_parameter_id)) [] ])
                 ; arg_modes = []
                 }
                 { result_type = constr_arrow_type; result_modes = [] }))
           [])
    in
    List.fold_right parameter_names ~init:inner_new_type ~f:(fun (name, kind) acc ->
      pexp_newtype name kind acc)
  in
  pstr_value
    Nonrecursive
    [ value_binding
        ~pat:
          (ppat_constraint (ppat_var (Located.mk function_name)) (Some function_type) [])
        ~expr:function_expression
        ~modes:[]
    ]
;;

let at_least_one_subproduct elements_to_convert =
  List.exists elements_to_convert ~f:(fun (_, granularity) ->
    match granularity with
    | Type_kind.Shallow -> false
    | Type_kind.Deep _ -> true)
;;
