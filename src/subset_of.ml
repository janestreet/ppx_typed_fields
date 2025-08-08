open! Base
open Ppxlib

let generate_str ~loc ~typ_name ~fields ~params ~super =
  let open (val Syntax.builder loc) in
  let of_suffix = if String.equal typ_name "t" then "" else [%string "_of_%{typ_name}"] in
  let function_name = [%string "superset_field%{of_suffix}"] in
  let core_type_params = Type_kind.generate_core_type_params params in
  let unique_parameter_id = Type_kind.generate_unique_id core_type_params in
  let parameters_as_constrs =
    List.map core_type_params ~f:(fun type_ ->
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree type_.ptyp_desc with
      | Ptyp_var (name, _) -> ptyp_constr (Lident name |> Located.mk) []
      | _ -> type_)
  in
  let var_arrow_type =
    ptyp_constr
      (Ldot (super, "t") |> Located.mk)
      (core_type_params @ [ ptyp_var unique_parameter_id ])
  in
  let constr_arrow_type =
    ptyp_constr
      (Ldot (super, "t") |> Located.mk)
      (parameters_as_constrs
       @ [ ptyp_constr (Located.mk (Lident unique_parameter_id)) [] ])
  in
  let function_body =
    pexp_function
      (List.map fields ~f:(fun (label, param) ->
         let constr = label.pld_name.txt |> String.capitalize |> Lident |> Located.mk in
         let pat_payload =
           match (param : Type_kind.granularity) with
           | Deep _ -> Some (ppat_var (Located.mk "subproduct"))
           | Shallow -> None
         in
         let expr_payload =
           match param with
           | Deep _ -> Some (pexp_ident (Located.mk (Lident "subproduct")))
           | Shallow -> None
         in
         case
           ~lhs:(ppat_construct constr pat_payload)
           ~rhs:[%expr [%e pexp_construct constr expr_payload]]
           ~guard:None))
  in
  Typed_deriver.generate_new_typed_function
    ~loc
    ~function_name
    ~core_type_params
    ~unique_parameter_id
    ~arg_modes:Ppxlib_jane.Shim.Modes.local
    ~result_modes:Ppxlib_jane.Shim.Modes.local
    ~var_arrow_type
    ~constr_arrow_type
    ~name_of_first_parameter:(Ldot (Lident [%string "Typed_field%{of_suffix}"], "t"))
    ~function_body
    ()
;;

let generate_sig ~loc ~typ_name ~params ~super =
  let open (val Syntax.builder loc) in
  let of_suffix = if String.equal typ_name "t" then "" else [%string "_of_%{typ_name}"] in
  let function_name = [%string "superset_field%{of_suffix}"] in
  let typed_fields = Ldot (Lident [%string "Typed_field%{of_suffix}"], "t") in
  let core_type_params = Type_kind.generate_core_type_params params in
  let unique_parameter_id = Type_kind.generate_unique_id core_type_params in
  let parameter_names =
    List.filter_map core_type_params ~f:(fun { ptyp_desc; _ } ->
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
      | Ptyp_var (name, _) -> Some (Located.mk name, None)
      | _ -> None)
  in
  let t_type_parameters = parameter_names @ [ Located.mk unique_parameter_id, None ] in
  let var_arrow_type =
    ptyp_constr
      (Ldot (super, "t") |> Located.mk)
      (core_type_params @ [ ptyp_var unique_parameter_id ])
  in
  let function_type =
    ptyp_poly
      t_type_parameters
      (ptyp_arrow
         { arg_label = Nolabel
         ; arg_type =
             ptyp_constr
               (Located.mk typed_fields)
               (core_type_params @ [ ptyp_var unique_parameter_id ])
         ; arg_modes = Ppxlib_jane.Shim.Modes.local
         }
         { result_type = var_arrow_type; result_modes = Ppxlib_jane.Shim.Modes.local })
  in
  psig_value
    (Ppxlib_jane.Shim.Value_description.create
       ~name:(Located.mk function_name)
       ~type_:function_type
       ~modalities:[]
       ~prim:[]
       ~loc)
;;
