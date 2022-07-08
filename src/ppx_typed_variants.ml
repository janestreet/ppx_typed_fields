open Base
open Import
open Ppxlib
open Type_kind_intf
open Variant_kind_generator_intf

(**
   Attached to constructor declarations.

   {[
     type t =

       | A of some_type_that_derives_typed_variants [@typed_variants.subvariant]
   ]} *)
let subvariant =
  Attribute.declare
    "typed_variants.subvariant"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

(**
   Attached to row fields' r tags.

   {[
     type t =
       [
         | `A [@typed_variants.subvariant]
       ]
   ]}
*)
let subvariant_row_fields =
  Attribute.declare
    "typed_variants.subvariant"
    Attribute.Context.rtag
    Ast_pattern.(pstr nil)
    ()
;;

(**
   Attached to constructor declarations.

   {[
     type t =

       | A of (int * int * int) [@typed_variants.subvariant]
   ]}
*)
let typed_fields =
  Attribute.declare
    "typed_fields"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

(**
   Attached to row fields' r tags.

   {[
     type t =
       [
         | `A [@typed_variants.subvariant]
       ]
   ]} *)

let typed_fields_row_fields =
  Attribute.declare "typed_fields" Attribute.Context.rtag Ast_pattern.(pstr nil) ()
;;

(**
   Attached to opaque types for label declarations (e.g.)

   {[
     type t  [@@deriving typed_variants] [@@typed_variants.opaque]
   ]} *)
let opaque_attribute =
  Attribute.declare
    "typed_variants.opaque"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

(** Returns a string set of the parameters used inside of an ast.*)
let finder_of_types =
  object
    inherit [(label, Base.String.comparator_witness) Set.t] Ast_traverse.fold as super

    method! core_type ctype acc =
      let acc =
        match ctype.ptyp_desc with
        | Ptyp_var name -> Set.add acc name
        | _ -> acc
      in
      super#core_type ctype acc
  end
;;

(** Returns a list with only the parameters that will be needed. *)
let find_minimum_parameters_needed ~total_params ~parameters_needed =
  let needed_parameters_as_parameter_list =
    List.filter total_params ~f:(fun (param_type, _) ->
      match param_type.ptyp_desc with
      | Ptyp_var name -> Set.mem parameters_needed name
      | _ -> false)
  in
  needed_parameters_as_parameter_list
;;

let find_minimum_needed_parameter_ids ~params ~parameters_needed =
  let parameter_name_to_index =
    List.foldi
      params
      ~init:(Map.empty (module String))
      ~f:(fun index acc (ctype, _) ->
        match ctype.ptyp_desc with
        | Ptyp_var name -> Map.set acc ~key:name ~data:index
        | _ -> acc)
  in
  Set.fold parameters_needed ~init:[] ~f:(fun acc parameter_name_needed ->
    match Map.find parameter_name_to_index parameter_name_needed with
    | Some id -> id :: acc
    | None -> acc)
;;

let raise_if_typed_fields_is_seen cd =
  let loc = cd.pcd_loc in
  match Attribute.get typed_fields cd with
  | Some _ ->
    Location.raise_errorf
      ~loc
      "Unsupported use of [@typed_fields]. It can only be attached to constructors that \
       have a subproduct attached to them (i.e. tuples, or anonymous records)."
  | None -> ()
;;

let raise_if_typed_fields_is_seen_on_row_field rf =
  let loc = rf.prf_loc in
  match Attribute.get typed_fields_row_fields rf with
  | Some _ ->
    Location.raise_errorf
      ~loc
      "Unsupported use of [@typed_fields]. It can only be attached to constructors that \
       have a subproduct attached to them (i.e. tuples, or anonymous records)."
  | None -> ()
;;

type relevant_core_types =
  | Polymorphic_variant of row_field list
  | Constr of core_type list
  | Not_supported

let identify_row_field rf =
  match rf.prf_desc with
  | Rtag (_, false, [ { ptyp_desc = Ptyp_constr (_, params); _ } ]) -> Constr params
  | Rtag (_, false, [ { ptyp_desc = Ptyp_variant (row_fields, Closed, None); _ } ]) ->
    Polymorphic_variant row_fields
  | _ -> Not_supported
;;

let raise_if_subvariant_is_seen =
  let error_message =
    "Unsupported use of subvariant annotation. Unexepected tag. They can only be \
     attached if the type is deriving typed variants or if the parent variant also has \
     subvariants attached. Additionally, if you are nesting the subvariant annotations, \
     then the path from any annotation to its parent must end up in the root type that \
     derives typed variants (There can be no 'subvariant' gaps)."
  in
  object
    inherit Ast_traverse.iter as super

    method! row_field rf =
      match Attribute.get subvariant_row_fields rf with
      | Some _ -> Location.raise_errorf ~loc:rf.prf_loc "%s" error_message
      | None -> super#row_field rf

    method! constructor_declaration cd =
      match Attribute.get subvariant cd with
      | Some _ -> Location.raise_errorf ~loc:cd.pcd_loc "%s" error_message
      | None -> super#constructor_declaration cd
  end
;;

(*  Given a core type, checks that its subvariants annotations are correct. The subvariant
    annotations are correct if the following hold:

    1. Subvariant annotations may only exist on polymorphic types or on constr's.
    2. Subvariant annotations' parent must either also be a constr and
    have a subvariant annotation or be the top-most annotation while
    deriving typed variants.
*)

let rec check_subvariant_annotations_for_row_field row_field =
  match identify_row_field row_field, Attribute.get subvariant_row_fields row_field with
  | Not_supported, _ -> raise_if_subvariant_is_seen#row_field row_field
  | Polymorphic_variant row_fields, Some _ ->
    List.iter row_fields ~f:check_subvariant_annotations_for_row_field
  | Polymorphic_variant row_fields, None ->
    List.iter row_fields ~f:raise_if_subvariant_is_seen#row_field
  | Constr ctypes, _ -> List.iter ctypes ~f:raise_if_subvariant_is_seen#core_type
;;

let has_typed_fields cd =
  match Attribute.get typed_fields cd with
  | Some _ -> true
  | None -> false
;;

let has_typed_fields_row_field rf =
  match Attribute.get typed_fields_row_fields rf with
  | Some _ -> true
  | None -> false
;;

let identify_constructor_declaration ~loc cd params =
  let open (val Ast_builder.make loc) in
  match cd.pcd_res, cd.pcd_args with
  (* No payload in the constructor. *)
  | None, Pcstr_tuple [] ->
    raise_if_subvariant_is_seen#constructor_declaration cd;
    raise_if_typed_fields_is_seen cd;
    No_values_constructor
      { constructor_name = cd.pcd_name.txt
      ; return_value_type = [%type: unit]
      ; is_polymorphic = false
      }
  (* Payload of a single constr type in the constructor. *)
  | None, Pcstr_tuple [ ({ ptyp_desc = Ptyp_constr (ident, inner_params); _ } as single) ]
    ->
    raise_if_typed_fields_is_seen cd;
    List.iter inner_params ~f:raise_if_subvariant_is_seen#core_type;
    let granularity =
      match Attribute.get subvariant cd with
      | Some _ -> Variant_kind_generator_intf.Constr_deep { ident; params = inner_params }
      | None -> Variant_kind_generator_intf.Shallow
    in
    let minimum_needed_parameters, minimum_needed_parameter_ids =
      let parameters_needed =
        finder_of_types#core_type single (Set.empty (module String))
      in
      let needed_parameters_as_id_list =
        find_minimum_needed_parameter_ids ~params ~parameters_needed
      in
      let minimum_needed_parameters =
        find_minimum_parameters_needed ~total_params:params ~parameters_needed
      in
      minimum_needed_parameters, needed_parameters_as_id_list
    in
    Single_value_constructor
      { constructor_name = cd.pcd_name.txt
      ; return_value_type = attribute_remover#core_type single
      ; return_value_type_with_original_attributes = single
      ; granularity
      ; minimum_needed_parameters
      ; minimum_needed_parameter_ids
      ; typed_fields = false
      ; is_polymorphic = false
      }
  (* Payload of a polymorphic variant type in the constructor. *)
  | ( None
    , Pcstr_tuple
        [ ({ ptyp_desc = Ptyp_variant (row_fields, Closed, None); _ } as single) ] ) ->
    raise_if_typed_fields_is_seen cd;
    let granularity =
      match Attribute.get subvariant cd with
      | Some _ ->
        List.iter row_fields ~f:check_subvariant_annotations_for_row_field;
        Variant_kind_generator_intf.Polymorphic_deep
      | None ->
        List.iter row_fields ~f:raise_if_subvariant_is_seen#row_field;
        Variant_kind_generator_intf.Shallow
    in
    let parameters_needed =
      finder_of_types#core_type single (Set.empty (module String))
    in
    let minimum_needed_parameter_ids =
      find_minimum_needed_parameter_ids ~params ~parameters_needed
    in
    let minimum_needed_parameters =
      find_minimum_parameters_needed ~total_params:params ~parameters_needed
    in
    Single_value_constructor
      { constructor_name = cd.pcd_name.txt
      ; return_value_type = attribute_remover#core_type single
      ; return_value_type_with_original_attributes = single
      ; granularity
      ; minimum_needed_parameters
      ; minimum_needed_parameter_ids
      ; typed_fields = false
      ; is_polymorphic = false
      }
  (* Payload of a single type in the constructor. *)
  | None, Pcstr_tuple [ single ] ->
    (match single.ptyp_desc with
     | Ptyp_tuple _ -> ()
     | _ -> raise_if_typed_fields_is_seen cd);
    raise_if_subvariant_is_seen#constructor_declaration cd;
    let parameters_needed =
      finder_of_types#core_type single (Set.empty (module String))
    in
    let minimum_needed_parameters =
      find_minimum_parameters_needed ~total_params:params ~parameters_needed
    in
    let minimum_needed_parameter_ids =
      find_minimum_needed_parameter_ids ~params ~parameters_needed
    in
    Single_value_constructor
      { constructor_name = cd.pcd_name.txt
      ; return_value_type = attribute_remover#core_type single
      ; return_value_type_with_original_attributes = single
      ; granularity = Shallow
      ; minimum_needed_parameters
      ; minimum_needed_parameter_ids
      ; typed_fields = has_typed_fields cd
      ; is_polymorphic = false
      }
  (* Anonymous tuple payload. *)
  | None, Pcstr_tuple multiple ->
    raise_if_subvariant_is_seen#constructor_declaration cd;
    let minimum_needed_parameters =
      let parameters_needed =
        List.fold
          multiple
          ~init:(Set.empty (module String))
          ~f:(fun acc ctype -> finder_of_types#core_type ctype acc)
      in
      find_minimum_parameters_needed ~total_params:params ~parameters_needed
    in
    Tuple_values_constructor
      { constructor_name = cd.pcd_name.txt
      ; return_value_type = ptyp_tuple multiple |> attribute_remover#core_type
      ; return_value_type_with_original_attributes = ptyp_tuple multiple
      ; tuple_types = multiple
      ; minimum_needed_parameters
      ; typed_fields = has_typed_fields cd
      }
  (* Anonymous record payload. *)
  | None, Pcstr_record label_declarations ->
    raise_if_subvariant_is_seen#constructor_declaration cd;
    let minimum_needed_parameters =
      let parameters_needed =
        List.fold
          label_declarations
          ~init:(Set.empty (module String))
          ~f:(fun acc label_declaration ->
            finder_of_types#label_declaration label_declaration acc)
      in
      find_minimum_parameters_needed ~total_params:params ~parameters_needed
    in
    let core_type_minimum_needed_params =
      List.map minimum_needed_parameters ~f:(fun (x, _) -> x)
    in
    let return_value_type_with_original_attributes =
      ptyp_constr
        (Ldot
           (Lident "Typed_variant_anonymous_records", cd.pcd_name.txt |> String.lowercase)
         |> Located.mk)
        core_type_minimum_needed_params
    in
    Anonymous_record_constructor
      { constructor_name = cd.pcd_name.txt
      ; return_value_type =
          return_value_type_with_original_attributes |> attribute_remover#core_type
      ; return_value_type_with_original_attributes
      ; minimum_needed_parameters
      ; label_declarations
      ; typed_fields = has_typed_fields cd
      }
  | _ ->
    Location.raise_errorf
      ~loc
      "Unsupported variant constructor. Typed variants only supports constructors with \
       no payload, or constructors defined with the 'of' keyword. There is no support \
       for GADT or constructor renaming."
;;

let identify_row_field ~loc rf params =
  let open (val Ast_builder.make loc) in
  check_subvariant_annotations_for_row_field rf;
  match rf.prf_desc with
  (*  Polymorphic constructor with no payload. *)
  | Rtag (label, true, []) ->
    raise_if_subvariant_is_seen#row_field rf;
    raise_if_typed_fields_is_seen_on_row_field rf;
    No_values_constructor
      { constructor_name = label.txt
      ; return_value_type = [%type: unit]
      ; is_polymorphic = true
      }
  (* Polymorphic constructor with a single constr as payload.  *)
  | Rtag
      (label, false, [ ({ ptyp_desc = Ptyp_constr (ident, inner_params); _ } as single) ])
    ->
    raise_if_typed_fields_is_seen_on_row_field rf;
    List.iter inner_params ~f:raise_if_subvariant_is_seen#core_type;
    let granularity =
      match Attribute.get subvariant_row_fields rf with
      | Some _ -> Variant_kind_generator_intf.Constr_deep { ident; params = inner_params }
      | None -> Variant_kind_generator_intf.Shallow
    in
    let parameters_needed =
      finder_of_types#core_type single (Set.empty (module String))
    in
    let minimum_needed_parameters =
      find_minimum_parameters_needed ~total_params:params ~parameters_needed
    in
    let minimum_needed_parameter_ids =
      find_minimum_needed_parameter_ids ~params ~parameters_needed
    in
    Single_value_constructor
      { constructor_name = label.txt
      ; return_value_type = attribute_remover#core_type single
      ; return_value_type_with_original_attributes = single
      ; granularity
      ; minimum_needed_parameters
      ; minimum_needed_parameter_ids
      ; typed_fields = false
      ; is_polymorphic = true
      }
  (* Constructor with a single constr as payload. *)
  | Rtag (label, false, [ ({ ptyp_desc = Ptyp_variant (_, Closed, None); _ } as single) ])
    ->
    raise_if_typed_fields_is_seen_on_row_field rf;
    let granularity =
      match Attribute.get subvariant_row_fields rf with
      | Some _ -> Variant_kind_generator_intf.Polymorphic_deep
      | None -> Variant_kind_generator_intf.Shallow
    in
    let parameters_needed =
      finder_of_types#core_type single (Set.empty (module String))
    in
    let minimum_needed_parameters =
      find_minimum_parameters_needed ~total_params:params ~parameters_needed
    in
    let minimum_needed_parameter_ids =
      find_minimum_needed_parameter_ids ~params ~parameters_needed
    in
    Single_value_constructor
      { constructor_name = label.txt
      ; return_value_type = attribute_remover#core_type single
      ; return_value_type_with_original_attributes = single
      ; granularity
      ; minimum_needed_parameters
      ; minimum_needed_parameter_ids
      ; typed_fields = false
      ; is_polymorphic = true
      }
  (* Constructor with any type as payload. *)
  | Rtag (label, false, [ single ]) ->
    (match single.ptyp_desc with
     | Ptyp_tuple _ -> ()
     | _ -> raise_if_typed_fields_is_seen_on_row_field rf);
    raise_if_subvariant_is_seen#row_field rf;
    let parameters_needed =
      finder_of_types#core_type single (Set.empty (module String))
    in
    let minimum_needed_parameters =
      find_minimum_parameters_needed ~total_params:params ~parameters_needed
    in
    let minimum_needed_parameter_ids =
      find_minimum_needed_parameter_ids ~params ~parameters_needed
    in
    Single_value_constructor
      { constructor_name = label.txt
      ; return_value_type = attribute_remover#core_type single
      ; return_value_type_with_original_attributes = single
      ; granularity = Shallow
      ; minimum_needed_parameters
      ; minimum_needed_parameter_ids
      ; typed_fields = has_typed_fields_row_field rf
      ; is_polymorphic = true
      }
  | _ ->
    Location.raise_errorf
      ~loc
      "Unsupported polymorphic variant constructor. Typed variants only supports \
       constructors with no payload, or constructors defined with the 'of' keyword. \
       There is no support for GADT or constructor renaming."
;;

let raise_if_opaque_attribute_is_seen td =
  match Attribute.get opaque_attribute td with
  | Some _ ->
    Location.raise_errorf
      ~loc:td.ptype_loc
      "'typed_variants.opaque' can only be attached to opaque declarations."
  | None -> ()
;;

let identify_type_case ~loc td =
  (* Variance and injectivity are stripped from the parameters. *)
  let params =
    td.ptype_params |> List.map ~f:(fun (type_, _) -> type_, (NoVariance, NoInjectivity))
  in
  match td.ptype_kind, td.ptype_manifest with
  | Ptype_variant [], _ ->
    raise_if_opaque_attribute_is_seen td;
    Nothing ((), params)
  | Ptype_variant constructor_declarations, _ ->
    raise_if_opaque_attribute_is_seen td;
    Variant
      ( List.map constructor_declarations ~f:(fun cd ->
          identify_constructor_declaration ~loc cd params)
      , params )
  | Ptype_abstract, Some { ptyp_desc = Ptyp_variant (row_fields, Closed, None); _ } ->
    raise_if_opaque_attribute_is_seen td;
    Variant
      ( List.map row_fields ~f:(fun row_field -> identify_row_field ~loc row_field params)
      , params )
  | Ptype_abstract, None ->
    let generate_opaque_codegen =
      match Attribute.get opaque_attribute td with
      | Some _ -> true
      | None -> false
    in
    Opaque (generate_opaque_codegen, params)
  | _ ->
    raise_if_opaque_attribute_is_seen td;
    Unknown
;;

let raise_if_nonrec ~loc = function
  | Nonrecursive ->
    Location.raise_errorf
      ~loc
      "nonrec is not compatible with the `typed_variants' preprocessor"
  | _ -> ()
;;

let is_mli_creation_supported ~loc td =
  match identify_type_case ~loc td with
  | Opaque (true, _) | Variant _ | Nothing _ -> true
  | Unknown | Opaque (false, _) -> false
;;

let check_at_least_one_valid_mli_creation ~loc rec_flag tds =
  raise_if_nonrec ~loc rec_flag;
  match List.exists tds ~f:(is_mli_creation_supported ~loc) with
  | false ->
    Location.raise_errorf
      ~loc
      "Unsupported use of 'typed_variants' at least one type in the type declaration \
       must be either a variant or an opaque type with the 'typed_variants.opaque' \
       attribute"
  | true -> ()
;;

let check_at_least_one_valid_ml_creation ~loc rec_flag tds =
  raise_if_nonrec ~loc rec_flag;
  let is_mli_creation_supported td =
    match identify_type_case ~loc td with
    | Variant _ | Nothing _ -> true
    | Unknown | Opaque _ -> false
  in
  match List.exists tds ~f:is_mli_creation_supported with
  | false ->
    Location.raise_errorf
      ~loc
      "Unsupported use of 'typed_variants' at least one type in the type declaration \
       must be a variant or a closed polymorphic variant"
  | true -> ()
;;

module Gen_struct = struct
  let module_struct_expr ~original_type ~original_kind ~td_case ~loc =
    match td_case with
    | Unknown | Opaque _ -> None
    | Variant (supported_constructors, params) ->
      Some
        (Typed_deriver_variants.gen_str
           (module Variant_generator)
           ~loc
           ~original_type
           ~original_kind
           ~elements_to_convert:supported_constructors
           ~params
           ~td_case)
    | Nothing (_, params) ->
      Some
        (Typed_deriver_variants.gen_str
           (module Nothing_generator)
           ~loc
           ~original_type
           ~original_kind
           ~elements_to_convert:[]
           ~params
           ~td_case)
  ;;

  let variants_of_td ~loc td =
    let name = td.ptype_name.txt in
    let open (val Ast_builder.make loc) in
    let td_case = identify_type_case ~loc td in
    let fields_module_name =
      if String.equal name "t" then "Typed_variant" else "Typed_variant_of_" ^ name
    in
    let expr =
      module_struct_expr
        ~original_type:
          (Some (generate_manifest_type_constr ~loc ~name ~params:td.ptype_params))
        ~original_kind:Ptype_abstract
        ~td_case
        ~loc
    in
    match expr with
    | None -> []
    | Some expr ->
      [ pstr_module
          (module_binding
             ~name:(Located.mk (Some fields_module_name))
             ~expr:(pmod_structure expr))
      ]
  ;;

  let generate ~loc ~path:_ (rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    check_at_least_one_valid_ml_creation ~loc rec_flag tds;
    List.map tds ~f:(variants_of_td ~loc) |> List.concat
  ;;
end

module Gen_sig = struct
  let generate_singleton_signatures ~loc ~td_case =
    match td_case with
    | Variant (constructors, _) ->
      Variant_generator.singleton_modules_signatures
        ~loc
        ~elements_to_convert:constructors
    | _ -> []
  ;;

  let generate_module_type_for_deep ~loc ~td_case ~name ~strip_depth =
    let open (val Ast_builder.make loc) in
    let unstripped_td_case = td_case in
    let td_case = if strip_depth then strip_depth_from_td_case td_case else td_case in
    match td_case with
    | Unknown | Opaque (false, _) -> None, []
    | Opaque (true, params) | Nothing (_, params) ->
      ( Some
          (Generic_generator.opaque_signature
             (module Typed_deriver_variants)
             ~loc
             ~manifest_type:
               (Some (Type_kind_intf.generate_manifest_type_constr ~loc ~name ~params))
             ~original_kind:Ptype_abstract
             ~params)
      , [] )
    | Variant (constructors, params) ->
      let ({ gadt_t = typ; upper; internal_gadt_rename; constructor_declarations = _ }
           : supported_constructor_declaration gen_t_result)
        =
        Generic_generator.gen_t
          ~loc
          ~original_type:(Some (generate_manifest_type_constr ~loc ~name ~params))
          ~original_kind:Ptype_abstract
          ~elements_to_convert:
            (List.map constructors ~f:(fun constr -> constr, Type_kind_intf.Shallow))
          ~generate_constructors:Variant_generator.generate_constructor_declarations
          ~params
          ~upper_name:derived_on_name
      in
      let type_ =
        pmty_signature
          ([ psig_type Nonrecursive [ upper ]
           ; psig_type Recursive [ typ ]
           ; psig_type Nonrecursive [ internal_gadt_rename ]
           ]
           @ Typed_deriver_variants.generate_include_signature ~loc ~params)
      in
      Some type_, generate_singleton_signatures ~loc ~td_case:unstripped_td_case
  ;;

  let generate_module_type_for_shallow ~loc ~td_case ~name =
    let open (val Ast_builder.make loc) in
    match td_case with
    | Unknown | Opaque (false, _) -> None, []
    | Opaque (true, params) ->
      ( Some
          (Generic_generator.opaque_signature
             (module Typed_deriver_variants)
             ~loc
             ~manifest_type:
               (Some (Type_kind_intf.generate_manifest_type_constr ~loc ~name ~params))
             ~original_kind:Ptype_abstract
             ~params)
      , [] )
    | Nothing _ ->
      let type_ = pmty_typeof (pmod_ident (Lident "Deep" |> Located.mk)) in
      Some type_, generate_singleton_signatures ~loc ~td_case
    | Variant (constructors, _) ->
      let type_ =
        List.filter_map constructors ~f:(fun element ->
          match element with
          | Single_value_constructor { granularity = Constr_deep _; _ }
          | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
            Some
              ("Singleton_for_"
               ^ (Variant_kind_generator_intf.supported_constructor_name element
                  |> String.lowercase))
          | _ -> None)
        |> List.fold
             ~init:(pmod_ident (Lident "Deep" |> Located.mk))
             ~f:(fun acc name -> pmod_apply acc (pmod_ident (Lident name |> Located.mk)))
        |> pmty_typeof
      in
      Some type_, generate_singleton_signatures ~loc ~td_case
  ;;

  let generate_signature ~loc ~td_case ~name_of_original_type =
    let open (val Ast_builder.make loc) in
    let type_, singleton_modules =
      generate_module_type_for_shallow ~loc ~td_case ~name:name_of_original_type
    in
    match type_ with
    | None -> []
    | Some type_ ->
      singleton_modules
      @ [ psig_module (module_declaration ~name:(Some "Shallow" |> Located.mk) ~type_) ]
  ;;

  let generate_anonymous_record_module ~loc td_case =
    let open (val Ast_builder.make loc) in
    let td_sig_items =
      match td_case with
      | Opaque _ | Unknown | Nothing _ -> []
      | Variant (constructors_to_convert, _) ->
        Typed_deriver_variants.generate_anonymous_records_sig
          ~loc
          ~elements_to_convert:constructors_to_convert
    in
    psig_module
      (module_declaration
         ~name:(Some "Typed_variant_anonymous_records" |> Located.mk)
         ~type_:(pmty_signature td_sig_items))
  ;;

  let generate_tuples_module ~loc td_case =
    let open (val Ast_builder.make loc) in
    let td_sig_items =
      match td_case with
      | Opaque _ | Unknown | Nothing _ -> []
      | Variant (constructors_to_convert, _) ->
        Typed_deriver_variants.generate_tuples_sig
          ~loc
          ~elements_to_convert:constructors_to_convert
    in
    psig_module
      (module_declaration
         ~name:(Some "Typed_variant_tuples" |> Located.mk)
         ~type_:(pmty_signature td_sig_items))
  ;;

  let generate_deep_module_signature ~loc ~td_case ~name =
    let open (val Ast_builder.make loc) in
    let base_module_type, _ =
      generate_module_type_for_deep ~loc ~td_case ~name ~strip_depth:false
    in
    match td_case, base_module_type with
    | Opaque _, _ | _, None -> []
    | Unknown, Some base_module_type | Nothing _, Some base_module_type ->
      [ Variant_generator.deep_functor_signature
          ~loc
          ~elements_to_convert:[]
          ~base_module_type
      ]
    | Variant (constructors_to_convert, _), Some base_module_type ->
      [ Variant_generator.deep_functor_signature
          ~loc
          ~elements_to_convert:constructors_to_convert
          ~base_module_type
      ]
  ;;

  let generate_full_depth_module_signature ~loc ~td_case =
    let open (val Ast_builder.make loc) in
    match td_case with
    | Variant (elements, _) ->
      Variant_generator.full_depth_signature ~loc ~elements_to_convert:elements
    | Opaque _ -> [ [%sigi: include module type of Shallow] ]
    | Nothing _ | Unknown -> [ [%sigi: include module type of Deep] ]
  ;;

  let variants_of_td ~loc td =
    let name = td.ptype_name.txt in
    let open (val Ast_builder.make loc) in
    let td_case = identify_type_case ~loc td in
    let fields_module_name =
      if String.equal name "t" then "Typed_variant" else "Typed_variant_of_" ^ name
    in
    let anonymous_record_module = generate_anonymous_record_module ~loc td_case in
    let tuples_module = generate_tuples_module ~loc td_case in
    let shallow = generate_signature ~loc ~td_case ~name_of_original_type:name in
    let deep = generate_deep_module_signature ~loc ~td_case ~name in
    let full_depth = generate_full_depth_module_signature ~loc ~td_case in
    let signature =
      pmty_signature
        ([ anonymous_record_module; tuples_module ] @ deep @ shallow @ full_depth)
    in
    [ psig_module
        (module_declaration
           ~name:(Some fields_module_name |> Located.mk)
           ~type_:signature)
    ]
  ;;

  let generate ~loc ~path:_ (rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    check_at_least_one_valid_mli_creation ~loc rec_flag tds;
    List.filter tds ~f:(is_mli_creation_supported ~loc)
    |> List.map ~f:(variants_of_td ~loc)
    |> List.concat
  ;;
end

module Gen_anonymous_struct = struct
  let generate ~loc ~path:_ recflag tds =
    let loc = { loc with loc_ghost = true } in
    check_at_least_one_valid_ml_creation ~loc recflag tds;
    let open (val Ast_builder.make loc) in
    let loc_ghoster =
      object
        inherit Ast_traverse.map as super
        method! location loc = super#location { loc with loc_ghost = true }
      end
    in
    let tds =
      List.map tds ~f:(fun td ->
        td |> name_type_params_in_td |> loc_ghoster#type_declaration)
    in
    match tds with
    | [] | _ :: _ :: _ ->
      Location.raise_errorf
        ~loc
        "'typed_variants' rewriter can only be applied on type definitions of either \
         variants or polymorphic variants."
    | [ td ] ->
      (* Needs to be an identifier that does not collide with any other types
         from the Typed_fields_lib.S signature. *)
      let local_type_name = Type_kind_intf.generate_local_type_name td in
      let td = { td with ptype_name = { td.ptype_name with txt = local_type_name } } in
      let module_expr =
        Gen_struct.module_struct_expr
          ~original_type:
            (Some
               (generate_manifest_type_constr
                  ~loc
                  ~name:local_type_name
                  ~params:td.ptype_params))
          ~original_kind:Ptype_abstract
          ~loc
          ~td_case:(identify_type_case ~loc td)
      in
      let open (val Ast_builder.make loc) in
      (match module_expr with
       | Some expr ->
         let structure =
           { (pmod_structure expr) with pmod_attributes = [ disable_warning_32 ~loc ] }
         in
         let local_type_declaration =
           let local_type_declaration = attribute_remover#type_declaration td in
           pstr_type Recursive [ local_type_declaration ]
         in
         pmod_structure [ local_type_declaration; pstr_include (include_infos structure) ]
       | None -> pmod_structure [])
  ;;
end

let () =
  Driver.register_transformation
    "typed_variants"
    ~extensions:
      [ Extension.declare
          "typed_variants"
          Extension.Context.module_expr
          Ast_pattern.(pstr (pstr_type __ __ ^:: nil))
          Gen_anonymous_struct.generate
      ]
;;

let variants =
  Deriving.add
    "typed_variants"
    ~str_type_decl:(Deriving.Generator.make Deriving.Args.empty Gen_struct.generate)
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.empty Gen_sig.generate)
;;

module For_testing = struct
  let expand_struct = Gen_struct.generate ~path:()
  let expand_sig = Gen_sig.generate ~path:()
  let expand_anonymous_struct = Gen_anonymous_struct.generate ~path:()
end
