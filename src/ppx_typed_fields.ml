open Base
open Import
open Ppxlib
open Type_kind_intf

type type_case =
  | Record of (label_declaration * granularity) list with_parameters
  | Tuple of (core_type * granularity) list with_parameters
  | Unit of unit with_parameters
  | Opaque of bool with_parameters
  | Unknown

(**
   Attached to core types (e.g.)

   {[
     type t = int * ((int * int)[@typed_fields.subproduct])[@@deriving typed_fields]
   ]} *)
let subproduct =
  Attribute.declare
    "typed_fields.subproduct"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;

(**
   Attached to label declarations (e.g.)

   {[
     type t = {
       a: int * int [@typed_fields.subproduct]
     ; b: int
     } [@@deriving typed_fields]
   ]} *)
let subfield =
  Attribute.declare
    "typed_fields.subproduct"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

(**
   Attached to opaque types for label declarations (e.g.)

   {[
     type t  [@@deriving typed_fields] [@@typed_fields.opaque]
   ]} *)
let opaque_attribute =
  Attribute.declare
    "typed_fields.opaque"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

(* Returns a tuple containing the minimum list of parameters that were needed and the
   positional ids of the parameters that were needed.

   For example:

   total_params = ('a, 'b, 'c, 'd)
   core_type = ('b * int * ('d * 'd))

   Since only 'b and 'd are needed, the output is:

   ('b, 'd), [1, 3]
*)
let find_minimum_parameters_needed ~total_params ~core_type =
  let parameter_name_to_index =
    List.foldi
      total_params
      ~init:(Map.empty (module String))
      ~f:(fun index acc (ctype, _) ->
        match ctype.ptyp_desc with
        | Ptyp_var name -> Map.set acc ~key:name ~data:index
        | _ -> acc)
  in
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
  in
  let parameters_needed =
    finder_of_types#core_type core_type (Set.empty (module String))
  in
  let needed_parameters_as_parameter_list =
    List.filter total_params ~f:(fun (param_type, _) ->
      match param_type.ptyp_desc with
      | Ptyp_var name -> Set.mem parameters_needed name
      | _ -> false)
  in
  let needed_parameters_as_id_list =
    Set.fold parameters_needed ~init:[] ~f:(fun acc parameter_name_needed ->
      match Map.find parameter_name_to_index parameter_name_needed with
      | Some id -> id :: acc
      | None -> acc)
  in
  needed_parameters_as_parameter_list, needed_parameters_as_id_list
;;

(** Identifies the fields that have a depth and attaches the depth and some extra
    information like the parameters that the subproduct needs which comes in handy
    when generating parts of the new ast. *)
let attach_granularity_to_tuple_types ~remove_attributes tuple_types params =
  List.map tuple_types ~f:(fun tuple_type ->
    match Attribute.get ~mark_as_seen:false subproduct tuple_type with
    | Some _ ->
      ( (if remove_attributes
         then tuple_type |> attribute_remover#core_type
         else tuple_type)
      , let minimum_needed_parameters, minimum_needed_parameter_ids =
          find_minimum_parameters_needed ~total_params:params ~core_type:tuple_type
        in
        Deep
          { minimum_needed_parameters
          ; minimum_needed_parameter_ids
          ; original_type_with_attributes = tuple_type
          } )
    | None -> tuple_type, Shallow)
;;

(** Identifies the fields that have a depth and attaches the depth and some extra
    information like the parameters that the subproduct needs which comes in handy
    when generating parts of the new ast.  *)
let attach_granularity_to_record_fields ~remove_attributes record_fields params =
  List.map record_fields ~f:(fun declaration ->
    match
      ( Attribute.get ~mark_as_seen:false subproduct declaration.pld_type
      , Attribute.get ~mark_as_seen:false subfield declaration )
    with
    | Some _, _ | _, Some _ ->
      ( (if remove_attributes
         then declaration |> attribute_remover#label_declaration
         else declaration)
      , let minimum_needed_parameters, minimum_needed_parameter_ids =
          find_minimum_parameters_needed
            ~total_params:params
            ~core_type:declaration.pld_type
        in
        Deep
          { minimum_needed_parameters
          ; minimum_needed_parameter_ids
          ; original_type_with_attributes = declaration.pld_type
          } )
    | None, _ -> declaration, Shallow)
;;

let raise_if_opaque_attribute_is_seen td =
  match Attribute.get opaque_attribute td with
  | Some _ ->
    Location.raise_errorf
      ~loc:td.ptype_loc
      "'typed_fields.opaque' can only be attached to opaque declarations."
  | None -> ()
;;

(** Identifies the case of the type declaration. *)
let identify_type_case ?(remove_attributes = false) td =
  (* Variance and injectivity are stripped from the parameters. *)
  let params =
    td.ptype_params |> List.map ~f:(fun (type_, _) -> type_, (NoVariance, NoInjectivity))
  in
  match td.ptype_kind, td.ptype_manifest with
  | Ptype_abstract, Some { ptyp_desc = Ptyp_constr ({ txt = Lident "unit"; _ }, _); _ } ->
    raise_if_opaque_attribute_is_seen td;
    Unit ((), params)
  | Ptype_abstract, Some { ptyp_desc = Ptyp_tuple tuple_types; _ } ->
    raise_if_opaque_attribute_is_seen td;
    Tuple (attach_granularity_to_tuple_types ~remove_attributes tuple_types params, params)
  | Ptype_abstract, None ->
    let should_generate =
      match Attribute.get opaque_attribute td with
      | Some _ -> true
      | None -> false
    in
    Opaque (should_generate, params)
  | Ptype_record fields, _ ->
    raise_if_opaque_attribute_is_seen td;
    Record (attach_granularity_to_record_fields ~remove_attributes fields params, params)
  | _ ->
    raise_if_opaque_attribute_is_seen td;
    Unknown
;;

let raise_if_nonrec ~loc = function
  | Nonrecursive ->
    Location.raise_errorf
      ~loc
      "nonrec is not compatible with the `typed_fields' preprocessor"
  | _ -> ()
;;

let is_record td =
  match td.ptype_kind, td.ptype_params with
  | Ptype_record _, _ -> true
  | _ -> false
;;

let is_unit td =
  match identify_type_case td with
  | Unit _ -> true
  | _ -> false
;;

let is_tuple td =
  match identify_type_case td with
  | Tuple _ -> true
  | _ -> false
;;

(** A subproduct tree is valid if and only if the following hold true.

    1. Subproduct annotations may only exist on tuples or on constr's.
    2. Subproduct annotations' patent must either also be a tuple and
    have a subproduct annotation or be the top-most annotation while
    deriving typed fields.
    3. For records, the subproduct annotation can be attached to either
    the label declaration or the type of the label declaration, but
    not both.
*)
let is_valid_subproduct_tree types_that_are_currently_being_defined td =
  let raise_if_tag_is_spotted =
    object
      inherit Ast_traverse.iter as super

      method! core_type ctype =
        match Attribute.get ~mark_as_seen:true subproduct ctype with
        | Some _ ->
          Location.raise_errorf
            ~loc:ctype.ptyp_loc
            "Typed fields' subproducts can only be attached on tuples. Furthermore, \
             typed_fields's suproducts' type's parent must be either another subproduct \
             or the top-level type deriving typed fields.  "
        | None -> super#core_type ctype
    end
  in
  let rec valid_use_of_subproducts ~ignore_current_type ~ctype ~has_skipped =
    match ctype.ptyp_desc with
    (* When a tuple is used as a subproduct. *)
    | Ptyp_tuple tuple_types ->
      (match Attribute.get ~mark_as_seen:true subproduct ctype with
       | Some _ when has_skipped ->
         Location.raise_errorf
           ~loc:ctype.ptyp_loc
           "typed_fields's suproducts' type's parent must be either another subproduct or \
            the top-level type deriving typed fields."
       | Some _ ->
         List.for_all tuple_types ~f:(fun ctype ->
           valid_use_of_subproducts ~ctype ~has_skipped:false ~ignore_current_type:false)
       | None ->
         List.for_all tuple_types ~f:(fun ctype ->
           valid_use_of_subproducts
             ~ctype
             ~has_skipped:(not ignore_current_type)
             ~ignore_current_type:false))
    | Ptyp_constr (name, type_parameters) ->
      (match Attribute.get ~mark_as_seen:true subproduct ctype with
       | Some _ ->
         (match name with
          | { txt = Lident name; _ }
            when Set.mem types_that_are_currently_being_defined name ->
            Location.raise_errorf
              ~loc:ctype.ptyp_loc
              "typed_field's subproducts cannot be attached to other types that are \
               mutually recursive."
          | _ -> ());
         List.iter type_parameters ~f:(fun ctype ->
           raise_if_tag_is_spotted#core_type ctype);
         true
       | None ->
         List.iter type_parameters ~f:(fun ctype ->
           raise_if_tag_is_spotted#core_type ctype);
         true)
    | _ ->
      raise_if_tag_is_spotted#core_type ctype;
      true
  in
  let valid_use_of_subproduct_for_label_declaration declaration =
    match
      ( Attribute.get ~mark_as_seen:true subfield declaration
      , Attribute.get ~mark_as_seen:true subproduct declaration.pld_type
      , declaration.pld_type.ptyp_desc )
    with
    | Some _, Some _, _ ->
      Location.raise_errorf
        ~loc:declaration.pld_type.ptyp_loc
        "Redundant typed_fields.subproduct annotation typed_fields.subproduct can be \
         attached to either the label declaration or the type of the label declaration, \
         not both. Perhaps you meant to attach the subproduct attribute on the core_type \
         somewhere deeper? "
    | Some _, _, Ptyp_constr ({ txt = Lident name; _ }, _) ->
      if Set.mem types_that_are_currently_being_defined name
      then
        Location.raise_errorf
          ~loc:declaration.pld_type.ptyp_loc
          "typed_field's subproducts cannot be attached to other types that are mutually \
           recursive."
      else ();
      valid_use_of_subproducts
        ~ctype:declaration.pld_type
        ~has_skipped:false
        ~ignore_current_type:true
    | _, Some _, Ptyp_constr ({ txt = Lident name; _ }, _) ->
      if Set.mem types_that_are_currently_being_defined name
      then
        Location.raise_errorf
          ~loc:declaration.pld_type.ptyp_loc
          "typed_field's subproducts cannot be attached to other types that are mutually \
           recursive."
      else ();
      valid_use_of_subproducts
        ~ctype:declaration.pld_type
        ~has_skipped:false
        ~ignore_current_type:false
    | Some _, None, Ptyp_tuple _ ->
      valid_use_of_subproducts
        ~ctype:declaration.pld_type
        ~has_skipped:false
        ~ignore_current_type:true
    | Some _, None, _ ->
      Location.raise_errorf
        ~loc:declaration.pld_loc
        "typed_fields's subproducts may only be attached to either tuple types or other \
         types deriving typed_fields."
    | None, _, _ ->
      valid_use_of_subproducts
        ~ctype:declaration.pld_type
        ~has_skipped:false
        ~ignore_current_type:false
  in
  match identify_type_case td with
  | Tuple (tuple_types, _) ->
    List.for_all tuple_types ~f:(fun (tuple_type, _) ->
      valid_use_of_subproducts
        ~ctype:tuple_type
        ~has_skipped:false
        ~ignore_current_type:false)
  | Record (label_declarations, _) ->
    List.for_all label_declarations ~f:(fun (declaration, _) ->
      valid_use_of_subproduct_for_label_declaration declaration)
  | _ -> true
;;

let is_mli_creation_supported td =
  match identify_type_case td with
  | Opaque (true, _) | Tuple _ | Record _ | Unit _ -> true
  | Unknown | Opaque (false, _) -> false
;;

let check_at_least_one_valid_mli_creation ~loc rec_flag tds =
  raise_if_nonrec ~loc rec_flag;
  let types_that_are_currently_being_defined =
    List.fold
      tds
      ~init:(Set.empty (module String))
      ~f:(fun acc td -> Set.add acc td.ptype_name.txt)
  in
  match
    List.filter tds ~f:is_mli_creation_supported
    |> List.filter ~f:(is_valid_subproduct_tree types_that_are_currently_being_defined)
  with
  | [] ->
    Location.raise_errorf
      ~loc
      "Unsupported use of 'typed_fields' inside of a module signature (you can only use \
       it on records, or on opaque types with the 'typed_fields.opaque' attribute)"
  | _ :: _ -> ()
;;

let is_supported td = List.exists [ is_record; is_unit; is_tuple ] ~f:(fun f -> f td)

let check_at_least_one_supported_type ~loc rec_flag tds =
  raise_if_nonrec ~loc rec_flag;
  let types_that_are_currently_being_defined =
    List.fold
      tds
      ~init:(Set.empty (module String))
      ~f:(fun acc td -> Set.add acc td.ptype_name.txt)
  in
  match
    ( List.filter tds ~f:is_supported
      |> List.filter ~f:(is_valid_subproduct_tree types_that_are_currently_being_defined)
    , tds )
  with
  | [], [ _ ] ->
    Location.raise_errorf
      ~loc
      "Unsupported use of typed_fields (you can only use it on records or tuples)."
  | [], _ ->
    Location.raise_errorf
      ~loc
      "'typed_fields' can only be applied on type definitions in which at least one type \
       definition is a record or tuple"
  | _ :: _, _ -> ()
;;

module Gen_struct = struct
  let module_struct_expr ~original_type ~original_kind ~td =
    let { ptype_name = { loc; _ }; _ } = td in
    match identify_type_case ~remove_attributes:true td with
    | Unit (_, params) | Tuple ([], params) ->
      Some
        (Typed_deriver_fields.gen_str
           (module Unit_kind_generator)
           ~loc
           ~original_type
           ~original_kind
           ~elements_to_convert:[]
           ~params)
    | Tuple (tuple_types, params) ->
      Some
        (Typed_deriver_fields.gen_str
           (module Tuple_kind_generator)
           ~loc
           ~original_type
           ~original_kind
           ~elements_to_convert:tuple_types
           ~params)
    | Record (fields, params) ->
      Some
        (Typed_deriver_fields.gen_str
           (module Record_kind_generator)
           ~original_type
           ~original_kind
           ~loc
           ~elements_to_convert:fields
           ~params)
    | _ -> None
  ;;

  let fields_of_td (td : type_declaration) : structure =
    let { ptype_name = { txt = name; loc }; _ } = td in
    let open (val Ast_builder.make loc) in
    let fields_module_name =
      if String.equal name "t" then "Typed_field" else "Typed_field_of_" ^ name
    in
    let expr =
      module_struct_expr
        ~original_type:
          (Some (generate_manifest_type_constr ~loc ~name ~params:td.ptype_params))
        ~original_kind:Ptype_abstract
        ~td
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
    check_at_least_one_supported_type ~loc rec_flag tds;
    List.concat_map tds ~f:fields_of_td
  ;;
end

module Gen_sig = struct
  let strip_depth_if_needed strip_depth elements =
    match strip_depth with
    | true -> List.map elements ~f:(fun (element, _) -> element, Shallow)
    | false -> elements
  ;;

  let get_singleton_modules modules = List.map modules ~f:(fun (f, _) -> f)

  let generate_module_type_for_shallow ~td : module_type option * signature_item list =
    let { ptype_name = { txt = name; loc }; _ } = td in
    let open (val Ast_builder.make loc) in
    match identify_type_case ~remove_attributes:true td with
    | Unit (_, _) -> Some (pmty_typeof (pmod_ident (Lident "Deep" |> Located.mk))), []
    | Tuple (tuple_types, _) ->
      let singleton_modules =
        Product_kind_generator.singleton_modules_signatures
          (module Tuple_generator)
          ~loc
          ~elements_to_convert:tuple_types
      in
      let module_type =
        pmty_typeof
          (List.fold
             singleton_modules
             ~init:(pmod_ident (Lident "Deep" |> Located.mk))
             ~f:(fun acc (_, name) ->
               pmod_apply acc (pmod_ident (Lident name |> Located.mk))))
      in
      Some module_type, get_singleton_modules singleton_modules
    | Opaque (true, params) ->
      ( Some
          (Generic_generator.opaque_signature
             (module Typed_deriver_fields)
             ~loc
             ~manifest_type:(Some (generate_manifest_type_constr ~loc ~name ~params))
             ~original_kind:Ptype_abstract
             ~params)
      , [] )
    | Record (fields, _) ->
      let singleton_modules =
        Product_kind_generator.singleton_modules_signatures
          (module Record_generator)
          ~loc
          ~elements_to_convert:fields
      in
      let module_type =
        pmty_typeof
          (List.fold
             singleton_modules
             ~init:(pmod_ident (Lident "Deep" |> Located.mk))
             ~f:(fun acc (_, name) ->
               pmod_apply acc (pmod_ident (Lident name |> Located.mk))))
      in
      Some module_type, get_singleton_modules singleton_modules
    | Unknown | Opaque (false, _) -> None, []
  ;;

  let generate_module_type_for_deep ~td ~strip_depth
    : module_type option * signature_item list
    =
    let { ptype_name = { txt = name; loc }; _ } = td in
    let open (val Ast_builder.make loc) in
    match identify_type_case ~remove_attributes:true td with
    | Unit (_, params) ->
      let ({ gadt_t = typ; upper; constructor_declarations = _; internal_gadt_rename }
           : core_type gen_t_result)
        =
        Generic_generator.gen_t
          ~loc
          ~original_type:(Some (generate_manifest_type_constr ~loc ~name ~params))
          ~original_kind:Ptype_abstract
          ~elements_to_convert:[]
          ~generate_constructors:Unit_kind_generator.constructor_declarations
          ~params
          ~upper_name:derived_on_name
      in
      ( Some
          (pmty_signature
             ([ psig_type Nonrecursive [ upper ]
              ; psig_type Recursive [ typ ]
              ; psig_type Nonrecursive [ internal_gadt_rename ]
              ]
              @ Typed_deriver_fields.generate_include_signature ~loc ~params))
      , [] )
    | Tuple (tuple_types, params) ->
      let ({ gadt_t = typ; upper; constructor_declarations = _; internal_gadt_rename }
           : core_type gen_t_result)
        =
        Generic_generator.gen_t
          ~loc
          ~original_type:(Some (generate_manifest_type_constr ~loc ~name ~params))
          ~original_kind:Ptype_abstract
          ~elements_to_convert:(strip_depth_if_needed strip_depth tuple_types)
          ~generate_constructors:Tuple_kind_generator.constructor_declarations
          ~params
          ~upper_name:derived_on_name
      in
      let singleton_modules =
        Product_kind_generator.singleton_modules_signatures
          (module Tuple_generator)
          ~loc
          ~elements_to_convert:tuple_types
      in
      ( Some
          (pmty_signature
             ([ psig_type Nonrecursive [ upper ]
              ; psig_type Recursive [ typ ]
              ; psig_type Nonrecursive [ internal_gadt_rename ]
              ]
              @ Typed_deriver_fields.generate_include_signature ~loc ~params))
      , get_singleton_modules singleton_modules )
    | Opaque (true, params) ->
      ( Some
          (Generic_generator.opaque_signature
             (module Typed_deriver_fields)
             ~loc
             ~manifest_type:(Some (generate_manifest_type_constr ~loc ~name ~params))
             ~original_kind:Ptype_abstract
             ~params)
      , [] )
    | Record (fields, params) ->
      let ({ gadt_t = typ; upper; constructor_declarations = _; internal_gadt_rename }
           : label_declaration gen_t_result)
        =
        Generic_generator.gen_t
          ~loc
          ~original_type:(Some (generate_manifest_type_constr ~loc ~name ~params))
          ~original_kind:Ptype_abstract
          ~elements_to_convert:(strip_depth_if_needed strip_depth fields)
          ~generate_constructors:Record_kind_generator.constructor_declarations
          ~params
          ~upper_name:derived_on_name
      in
      let singleton_modules =
        Product_kind_generator.singleton_modules_signatures
          (module Record_generator)
          ~loc
          ~elements_to_convert:fields
      in
      ( Some
          (pmty_signature
             ([ psig_type Nonrecursive [ upper ]
              ; psig_type Recursive [ typ ]
              ; psig_type Nonrecursive [ internal_gadt_rename ]
              ]
              @ Typed_deriver_fields.generate_include_signature ~loc ~params))
      , get_singleton_modules singleton_modules )
    | Unknown | Opaque (false, _) -> None, []
  ;;

  let generate_signature_with_name ~td ~fields_module_name
    : signature * signature_item list
    =
    let { ptype_name = { loc; _ }; _ } = td in
    let open (val Ast_builder.make loc) in
    let type_, singleton_modules = generate_module_type_for_shallow ~td in
    match type_ with
    | None -> [], singleton_modules
    | Some type_ ->
      ( [ psig_module
            (module_declaration ~name:(Located.mk (Some fields_module_name)) ~type_)
        ]
      , singleton_modules )
  ;;

  let fields_of_td (td : type_declaration) : signature =
    let { ptype_name = { txt = name; loc }; ptype_params = params; _ } = td in
    let open (val Ast_builder.make loc) in
    let fields_module_name =
      if String.equal name "t" then "Typed_field" else "Typed_field_of_" ^ name
    in
    let shallow =
      let shallow, singleton_modules =
        generate_signature_with_name ~td ~fields_module_name:"Shallow"
      in
      singleton_modules @ shallow
    in
    let deep =
      let module_type, _ = generate_module_type_for_deep ~td ~strip_depth:false in
      match identify_type_case td, module_type with
      | Tuple (tuple_types, _), Some base_module_type ->
        [ Product_kind_generator.deep_functor_signature
            (module Tuple_generator)
            ~loc
            ~elements_to_convert:tuple_types
            ~base_module_type
        ]
      | Record (record_fields, _), Some base_module_type ->
        [ Product_kind_generator.deep_functor_signature
            (module Record_generator)
            ~loc
            ~elements_to_convert:record_fields
            ~base_module_type
        ]
      | Opaque _, _ -> []
      | Unit _, Some module_type ->
        [ psig_module
            (module_declaration ~name:(Some "Deep" |> Located.mk) ~type_:module_type)
        ]
      | Unknown, _ | _, None -> []
    in
    let full_depth =
      match identify_type_case td with
      | Tuple (tuple_types, _) ->
        Product_kind_generator.full_depth_signature
          (module Tuple_generator)
          ~loc
          ~elements_to_convert:tuple_types
      | Record (record_fields, _) ->
        Product_kind_generator.full_depth_signature
          (module Record_generator)
          ~loc
          ~elements_to_convert:record_fields
      | Opaque (true, _) -> [ [%sigi: include module type of Shallow] ]
      | Unit _ ->
        [ (let module_type =
             Generic_generator.opaque_signature
               (module Typed_deriver_fields)
               ~loc
               ~manifest_type:(Some (generate_manifest_type_constr ~loc ~name ~params))
               ~original_kind:Ptype_abstract
               ~params
           in
           psig_include (include_infos module_type))
        ]
      | Unknown | Opaque (false, _) -> []
    in
    let signature = pmty_signature (deep @ shallow @ full_depth) in
    [ psig_module
        (module_declaration
           ~name:(Some fields_module_name |> Located.mk)
           ~type_:signature)
    ]
  ;;

  let generate ~loc ~path:_ (rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    check_at_least_one_valid_mli_creation ~loc rec_flag tds;
    List.filter tds ~f:is_mli_creation_supported |> List.concat_map ~f:fields_of_td
  ;;
end

module Gen_anonymous_struct = struct
  let generate ~loc ~path:_ recflag tds =
    let loc = { loc with loc_ghost = true } in
    check_at_least_one_supported_type ~loc recflag tds;
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
        "'typed_fields' can only be applied on type definitions in which at least one \
         type definition is a record or tuple"
    | [ td ] ->
      let local_type_name = generate_local_type_name td in
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
          ~td
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
    "typed_fields"
    ~extensions:
      [ Extension.declare
          "typed_fields"
          Extension.Context.module_expr
          Ast_pattern.(pstr (pstr_type __ __ ^:: nil))
          Gen_anonymous_struct.generate
      ]
;;

let fields =
  Deriving.add
    "typed_fields"
    ~str_type_decl:(Deriving.Generator.make Deriving.Args.empty Gen_struct.generate)
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.empty Gen_sig.generate)
;;

module For_testing = struct
  let expand_struct = Gen_struct.generate ~path:()
  let expand_sig = Gen_sig.generate ~path:()
  let expand_anonymous_struct = Gen_anonymous_struct.generate ~path:()
  let expand_variant_struct = Ppx_typed_variants.For_testing.expand_struct
  let expand_variant_sig = Ppx_typed_variants.For_testing.expand_sig

  let expand_variant_anonymous_struct =
    Ppx_typed_variants.For_testing.expand_anonymous_struct
  ;;
end
