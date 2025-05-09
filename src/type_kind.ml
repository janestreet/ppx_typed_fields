open! Base
open Ppxlib
include Type_kind_intf.Definitions

let internal_gadt_name = "typed__t"

let generate_core_type_params params =
  List.map params ~f:(fun (core_type, _) -> core_type)
;;

let generate_unique_name ~start ~identifiers_to_avoid =
  let rec loop curr =
    if Set.mem identifiers_to_avoid curr then loop (curr ^ "_") else curr
  in
  loop start
;;

let generate_local_type_name td =
  let finder_of_type_names_in_use =
    object
      inherit [(string, String.comparator_witness) Set.t] Ast_traverse.fold as super

      method! core_type ctype acc =
        let acc =
          match ctype.ptyp_desc with
          | Ptyp_constr ({ txt = Lident name; _ }, _) -> Set.add acc name
          | _ -> acc
        in
        super#core_type ctype acc
    end
  in
  let identifiers_to_avoid =
    finder_of_type_names_in_use#type_declaration td (Set.empty (module String))
  in
  generate_unique_name ~start:"local_type" ~identifiers_to_avoid
;;

let generate_manifest_type_constr ~loc ~name ~params =
  let open (val Syntax.builder loc) in
  let core_type_params = List.map params ~f:(fun (core_type, _) -> core_type) in
  ptyp_constr (Located.mk (Lident name)) core_type_params
;;

let disable_warning_32 ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-32") [] ])
;;

let generate_creator_type_declaration
  ~loc
  ~unique_parameter_id
  ~core_type_params
  ~params
  ~t_name
  =
  let open (val Syntax.builder loc) in
  let creator_function_type =
    ptyp_poly
      [ Located.mk unique_parameter_id, None ]
      (ptyp_arrow
         { arg_label = Nolabel
         ; arg_type =
             ptyp_constr
               (Located.mk (Lident t_name))
               (core_type_params @ [ ptyp_var unique_parameter_id ])
         ; arg_modes = Ppxlib_jane.Shim.Modes.local
         }
         { result_type = ptyp_var unique_parameter_id; result_modes = [] })
  in
  type_declaration
    ~name:(Located.mk "creator")
    ~params
    ~cstrs:[]
    ~private_:Public
    ~manifest:None
    ~kind:
      (Ptype_record
         [ label_declaration
             ~name:(Located.mk "f")
             ~mutable_:Immutable
             ~type_:creator_function_type
             ~modalities:[]
         ])
;;

let attribute_remover =
  object
    inherit Ast_traverse.map as super

    method! core_type ctype =
      let ctype = { ctype with ptyp_attributes = [] } in
      super#core_type ctype

    method! label_declaration ld =
      let ld = { ld with pld_attributes = [] } in
      super#label_declaration ld

    method! row_field rf =
      let rf = { rf with prf_attributes = [] } in
      super#row_field rf

    method! constructor_declaration cd =
      let cd = { cd with pcd_attributes = [] } in
      super#constructor_declaration cd
  end
;;

let upper ~loc ~manifest_type ~original_kind ~params ~name =
  let open (val Syntax.builder loc) in
  type_declaration
    ~name:(Located.mk name)
    ~params
    ~cstrs:[]
    ~kind:original_kind
    ~private_:Public
    ~manifest:manifest_type
;;

let append_functor_parameter prefix = [%string "%{prefix}_subproduct"]

let generate_param_name_to_index ~core_type_params =
  List.foldi
    core_type_params
    ~init:(Map.empty (module String))
    ~f:(fun index acc { ptyp_desc; _ } ->
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
      | Ptyp_var (name, _) -> Map.set acc ~key:name ~data:index
      | _ -> acc)
;;

let original_param_to_functor_param original_name param_name_to_index =
  match Map.find param_name_to_index original_name with
  | Some index -> Ldot (Lident [%string "T%{(index + 1)#Int}"], "t")
  | None -> Ldot (Lident "T", "t")
;;

let create_mapper ~loc param_name_to_index =
  let open (val Syntax.builder loc) in
  object
    inherit Ast_traverse.map as super

    method! core_type type_ =
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree type_.ptyp_desc with
      | Ptyp_var (name, _) ->
        ptyp_constr
          (original_param_to_functor_param name param_name_to_index |> Located.mk)
          []
      | _ -> super#core_type type_
  end
;;

let generate_unique_id params =
  let existing =
    List.filter_map params ~f:(fun core_type ->
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
      | Ptyp_var (id, _) -> Some id
      | _ -> None)
    |> Set.of_list (module String)
  in
  let rec loop curr = if Set.mem existing curr then loop (curr ^ "_") else curr in
  loop "result"
;;

let or_patterns (patterns : pattern list) ~(loc : Location.t) =
  let open (val Syntax.builder loc) in
  List.reduce_exn patterns ~f:ppat_or
;;

let exclave_if exp ~loc ~local =
  match local with
  | false -> exp
  | true -> [%expr [%e exp]]
;;
