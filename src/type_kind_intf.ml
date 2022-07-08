open Base
open Ppxlib

let internal_gadt_name = "typed__t"

type granularity =
  | Shallow
  | Deep of
      { minimum_needed_parameters : (core_type * (variance * injectivity)) list
      ; minimum_needed_parameter_ids : int list
      ; original_type_with_attributes : core_type
      }

(* Attaches a number of type parameters to a type case. *)
type 'a with_parameters = 'a * (core_type * (variance * injectivity)) list

(** Type is used in order to represent the results of gen_t and its helper function.

    This is defined in this module to avoid circular dependencies between
    the generic and the more specific module which both need access to it.  *)
type 'a gen_t_result =
  { gadt_t : type_declaration
  ; upper : type_declaration
  ; constructor_declarations : (('a * granularity) * constructor_declaration) list
  ; internal_gadt_rename : type_declaration
  }

let generate_core_type_params params =
  List.map params ~f:(fun (core_type, _) -> core_type)
;;

(*Repeatedly appends '_' to start until it is not inside of 'identifiers_to_avoid'. *)
let generate_unique_name ~start ~identifiers_to_avoid =
  let rec loop curr =
    if Set.mem identifiers_to_avoid curr then loop (curr ^ "_") else curr
  in
  loop start
;;

(* Needs to be an identifier that does not collide with any other types
   from the Typed_fields_lib.S signature. *)
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
  let open (val Ast_builder.make loc) in
  let core_type_params = List.map params ~f:(fun (core_type, _) -> core_type) in
  ptyp_constr (Located.mk (Lident name)) core_type_params
;;

(* Disables unused value warning. *)
let disable_warning_32 ~loc =
  let open (val Ast_builder.make loc) in
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
  let open (val Ast_builder.make loc) in
  let creator_function_type =
    ptyp_poly
      [ Located.mk unique_parameter_id ]
      (ptyp_arrow
         Nolabel
         (ptyp_constr
            (Located.mk (Lident t_name))
            (core_type_params @ [ ptyp_var unique_parameter_id ]))
         (ptyp_var unique_parameter_id))
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
         ])
;;

(**
   Attributes need to be manually removed so that they do not reappear in the
   output of the ppx.
*)
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
  let open (val Ast_builder.make loc) in
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
      match ptyp_desc with
      | Ptyp_var name -> Map.set acc ~key:name ~data:index
      | _ -> acc)
;;

let original_param_to_functor_param original_name param_name_to_index =
  match Map.find param_name_to_index original_name with
  | Some index -> Ldot (Lident [%string "T%{(index + 1)#Int}"], "t")
  | None -> Ldot (Lident "T", "t")
;;

(* Replaces the original type parameters to the types that
   will be used by the functor.

   e.g. ('a * 'b) list * 'c -> (T1.t * T2.t) * list * T3.t
*)
let create_mapper ~loc param_name_to_index =
  let open (val Ast_builder.make loc) in
  object
    inherit Ast_traverse.map as super

    method! core_type type_ =
      match type_.ptyp_desc with
      | Ptyp_var name ->
        ptyp_constr
          (original_param_to_functor_param name param_name_to_index |> Located.mk)
          []
      | _ -> super#core_type type_
  end
;;

(* Generates a unique id by repeatedly appending "_" to "result", e.g. "result_". *)
let generate_unique_id params =
  let existing =
    List.filter_map params ~f:(fun core_type ->
      match core_type.ptyp_desc with
      | Ptyp_var id -> Some id
      | _ -> None)
    |> Set.of_list (module String)
  in
  let rec loop curr = if Set.mem existing curr then loop (curr ^ "_") else curr in
  loop "result"
;;

module type S = sig
  type t

  (** The structure items will be inserted after the type type
      definitions and before any other items.*)
  val extra_structure_items_to_insert : location -> structure_item list

  (** Generates the GADT constructors used in the type t. *)
  val constructor_declarations
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> core_type_params:core_type list
    -> ((t * granularity) * constructor_declaration) list

  (** Generates an expression containing the names of the
      names of the fields, e.g. ["name1"; "name2"]*)
  val names_list
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> expression

  (** Generates an expression containing the names of the
      names of the fields, e.g.

      match t with
      | Constr1 -> "constr1"
      | Name -> "name"
  *)
  val name_function_body : loc:location -> expression

  (** Generates an expression containing the path of the
      names of the fields, e.g.

      match t with
      | Constr1 -> ["constr1"]
      | Name subproduct -> "name" :: Name_subproduct.path subproduct
  *)
  val path_function_body
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> expression

  (** Generates an expression containing the path of the
      names of the fields, e.g.

      match t with
      | Constr1 -> [ 0 ]
      | Name subproduct -> 1 :: Name_subproduct.__ord subproduct
  *)
  val ord_function_body
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> expression

  (** Generates the body of the get function.

      match t with
      | Constr1 -> record.constr1
      | Name -> record.name
  *)
  val get_function_body
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> expression

  (** Generates the body of the set function.

      match t with
      | Constr1 -> {record with constr1 = value}
      | Name -> {record with name = value}
  *)
  val set_function_body
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> expression

  (**
     Generates create function body. For example:

     let constr1 = f Constr1 in
     let name = f Name in
     {constr1 ; name}
  *)
  val create_function_body
    :  loc:location
    -> constructor_declarations:((t * granularity) * constructor_declaration) list
    -> expression

  (**
     Generates a list of type ids definitions.

     e.g.

     [
     let (constr1 : (<type>) Type_equal.Id.t) =
     Type_equal.Id.create ~name:"constr1" Sexplib.Conv.opaque
     ; ...
     ]
  *)
  val type_ids
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> core_type_params:core_type list
    -> structure_item list

  (**
     Generates a list of modules that are used as the parameters.

     e.g.

     [
     module Name_subproduct = [%typed_fields type t = int * int]
     ; ...
     ]
  *)
  val subproduct_type_id_modules
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> core_type_params:core_type list
    -> structure_item list

  (**
     Generates body for the [type_id] function
     For example:

     match t with
     | Constr1 -> constr1
     | Name -> name
  *)
  val type_id_function_body
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> expression

  (**
     Generates the body for the all function inside of packed.

     [T Constr1 ; T Name]
  *)
  val all_body
    :  loc:location
    -> constructor_declarations:((t * granularity) * constructor_declaration) list
    -> expression

  val pack_body : loc:location -> elements_to_convert:(t * granularity) list -> expression

  (**
     Generates the body for the sexp_of_t function inside of packed.

     match t with
     | Constr1 -> Sexplib.Sexp.Atom "Constr1"
     | ...
  *)
  val sexp_of_t_body
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> expression

  (**
     Generates the body for the t_of_sexp function inside of packed.

     match t with
     | Sexplib.Sexp.Atom "Constr1" -> Constr1
     | ...
  *)
  val t_of_sexp_body
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> expression

  (**
     Generates the deep functor structure.
     e.g.

     module Deep
     (Name_subproduct : <type of name's base typed fields>)
     (Constr1: <type of constr1's typed fields>) = <module_expression>
  *)
  val deep_functor_structure
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> module_expression:module_expr
    -> module_expr

  (**
     Generates the full_depth module.
     e.g.

     [
     module Constr1_subproduct = [%typed_field ...];
     module Name_subproduct = [%typed_field ...];
     ...;
     include Deep (Constr1_subproduct) (Name_subproduct)
     ]
  *)
  val full_depth_module
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> structure_item list

  (*  Generates the structure for the sigleton modules sent to Shallow

      [
      module Singleton_for_t_1 = struct ... end;
      module Singleton_for_t_2 = struct ... end;
      ...

      ]
  *)
  val singleton_modules_structures
    :  loc:location
    -> elements_to_convert:(t * granularity) list
    -> (structure_item * label) list
end

let or_patterns (patterns : pattern list) ~(loc : Location.t) =
  let open (val Ast_builder.make loc) in
  List.reduce_exn patterns ~f:ppat_or
;;
