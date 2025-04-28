open! Base
open Ppxlib

module Definitions = struct
  type granularity =
    | Shallow
    | Deep of
        { minimum_needed_parameters : (core_type * (variance * injectivity)) list
        ; minimum_needed_parameter_ids : int list
        ; original_type_with_attributes : core_type
        }

  (** Attaches a number of type parameters to a type case. *)
  type 'a with_parameters = 'a * (core_type * (variance * injectivity)) list

  (** Type is used in order to represent the results of gen_t and its helper function.

      This is defined in this module to avoid circular dependencies between the generic
      and the more specific module which both need access to it. *)
  type 'a gen_t_result =
    { gadt_t : type_declaration
    ; upper : type_declaration
    ; constructor_declarations : (('a * granularity) * constructor_declaration) list
    ; internal_gadt_rename : type_declaration
    }

  module type S = sig
    type t

    (** The structure items will be inserted after the type type definitions and before
        any other items. *)
    val extra_structure_items_to_insert : location -> structure_item list

    (** Generates the GADT constructors used in the type t. *)
    val constructor_declarations
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> core_type_params:core_type list
      -> ((t * granularity) * constructor_declaration) list

    (** Generates an expression containing the names of the names of the fields, e.g.
        ["name1"; "name2"] *)
    val names_list
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates an expression containing the names of the names of the fields, e.g.

        {[
          match t with
          | Constr1 -> "constr1"
          | Name -> "name"
        ]} *)
    val name_function_body : loc:location -> expression

    (** Generates an expression containing the path of the names of the fields, e.g.

        {[
          match t with
          | Constr1 -> [ "constr1" ]
          | Name subproduct -> "name" :: Name_subproduct.path subproduct
        ]} *)
    val path_function_body
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates an expression containing the path of the names of the fields, e.g.

        {[
          match t with
          | Constr1 -> [ 0 ]
          | Name subproduct -> 1 :: Name_subproduct.__ord subproduct
        ]} *)
    val ord_function_body
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates the body of the get function.

        {[
          match t with
          | Constr1 -> record.constr1
          | Name -> record.name
        ]} *)
    val get_function_body
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates the body of the set function.

        {[
          match t with
          | Constr1 -> { record with constr1 = value }
          | Name -> { record with name = value }
        ]} *)
    val set_function_body
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates create function body. For example:

        {[
          let constr1 = f Constr1 in
          let name = f Name in
          { constr1; name }
        ]} *)
    val create_function_body
      :  loc:location
      -> constructor_declarations:((t * granularity) * constructor_declaration) list
      -> local:bool
      -> expression

    (** Generates a list of type ids definitions.

        e.g.

        {[
          let (constr1 : (<type>) Type_equal.Id.t) =
            Type_equal.Id.create ~name:"constr1" Sexplib.Conv.opaque
          ;;

          ...
        ]} *)
    val type_ids
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> core_type_params:core_type list
      -> structure_item list

    (** Generates a list of modules that are used as the parameters.

        e.g.

        {[
          module Name_subproduct = [%typed_fields type t = int * int]
          ...
        ]} *)
    val subproduct_type_id_modules
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> core_type_params:core_type list
      -> structure_item list

    (** Generates body for the [type_id] function For example:

        {[
          match t with
          | Constr1 -> constr1
          | Name -> name
        ]} *)
    val type_id_function_body
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates the body for the all function inside of packed.

        [T Constr1 ; T Name] *)
    val all_body
      :  loc:location
      -> constructor_declarations:((t * granularity) * constructor_declaration) list
      -> expression

    val pack_body
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates the body for the sexp_of_t function inside of packed.

        {[
          match t with
          | Constr1 -> Sexplib.Sexp.Atom "Constr1"
          | ...
        ]} *)
    val sexp_of_t_body
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates the body for the t_of_sexp function inside of packed.

        {[
          match t with
          | Sexplib.Sexp.Atom "Constr1" -> Constr1
          | ...
        ]} *)
    val t_of_sexp_body
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> expression

    (** Generates the deep functor structure. e.g.

        {[
          module Deep
              (Name_subproduct : <type of name's base typed fields>)
              (Constr1: <type of constr1's typed fields>) = <module_expression>
        ]} *)
    val deep_functor_structure
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> module_expression:module_expr
      -> module_expr

    (** Generates the full_depth module. e.g.

        {[
          module Constr1_subproduct = [%typed_field ...]
          module Name_subproduct = [%typed_field ...]
          ...
          include Deep (Constr1_subproduct) (Name_subproduct)
        ]} *)
    val full_depth_module
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> structure_item list

    (** Generates the structure for the sigleton modules sent to Shallow

        {[
          module Singleton_for_t_1 = struct ... end;
          module Singleton_for_t_2 = struct ... end;
          ...
        ]} *)
    val singleton_modules_structures
      :  loc:location
      -> elements_to_convert:(t * granularity) list
      -> (structure_item * label) list
  end
end

module type Type_kind = sig
  include module type of struct
    include Definitions
  end

  val internal_gadt_name : string

  val generate_core_type_params
    :  (core_type * (variance * injectivity)) list
    -> core_type list

  (** Repeatedly appends '_' to start until it is not inside of 'identifiers_to_avoid'. *)
  val generate_unique_name
    :  start:string
    -> identifiers_to_avoid:Set.M(String).t
    -> string

  (** Needs to be an identifier that does not collide with any other types from the
      Typed_fields_lib.S signature. *)
  val generate_local_type_name : type_declaration -> string

  val generate_manifest_type_constr
    :  loc:Location.t
    -> name:string
    -> params:(core_type * (variance * injectivity)) list
    -> core_type

  (** Disables unused value warning. *)
  val disable_warning_32 : loc:Location.t -> attribute

  val generate_creator_type_declaration
    :  loc:Location.t
    -> unique_parameter_id:string
    -> core_type_params:core_type list
    -> params:(core_type * (variance * injectivity)) list
    -> t_name:string
    -> type_declaration

  (** Attributes need to be manually removed so that they do not reappear in the output of
      the ppx. *)
  val attribute_remover : Ast_traverse.map

  val upper
    :  loc:Location.t
    -> manifest_type:core_type option
    -> original_kind:type_kind
    -> params:(core_type * (variance * injectivity)) list
    -> name:string
    -> type_declaration

  val append_functor_parameter : string -> string

  val generate_param_name_to_index
    :  core_type_params:core_type list
    -> int Map.M(String).t

  val original_param_to_functor_param : string -> int Map.M(String).t -> Longident.t

  (** Replaces the original type parameters to the types that will be used by the functor.

      e.g. ('a * 'b) list * 'c -> (T1.t * T2.t) * list * T3.t *)
  val create_mapper : loc:Location.t -> int Map.M(String).t -> Ast_traverse.map

  (** Generates a unique id by repeatedly appending "_" to "result", e.g. "result_". *)
  val generate_unique_id : core_type list -> string

  val or_patterns : pattern list -> loc:Location.t -> pattern
end
