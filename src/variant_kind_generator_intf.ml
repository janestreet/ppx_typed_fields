open Base
open Type_kind_intf
open Ppxlib

type variant_granularity =
  | Shallow
  (* When the subvariant is another type that derives typed variants. *)
  | Constr_deep of
      { ident : longident_loc
      ; params : core_type list
      }
  | Polymorphic_deep

type supported_constructor_declaration =
  (* | C of {a : int} *)
  | Anonymous_record_constructor of
      { constructor_name : string
      ; return_value_type : core_type
      ; return_value_type_with_original_attributes : core_type
      ; minimum_needed_parameters : (core_type * (variance * injectivity)) list
      ; label_declarations : label_declaration list
      ; typed_fields : bool
      }
  (* | C of (int * int * int) *)
  | Single_value_constructor of
      { constructor_name : string
      ; return_value_type : core_type
      ; return_value_type_with_original_attributes : core_type
      ; minimum_needed_parameters : (core_type * (variance * injectivity)) list
      ; minimum_needed_parameter_ids : int list
      ; granularity : variant_granularity
      ; typed_fields : bool
      ; is_polymorphic : bool
      }
  (* | C of int * int * int *)
  | Tuple_values_constructor of
      { constructor_name : string
      ; return_value_type : core_type
      ; return_value_type_with_original_attributes : core_type
      ; minimum_needed_parameters : (core_type * (variance * injectivity)) list
      ; tuple_types : core_type list
      ; typed_fields : bool
      }
  (* | C *)
  | No_values_constructor of
      { constructor_name : string
      ; return_value_type : core_type
      ; is_polymorphic : bool
      }

let append_functor_parameter original_name = original_name ^ "_subvariant"

type type_case =
  | Variant of supported_constructor_declaration list with_parameters
  | Nothing of unit with_parameters
  | Opaque of bool with_parameters
  | Unknown

let supported_constructor_name = function
  | Anonymous_record_constructor { constructor_name; _ }
  | No_values_constructor { constructor_name; _ }
  | Single_value_constructor { constructor_name; _ }
  | Tuple_values_constructor { constructor_name; _ } -> constructor_name
;;

let supported_constructor_type = function
  | Anonymous_record_constructor { return_value_type; _ }
  | Tuple_values_constructor { return_value_type; _ }
  | No_values_constructor { return_value_type; _ }
  | Single_value_constructor { return_value_type; _ } -> return_value_type
;;

let strip_depth_from_supported_declaration declaration =
  match declaration with
  | Anonymous_record_constructor _ | No_values_constructor _ | Tuple_values_constructor _
    -> declaration
  | Single_value_constructor contents ->
    Single_value_constructor { contents with granularity = Shallow }
;;

let strip_depth_from_td_case td_case =
  match td_case with
  | Nothing _ | Opaque _ | Unknown -> td_case
  | Variant (declarations, params) ->
    Variant (List.map declarations ~f:strip_depth_from_supported_declaration, params)
;;

let at_least_one_subvariant constructor_declarations =
  List.exists constructor_declarations ~f:(fun cd ->
    match cd with
    | Single_value_constructor { granularity = Constr_deep _; _ }
    | Single_value_constructor { granularity = Polymorphic_deep; _ } -> true
    | _ -> false)
;;

module type S = sig
  (** The structure items will be inserted after the type type
      definitions and before any other items.*)
  val extra_structure_items_to_insert : location -> structure_item list

  (** Generates an expression containing the names of the
      names of the fields, e.g. ["name1"; "name2"]*)
  val names_list
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (** Generates an expression containing the names of the
      names of the fields, e.g.

      match t with
      | Constr1 -> "constr1"
      | Name -> "name"
  *)
  val name_function_body
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (** Generates an expression containing the path of the
      names of the fields, e.g.

      match t with
      | Constr1 -> ["constr1"]
      | Name subproduct -> "name" :: Name_subproduct.path subproduct
  *)
  val path_function_body
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (** Generates an expression containing the path of the
      names of the fields, e.g.

      match t with
      | Constr1 -> [0]
      | Name subproduct -> 1 :: Name_subproduct.__ord subproduct
  *)
  val ord_function_body
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (** Generates the body of the get function.

      match t with
      | Constr1 -> record.constr1
      | Name -> record.name
  *)
  val get_function_body
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (**
     Generates create function body. For example:

     let constr1 = f Constr1 in
     let name = f Name in
     {constr1 ; name}
  *)
  val create_function_body
    :  loc:location
    -> constructor_declarations:
         ((supported_constructor_declaration * granularity) * constructor_declaration)
           list
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
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
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
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (**
     Generates the body for the all function inside of packed.

     [T Constr1 ; T Name]
  *)
  val all_body
    :  loc:location
    -> constructor_declarations:
         ((supported_constructor_declaration * granularity) * constructor_declaration)
           list
    -> expression

  val pack_body
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (**
     Generates the body for the sexp_of_t function inside of packed.

     match t with
     | Constr1 -> Sexplib.Sexp.Atom "Constr1"
     | ...
  *)
  val sexp_of_t_body
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (**
     Generates the body for the t_of_sexp function inside of packed.

     match t with
     | Sexplib.Sexp.Atom "Constr1" -> Constr1
     | ...
  *)
  val t_of_sexp_body
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> expression

  (** Generates the body of the get function.

      match t with
      | Constr1 -> {f = T Constr1}
      | Name -> {f = T Name}
  *)
  val which_function_body
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> number_of_params:int
    -> expression

  (* Generates top level type definitions
     `type record = t` and
     `type _ t = A : a |  B : b ...`  *)
  val generate_constructor_declarations
    :  loc:location
    -> elements_to_convert:(supported_constructor_declaration * granularity) list
    -> core_type_params:core_type list
    -> ((supported_constructor_declaration * granularity) * constructor_declaration) list

  (**
     Generates the deep functor structure.
     e.g.

     module Deep
     (Name_subproduct : <type of name's base typed fields>)
     (Constr1: <type of constr1's typed fields>) = <module_expression>
  *)
  val deep_functor_structure
    :  loc:location
    -> elements_to_convert:supported_constructor_declaration list
    -> module_expression:module_expr
    -> structure_item

  (**
     Generates the deep functor structure.
     e.g.

     module Deep
     (Name_subproduct : <type of name's base typed fields>)
     (Constr1: <type of constr1's typed fields>) = <module_expression>
  *)
  val deep_functor_signature
    :  loc:location
    -> elements_to_convert:supported_constructor_declaration list
    -> base_module_type:module_type
    -> signature_item

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
    -> elements_to_convert:supported_constructor_declaration list
    -> structure_item list

  (**
     Generates the full_depth module's signature.
     e.g.

     [
     module Constr1_subproduct : module type of [%typed_field ...];
     module Name_subproduct : module type of [%typed_field ...];
     ...;
     include module type of Deep
     (Constr1_subproduct)
     (Name_subproduct)
     ]
  *)
  val full_depth_signature
    :  loc:location
    -> elements_to_convert:supported_constructor_declaration list
    -> signature_item list

  (*  Generates the signature for the singleton modules sent to Shallow

      [
      module Singleton_for_t_1 : sig ... end;
      module Singleton_for_t_2 : sig ... end;
      ...

      ]
  *)
  val singleton_modules_signatures
    :  loc:location
    -> elements_to_convert:supported_constructor_declaration list
    -> signature_item list

  (*  Generates the structure for the sigleton modules sent to Shallow

      [
      module Singleton_for_t_1 = struct ... end;
      module Singleton_for_t_2 = struct ... end;
      ...

      ]
  *)
  val singleton_modules_structures
    :  loc:location
    -> elements_to_convert:supported_constructor_declaration list
    -> structure_item list
end
