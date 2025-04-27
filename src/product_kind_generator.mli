open! Base
open Ppxlib

type t

(** Generates the GADT constructors used in the type t. *)
val constructor_declarations
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> core_type_params:core_type list
  -> (('a * Type_kind.granularity) * constructor_declaration) list

(** Generates an expression containing the names of the names of the fields, e.g.
    ["name1"; "name2"] *)
val names_list
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** {v
 Generates an expression containing the names of the
    names of the fields, e.g.

    match t with
    | Constr1 -> "constr1"
    | Name -> "name"
    v} *)
val name_function_body : loc:location -> expression

(** {v
 Generates an expression containing the path of the
    names of the fields, e.g.

    match t with
    | Constr1 -> ["constr1"]
    | Name subproduct -> "name" :: Name_subproduct.path subproduct
    v} *)
val path_function_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** {v
 Generates an expression containing the order of the
    names of the fields, e.g.

    match t with
    | Constr1 -> [0]
    | Name subproduct -> 1:: Name_subproduct.__ord subproduct
    v} *)
val ord_function_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** {v
 Generates the body of the get function.

    match t with
    | Constr1 -> record.constr1
    | Name -> record.name
    v} *)
val get_function_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** {v
 Generates the body of the set function.

    match t with
    | Constr1 -> {record with constr1 = value}
    | Name -> {record with name = value}
    v} *)
val set_function_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** Generates create function body. For example:

    let constr1 = f Constr1 in let name = f Name in [{constr1 ; name}] *)
val create_function_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> constructor_declarations:
       (('a * Type_kind.granularity) * constructor_declaration) list
  -> local:bool
  -> expression

(** Generates a list of modules that are used as the parameters.

    e.g.

    [ module Name_subproduct = [%typed_fields type t = int * int] ; ... ] *)
val subproduct_type_id_modules
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> core_type_params:core_type list
  -> structure_item list

(** Generates a list of type ids definitions.

    e.g.

    [ let (constr1 : (<type>) Type_equal.Id.t) = Type_equal.Id.create ~name:"constr1" Sexplib.Conv.opaque ; ... ] *)
val type_ids
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> core_type_params:core_type list
  -> structure_item list

(** {v
   Generates body for the [type_id] function
   For example:

   match t with
   | Constr1 -> constr1
   | Name -> name
    v} *)
val type_id_function_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** Generates the body for the all function inside of packed.

    [T Constr1 ; T Name] *)
val all_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> constructor_declarations:
       (('a * Type_kind.granularity) * constructor_declaration) list
  -> expression

(** {v
   Generates the pack function body. (e.g.):

   match t with
   | Name -> {f = T Name}
    v} *)
val pack_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** {v
   Generates the body for the sexp_of_t function inside of packed.

   match t with
   | Constr1 -> Sexplib.Sexp.Atom "Constr1"
   | ...
    v} *)
val sexp_of_t_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** {v
   Generates the body for the t_of_sexp function inside of packed.

   match t with
   | Sexplib.Sexp.Atom "Constr1" -> Constr1
   | ...
    v} *)
val t_of_sexp_body
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> expression

(** Generates the deep functor signature. e.g.

    module Deep (Name_subproduct : <type of name's base typed fields>) (Constr1: <type of
    constr1's typed fields>) = <base_module_type> *)
val deep_functor_signature
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> base_module_type:module_type
  -> signature_item

(** Generates the deep functor structure. e.g.

    module Deep (Name_subproduct : <type of name's base typed fields>) (Constr1: <type of
    constr1's typed fields>) = <module_expression> *)
val deep_functor_structure
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> module_expression:module_expr
  -> module_expr

(** Generates the full_depth module. e.g.

    [ module Constr1_subproduct = [%typed_field ...]; module Name_subproduct = [%typed_field ...]; ...; include Deep (Constr1_subproduct) (Name_subproduct) ] *)
val full_depth_module
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> structure_item list

(** Generates the full_depth module's signature. e.g.

    [ module Constr1_subproduct : module type of [%typed_field ...]; module Name_subproduct : module type of [%typed_field ...]; ...; include module type of Deep (Constr1_subproduct) (Name_subproduct) ] *)
val full_depth_signature
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> signature_item list

(*  Generates the signature for the sigleton modules sent to Shallow

    [
    module Singleton_for_t_1 : sig ... end;
    module Singleton_for_t_2 : sig ... end;
    ...

    ]
*)
val singleton_modules_signatures
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> (signature_item * label) list

(*  Generates the structure for the sigleton modules sent to Shallow

    [
    module Singleton_for_t_1 = struct ... end;
    module Singleton_for_t_2 = struct ... end;
    ...

    ]
*)
val singleton_modules_structures
  :  (module Product_kind.S with type t = 'a)
  -> loc:location
  -> elements_to_convert:('a * Type_kind.granularity) list
  -> (structure_item * label) list
