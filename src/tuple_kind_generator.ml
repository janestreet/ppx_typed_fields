open! Base
open Ppxlib

type t = core_type

(* The structure items will be inserted after the type type
   definitions and before any other items.*)
let extra_structure_items_to_insert _ = []

let constructor_declarations ~loc ~elements_to_convert ~core_type_params =
  Product_kind_generator.constructor_declarations
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
    ~core_type_params
;;

let names_list ~loc ~elements_to_convert =
  Product_kind_generator.names_list (module Tuple_generator) ~loc ~elements_to_convert
;;

let name_function_body ~loc = Product_kind_generator.name_function_body ~loc

let path_function_body ~loc ~elements_to_convert =
  Product_kind_generator.path_function_body
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;

let ord_function_body ~loc ~elements_to_convert =
  Product_kind_generator.ord_function_body
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;

let get_function_body ~loc ~elements_to_convert =
  Product_kind_generator.get_function_body
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;

let set_function_body ~loc ~elements_to_convert =
  Product_kind_generator.set_function_body
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;

let create_function_body ~loc ~constructor_declarations ~local =
  Product_kind_generator.create_function_body
    (module Tuple_generator)
    ~loc
    ~constructor_declarations
    ~local
;;

let type_ids ~loc ~elements_to_convert ~core_type_params =
  Product_kind_generator.type_ids
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
    ~core_type_params
;;

let subproduct_type_id_modules ~loc ~elements_to_convert ~core_type_params =
  Product_kind_generator.subproduct_type_id_modules
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
    ~core_type_params
;;

let type_id_function_body ~loc ~elements_to_convert =
  Product_kind_generator.type_id_function_body
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;

let globalize0_function_body ~loc ~elements_to_convert =
  Product_kind_generator.globalize0_function_body
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;

let globalize_packed_function_body ~loc ~elements_to_convert =
  Product_kind_generator.globalize_packed_function_body
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;

let all_body ~loc ~constructor_declarations =
  Product_kind_generator.all_body (module Tuple_generator) ~loc ~constructor_declarations
;;

let pack_body ~loc ~elements_to_convert =
  Product_kind_generator.pack_body (module Tuple_generator) ~loc ~elements_to_convert
;;

let sexp_of_t_body ~loc ~elements_to_convert =
  Product_kind_generator.sexp_of_t_body (module Tuple_generator) ~loc ~elements_to_convert
;;

let t_of_sexp_body ~loc ~elements_to_convert =
  Product_kind_generator.t_of_sexp_body (module Tuple_generator) ~loc ~elements_to_convert
;;

let deep_functor_structure ~loc ~elements_to_convert ~module_expression =
  Product_kind_generator.deep_functor_structure
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
    ~module_expression
;;

let full_depth_module ~loc ~elements_to_convert =
  Product_kind_generator.full_depth_module
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;

let singleton_modules_structures ~loc ~elements_to_convert =
  Product_kind_generator.singleton_modules_structures
    (module Tuple_generator)
    ~loc
    ~elements_to_convert
;;
