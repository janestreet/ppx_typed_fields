open! Base
open Ppxlib

type t = core_type

(* The structure items will be inserted after the type type
   definitions and before any other items.*)
let extra_structure_items_to_insert loc =
  [ [%stri
      let unreachable_code = function
        | (_ : _ typed__t) -> .
      ;;]
  ]
;;

let constructor_declarations ~loc:_ ~elements_to_convert:_ ~core_type_params:_ = []
let names_list ~loc ~elements_to_convert:_ = [%expr []]
let name_function_body ~loc = [%expr unreachable_code]
let path_function_body ~loc ~elements_to_convert:_ = [%expr unreachable_code]
let ord_function_body ~loc ~elements_to_convert:_ = [%expr unreachable_code]
let get_function_body ~loc ~elements_to_convert:_ = [%expr unreachable_code]

let set_function_body ~loc ~elements_to_convert:_ =
  [%expr fun t _ _ -> unreachable_code t]
;;

let create_function_body ~loc ~constructor_declarations:_ ~local:_ = [%expr ()]
let type_ids ~loc:_ ~elements_to_convert:_ ~core_type_params:_ = []
let subproduct_type_id_modules ~loc:_ ~elements_to_convert:_ ~core_type_params:_ = []
let type_id_function_body ~loc ~elements_to_convert:_ = [%expr unreachable_code]
let globalize0_function_body ~loc ~elements_to_convert:_ = [%expr unreachable_code]

let sexp_of_t_body ~loc ~elements_to_convert:_ ~stack:_ =
  [%expr
    match packed with
    | (_ : t) -> .]
;;

let all_body ~loc ~constructor_declarations:_ = [%expr []]
let pack_body ~loc ~elements_to_convert:_ ~local:_ = [%expr unreachable_code]

let globalize_packed_function_body ~loc ~elements_to_convert:_ =
  [%expr
    function
    | (_ : t) -> .]
;;

let t_of_sexp_body ~loc ~elements_to_convert:_ =
  [%expr
    Base.raise_s
      (Sexplib.Sexp.List
         [ Sexplib.Sexp.Atom "Unit has no fields, so cannot convert to field."; sexp ])]
;;

let deep_functor_structure ~loc:_ ~elements_to_convert:_ ~module_expression =
  module_expression
;;

let full_depth_module ~loc ~elements_to_convert:_ = [ [%stri include Shallow] ]
let singleton_modules_structures ~loc:_ ~elements_to_convert:_ = []
