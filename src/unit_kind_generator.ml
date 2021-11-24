open Ppxlib

type t = core_type

(* The structure items will be inserted after the type type
   definitions and before any other items.*)
let extra_structure_items_to_insert loc =
  let open (val Ast_builder.make loc) in
  [ [%stri
    let unreachable_code = function
      | (_ : _ t) -> .
    ;;]
  ]
;;

let constructor_declarations ~loc:_ ~elements_to_convert:_ ~core_type_params:_ = []

let names_list ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr []]
;;

let name_function_body ~loc =
  let open (val Ast_builder.make loc) in
  [%expr unreachable_code]
;;

let path_function_body ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr unreachable_code]
;;

let ord_function_body ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr unreachable_code]
;;

let get_function_body ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr unreachable_code]
;;

let set_function_body ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr fun t _ _ -> unreachable_code t]
;;

let create_function_body ~loc ~constructor_declarations:_ =
  let open (val Ast_builder.make loc) in
  [%expr ()]
;;

let type_ids ~loc:_ ~elements_to_convert:_ ~core_type_params:_ = []
let subproduct_type_id_modules ~loc:_ ~elements_to_convert:_ ~core_type_params:_ = []

let type_id_function_body ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr unreachable_code]
;;

let sexp_of_t_body ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr
    match packed with
    | (_ : t) -> .]
;;

let all_body ~loc ~constructor_declarations:_ =
  let open (val Ast_builder.make loc) in
  [%expr []]
;;

let pack_body ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr unreachable_code]
;;

let t_of_sexp_body ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr
    Base.raise_s
      (Sexplib.Sexp.List
         [ Sexplib.Sexp.Atom "Unit has no fields, so cannot convert to field."; sexp ])]
;;

let deep_functor_structure ~loc:_ ~elements_to_convert:_ ~module_expression =
  module_expression
;;

let full_depth_module ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [ [%stri include Shallow] ]
;;

let singleton_modules_structures ~loc:_ ~elements_to_convert:_ = []
