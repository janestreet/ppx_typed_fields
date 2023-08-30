open Ppxlib

(* The structure items will be inserted after the type type
   definitions and before any other items.*)
let extra_structure_items_to_insert loc =
  let open (val Ast_builder.make loc) in
  [ [%stri
      let unreachable_code = function
        | (_ : _ typed__t) -> .
      ;;]
  ]
;;

let generate_constructor_declarations ~loc:_ ~elements_to_convert:_ ~core_type_params:_ =
  []
;;

let names_list ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [%expr []]
;;

let name_function_body ~loc ~elements_to_convert:_ =
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

let create_function_body ~loc ~constructor_declarations:_ =
  let open (val Ast_builder.make loc) in
  [%expr unreachable_code]
;;

let type_ids ~loc:_ ~elements_to_convert:_ ~core_type_params:_ = []

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
         [ Sexplib.Sexp.Atom "Nothing has no constructors, so cannot convert to variant."
         ; sexp
         ])]
;;

let which_function_body ~loc ~elements_to_convert:_ ~number_of_params =
  let open (val Ast_builder.make loc) in
  match number_of_params with
  | 0 ->
    [%expr
      function
      | (_ : derived_on) -> .]
  | _ ->
    [%expr
      function
      | (_ : _ derived_on) -> .]
;;

let deep_functor_signature ~loc ~elements_to_convert:_ ~base_module_type =
  let open (val Ast_builder.make loc) in
  psig_module
    (module_declaration ~name:(Some "Deep" |> Located.mk) ~type_:base_module_type)
;;

let deep_functor_structure ~loc ~elements_to_convert:_ ~module_expression =
  let open (val Ast_builder.make loc) in
  pstr_module (module_binding ~name:(Some "Deep" |> Located.mk) ~expr:module_expression)
;;

(**
   Generates the full depth module of a structure, e.g.
   [
   module Constr1_subproduct = [%typed_field ...];
   module Name_subproduct = [%typed_field ...];
   ...;
   include Deep (Constr1_subproduct) (Name_subproduct)
   ]
*)
let full_depth_module ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [ [%stri include Deep] ]
;;

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
let full_depth_signature ~loc ~elements_to_convert:_ =
  let open (val Ast_builder.make loc) in
  [ [%sigi: include module type of Deep] ]
;;

(*  Generates the signature for the singleton modules sent to Shallow

    [
    module Singleton_for_t_1 : sig ... end;
    module Singleton_for_t_2 : sig ... end;
    ...

    ]
*)
let singleton_modules_signatures ~loc:_ ~elements_to_convert:_ = []

(*  Generates the structure for the sigleton modules sent to Shallow

    [
    module Singleton_for_t_1 = struct ... end;
    module Singleton_for_t_2 = struct ... end;
    ...

    ]
*)
let singleton_modules_structures ~loc:_ ~elements_to_convert:_ = []
