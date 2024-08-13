open Base
open Import
open Ppxlib
open Type_kind_intf

type t

(** Generates the GADT constructors used in the type t. *)
let constructor_declarations
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~(elements_to_convert : (a * Type_kind_intf.granularity) list)
  ~core_type_params
  =
  let open (val Ast_builder.make loc) in
  let unique_parameter_name = Type_kind_intf.generate_unique_id core_type_params in
  List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
    let args =
      match granularity with
      | Shallow -> Pcstr_tuple []
      | Deep { minimum_needed_parameters; _ } ->
        let type_ = Specific_implementation.to_type element in
        let pararameters_needed =
          match type_.ptyp_desc with
          | Ptyp_constr (_, params) -> params
          | _ -> Type_kind_intf.generate_core_type_params minimum_needed_parameters
        in
        let subproduct_module_name =
          Specific_implementation.name index element
          |> String.capitalize
          |> Type_kind_intf.append_functor_parameter
        in
        Pcstr_tuple
          [ ptyp_constr
              (Ldot (Lident subproduct_module_name, "t") |> Located.mk)
              (pararameters_needed @ [ ptyp_var unique_parameter_name ])
            |> Ppxlib_jane.Shim.Pcstr_tuple_arg.of_core_type
          ]
    in
    ( (element, granularity)
    , constructor_declaration
        ~name:
          (element
           |> Specific_implementation.name index
           |> String.capitalize
           |> Located.mk)
        ~args
        ~res:
          (Some
             (ptyp_constr
                (Located.mk (Lident internal_gadt_name))
                (core_type_params
                 @ [ (match granularity with
                      | Shallow -> Specific_implementation.to_type element
                      | Deep _ -> ptyp_var unique_parameter_name)
                   ]))) ))
;;

(** Generates an expression containing the names of the
    names of the fields, e.g. ["name1"; "name2"]*)
let names_list
  (type a)
  (module _ : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert:_
  =
  let open (val Ast_builder.make loc) in
  [%expr Base.List.map Packed.all ~f:(fun { f = T f } -> name f)]
;;

(**
   Generates payload for a constructor that might be a subproduct.

   If it is not a subproduct, then it returns None.
*)
let generate_contructor_payload ~loc = function
  | Type_kind_intf.Shallow -> None
  | Type_kind_intf.Deep _ ->
    let open (val Ast_builder.make loc) in
    Some (ppat_var (Located.mk "subproduct"))
;;

(**
   Returns a string with the name of a subproduct.

   (e.g.  "Name_subproduct")
*)
let generate_subproduct_module_name
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  index
  element
  =
  Specific_implementation.name index element
  |> String.capitalize
  |> Type_kind_intf.append_functor_parameter
;;

(**
   Generates a subproduct function for a given parent module.

   (e.g.  "Name_subproduct.path")
*)
let generate_subproduct_function
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~index
  ~element
  ~name
  =
  let open (val Ast_builder.make loc) in
  let subproduct_module_name =
    generate_subproduct_module_name (module Specific_implementation) index element
  in
  pexp_ident (Ldot (Lident subproduct_module_name, name) |> Located.mk)
;;

(** Generates an expression containing the names of the
    names of the fields, e.g.

    match t with
    | Constr1 -> "constr1"
    | Name -> "name"
*)
let name_function_body ~loc = [%expr fun x -> Base.List.last_exn (path x)]

(** Generates an expression containing the path of the
    names of the fields, e.g.

    match t with
    | Constr1 -> ["constr1"]
    | Name subproduct -> "name" :: Name_subproduct.path subproduct
*)
let path_function_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let patterns =
    List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
      let variant_name =
        Specific_implementation.name index element |> String.capitalize
      in
      let pattern =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_contructor_payload ~loc granularity)
      in
      let current_root_path =
        let current_string_expression =
          Specific_implementation.name index element |> estring
        in
        match granularity with
        | Type_kind_intf.Shallow -> [%expr [ [%e current_string_expression] ]]
        | Type_kind_intf.Deep _ ->
          let subproduct_function =
            generate_subproduct_function
              (module Specific_implementation)
              ~loc
              ~index
              ~element
              ~name:"path"
          in
          [%expr [%e current_string_expression] :: [%e subproduct_function] subproduct]
      in
      case ~lhs:pattern ~guard:None ~rhs:current_root_path)
  in
  let match_statement = pexp_match (pexp_ident (Located.mk (Lident "x"))) patterns in
  [%expr fun x -> [%e match_statement]]
;;

(** Generates an expression containing the ord of the
    names of the fields, e.g.

    match t with
    | Constr1 -> [0]
    | Name subproduct -> 1 :: Name_subproduct.__ord subproduct
*)
let ord_function_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let patterns =
    List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
      let variant_name =
        Specific_implementation.name index element |> String.capitalize
      in
      let pattern =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_contructor_payload ~loc granularity)
      in
      let current_root_path =
        let current_ord_expression = eint index in
        match granularity with
        | Type_kind_intf.Shallow -> [%expr [ [%e current_ord_expression] ]]
        | Type_kind_intf.Deep _ ->
          let subproduct_function =
            generate_subproduct_function
              (module Specific_implementation)
              ~loc
              ~index
              ~element
              ~name:"__ord"
          in
          [%expr [%e current_ord_expression] :: [%e subproduct_function] subproduct]
      in
      case ~lhs:pattern ~guard:None ~rhs:current_root_path)
  in
  let match_statement = pexp_match (pexp_ident (Located.mk (Lident "x"))) patterns in
  [%expr fun x -> [%e match_statement]]
;;

(** Generates the body of the get function.

    match t with
    | Constr1 -> record.constr1
    | Name -> record.name
*)
let get_function_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let cases =
    List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
      let variant_name =
        Specific_implementation.name index element |> String.capitalize
      in
      let pattern =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_contructor_payload ~loc granularity)
      in
      let rhs =
        match granularity with
        | Type_kind_intf.Shallow ->
          Specific_implementation.get_rhs_expression
            ~loc
            ~index
            ~element
            ~number_of_elements:(List.length elements_to_convert)
        | Type_kind_intf.Deep _ ->
          let subproduct_function =
            generate_subproduct_function
              (module Specific_implementation)
              ~loc
              ~index
              ~element
              ~name:"get"
          in
          [%expr
            [%e
              Specific_implementation.get_rhs_expression
                ~loc
                ~index
                ~element
                ~number_of_elements:(List.length elements_to_convert)]
            |> [%e subproduct_function] subproduct]
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  [%expr fun t record -> [%e pexp_match (pexp_ident (Located.mk (Lident "t"))) cases]]
;;

(** Generates the body of the set function.

    match t with
    | Constr1 -> {record with constr1 = value}
    | Name -> {record with name = value}
*)
let set_function_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let cases =
    List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
      let variant_name =
        Specific_implementation.name index element |> String.capitalize
      in
      let pattern =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_contructor_payload ~loc granularity)
      in
      let rhs =
        match granularity with
        | Type_kind_intf.Shallow ->
          Specific_implementation.set_rhs_expression
            ~loc
            ~index
            ~element
            ~number_of_elements:(List.length elements_to_convert)
            ~expression_to_set:[%expr value]
        | Type_kind_intf.Deep _ ->
          let subproduct_function_expression =
            generate_subproduct_function
              (module Specific_implementation)
              ~loc
              ~index
              ~element
              ~name:"set"
          in
          let expression_to_set =
            [%expr
              [%e subproduct_function_expression]
                subproduct
                [%e
                  Specific_implementation.get_rhs_expression
                    ~loc
                    ~index
                    ~element
                    ~number_of_elements:(List.length elements_to_convert)]
                value]
          in
          Specific_implementation.set_rhs_expression
            ~loc
            ~index
            ~element
            ~number_of_elements:(List.length elements_to_convert)
            ~expression_to_set
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  [%expr
    fun t record value -> [%e pexp_match (pexp_ident (Located.mk (Lident "t"))) cases]]
;;

(**
   Generates create function body. For example:

   let constr1 = f Constr1 in
   let name = f Name in
   {constr1 ; name}
*)
let create_function_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~constructor_declarations
  ~local
  =
  Specific_implementation.create_expression ~loc ~constructor_declarations ~local
;;

(**
   Generates a list of modules that are used as the parameters.

   e.g.

   [
   module Name_subproduct = [%typed_fields type t = int * int]
   ; ...
   ]
*)
let subproduct_type_id_modules
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  ~core_type_params
  =
  let open (val Ast_builder.make loc) in
  List.filter_mapi elements_to_convert ~f:(fun index (element, granularity) ->
    match granularity with
    | Type_kind_intf.Shallow -> None
    | Type_kind_intf.Deep { minimum_needed_parameter_ids; _ } ->
      Some
        (let subproduct_module_name =
           generate_subproduct_module_name (module Specific_implementation) index element
         in
         let module_name = [%string "%{subproduct_module_name}_type_ids"] in
         let type_ = Specific_implementation.to_type element in
         let initial_ident =
           pmod_ident (Ldot (Lident subproduct_module_name, "Type_ids") |> Located.mk)
         in
         let module_expr_with_functor_application =
           match type_.ptyp_desc with
           | Ptyp_constr (_, params) ->
             let param_name_to_index = generate_param_name_to_index ~core_type_params in
             let mapper = create_mapper ~loc param_name_to_index in
             List.fold params ~init:initial_ident ~f:(fun acc param ->
               let functor_parameter =
                 pmod_structure
                   [ pstr_type
                       Recursive
                       [ type_declaration
                           ~name:(Located.mk "t")
                           ~params:[]
                           ~cstrs:[]
                           ~kind:Ptype_abstract
                           ~private_:Public
                           ~manifest:(Some (mapper#core_type param))
                       ]
                   ]
               in
               pmod_apply acc functor_parameter)
           | _ ->
             List.fold
               (List.sort minimum_needed_parameter_ids ~compare:Int.compare)
               ~init:initial_ident
               ~f:(fun acc id ->
                 pmod_apply
                   acc
                   (pmod_ident (Lident [%string "T%{(id + 1)#Int}"] |> Located.mk)))
         in
         pstr_module
           (module_binding
              ~name:(Some module_name |> Located.mk)
              ~expr:module_expr_with_functor_application)))
;;

(**
   Generates a list of type ids definitions.

   e.g.

   [
   let (constr1 : (<type>) Type_equal.Id.t) =
   Type_equal.Id.create ~name:"constr1" Sexplib.Conv.opaque
   ; ...
   ]
*)
let type_ids
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  ~core_type_params
  =
  let open (val Ast_builder.make loc) in
  let param_name_to_index = generate_param_name_to_index ~core_type_params in
  let mapper = create_mapper ~loc param_name_to_index in
  List.filter_mapi elements_to_convert ~f:(fun index (element, granularity) ->
    match granularity with
    | Type_kind_intf.Deep _ -> None
    | Type_kind_intf.Shallow ->
      Some
        [%stri
          let ([%p pvar (Specific_implementation.name index element)] :
                [%t mapper#core_type (Specific_implementation.to_type element)]
                  Base.Type_equal.Id.t)
            =
            Base.Type_equal.Id.create
              ~name:[%e estring (Specific_implementation.name index element)]
              Sexplib.Conv.sexp_of_opaque
          ;;])
;;

(**
   Generates body for the [type_id] function
   For example:

   match t with
   | Constr1 -> constr1
   | Name -> name
*)
let type_id_function_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let cases =
    List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
      let variant_name =
        Specific_implementation.name index element |> String.capitalize
      in
      let pattern =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_contructor_payload ~loc granularity)
      in
      let type_id = Located.mk (Lident (Specific_implementation.name index element)) in
      let rhs =
        match granularity with
        | Type_kind_intf.Shallow -> pexp_ident type_id
        | Type_kind_intf.Deep _ ->
          let subproduct_module_name =
            generate_subproduct_module_name (module Specific_implementation) index element
          in
          let subproduct_module_typed_field_name =
            [%string "%{subproduct_module_name}_type_ids"]
          in
          let subproduct_type_id_function_expression =
            pexp_ident
              (Ldot (Lident subproduct_module_typed_field_name, "type_id") |> Located.mk)
          in
          [%expr [%e subproduct_type_id_function_expression] subproduct]
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  pexp_function cases
;;

(**
   Generates the body for the all function inside of packed.

   [T Constr1 ; T Name]
*)
let all_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~constructor_declarations
  =
  let open (val Ast_builder.make loc) in
  let packed_fields =
    List.mapi
      constructor_declarations
      ~f:(fun index ((element, granularity), constructor) ->
        match granularity with
        | Type_kind_intf.Shallow -> [%expr [ { f = T [%e econstruct constructor None] } ]]
        | Type_kind_intf.Deep _ ->
          let subproduct_module_name =
            generate_subproduct_module_name (module Specific_implementation) index element
          in
          let constructor_expression =
            pexp_construct
              (Lident (Specific_implementation.name index element |> String.capitalize)
               |> Located.mk)
              (Some (Lident "subproduct" |> Located.mk |> pexp_ident))
          in
          let subproduct_packed_all =
            pexp_ident
              (Ldot (Ldot (Lident subproduct_module_name, "Packed"), "all") |> Located.mk)
          in
          [%expr
            Base.List.map [%e subproduct_packed_all] ~f:(fun { f = subproduct } ->
              { f =
                  (let (T subproduct) = subproduct in
                   T [%e constructor_expression])
              })])
  in
  [%expr Base.List.concat [%e elist packed_fields]]
;;

let wrap_t_struct_around_expression ~loc expression =
  let open (val Ast_builder.make loc) in
  pexp_record [ Lident "f" |> Located.mk, expression ] None
;;

let pack_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let cases =
    List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
      let constructor_name =
        Specific_implementation.name index element |> String.capitalize
      in
      let lhs =
        ppat_construct
          (Lident constructor_name |> Located.mk)
          (generate_contructor_payload ~loc granularity)
      in
      let rhs =
        let bottom_constructor_with_record =
          let inner_constructor =
            match granularity with
            | Shallow -> None
            | Deep _ -> Some [%expr subproduct]
          in
          pexp_construct
            (Lident "T" |> Located.mk)
            (Some
               (pexp_construct (Lident constructor_name |> Located.mk) inner_constructor))
        in
        match granularity with
        | Shallow -> wrap_t_struct_around_expression ~loc bottom_constructor_with_record
        | Deep _ ->
          let parameter_module_name =
            generate_subproduct_module_name (module Specific_implementation) index element
          in
          let pack_function_ident =
            pexp_ident
              (Ldot (Ldot (Lident parameter_module_name, "Packed"), "pack") |> Located.mk)
          in
          [%expr
            let subproduct = [%e pack_function_ident] subproduct in
            { f =
                (let { f = T subproduct } = subproduct in
                 [%e bottom_constructor_with_record])
            }]
      in
      case ~lhs ~guard:None ~rhs)
  in
  pexp_function cases
;;

(**
   Generates the body for the sexp_of_t function inside of packed.

   match t with
   | Constr1 -> Sexplib.Sexp.Atom "Constr1"
   | ...
*)
let sexp_of_t_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let cases =
    List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
      let variant_name =
        Specific_implementation.name index element |> String.capitalize
      in
      let constructor_option =
        match granularity with
        | Type_kind_intf.Shallow -> None
        | Type_kind_intf.Deep _ -> Some (ppat_var ("subproduct" |> Located.mk))
      in
      let pattern =
        [%pat?
          { f =
              T [%p ppat_construct (Located.mk (Lident variant_name)) constructor_option]
          }]
      in
      let rhs =
        match granularity with
        | Type_kind_intf.Shallow -> [%expr Sexplib.Sexp.Atom [%e estring variant_name]]
        | Type_kind_intf.Deep _ ->
          let atom_name_expression =
            Specific_implementation.name index element |> String.capitalize |> estring
          in
          let subproduct_module_name =
            generate_subproduct_module_name (module Specific_implementation) index element
          in
          let sexp_of_t_subproduct_function =
            pexp_ident
              (Ldot (Ldot (Lident subproduct_module_name, "Packed"), "sexp_of_t")
               |> Located.mk)
          in
          let subproduct_pack_subproductor_function =
            pexp_ident
              (Ldot (Ldot (Lident subproduct_module_name, "Packed"), "pack") |> Located.mk)
          in
          [%expr
            Sexplib.Sexp.List
              [ Sexplib.Sexp.Atom [%e atom_name_expression]
              ; [%e sexp_of_t_subproduct_function]
                  ([%e subproduct_pack_subproductor_function] subproduct)
              ]]
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  pexp_match [%expr packed] cases
;;

(**
   Generates the body for the t_of_sexp function inside of packed.

   match t with
   | Sexplib.Sexp.Atom "Constr1" -> Constr1
   | ...
*)
let t_of_sexp_body
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let cases =
    List.mapi elements_to_convert ~f:(fun index (element, granularity) ->
      let constructor = Specific_implementation.name index element |> String.capitalize in
      let acceptable_sexp_atoms = [ constructor; constructor |> String.uncapitalize ] in
      let pattern =
        let sexp_pattern =
          List.map acceptable_sexp_atoms ~f:(fun sexp_atom -> pstring sexp_atom)
          |> or_patterns ~loc
        in
        match granularity with
        | Type_kind_intf.Shallow -> [%pat? Sexplib.Sexp.Atom [%p sexp_pattern]]
        | Type_kind_intf.Deep _ ->
          let variant_atom_name = [%pat? Sexplib.Sexp.Atom [%p sexp_pattern]] in
          [%pat? Sexplib.Sexp.List ([%p variant_atom_name] :: subproduct_sexp_list)]
      in
      let rhs =
        match granularity with
        | Type_kind_intf.Shallow ->
          [%expr { f = T [%e pexp_construct (Located.mk (Lident constructor)) None] }]
        | Type_kind_intf.Deep _ ->
          let subproduct_module_name =
            generate_subproduct_module_name (module Specific_implementation) index element
          in
          let subproduct_t_of_sexp_function_expression =
            pexp_ident
              (Ldot (Ldot (Lident subproduct_module_name, "Packed"), "t_of_sexp")
               |> Located.mk)
          in
          let nested_constructor =
            pexp_construct
              (Lident constructor |> Located.mk)
              (Some (pexp_ident (Lident "subproduct_constructor" |> Located.mk)))
          in
          [%expr
            let subproduct_constructor =
              [%e subproduct_t_of_sexp_function_expression]
                (Typed_fields_lib.Private.list_to_sexp subproduct_sexp_list)
            in
            { f =
                (let { f = T subproduct_constructor } = subproduct_constructor in
                 T [%e nested_constructor])
            }]
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  let catch_all = case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr assert false] in
  let cases = cases @ [ catch_all ] in
  pexp_match [%expr sexp] cases
;;

let disable_warning_32 ~loc =
  let open (val Ast_builder.make loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-32") [] ])
;;

(**
   Generates a Deep functor by repeatedly applying the functor function for
   each subproduct in elements to convert. This is generic since the same
   functor must be produced for both the signature and the struct implementation.


   e.g. (Name_subproduct : <type>) (Name2_subproduct : <type>) = <inital_expression>
*)
let generic_generate_functor
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  ~functor_creation_function
  ~initial_expression
  =
  let open (val Ast_builder.make loc) in
  let subproduct_elements =
    List.filter_mapi elements_to_convert ~f:(fun original_index (element, granularity) ->
      match granularity with
      | Type_kind_intf.Shallow -> None
      | Type_kind_intf.Deep { minimum_needed_parameters; _ } ->
        Some (element, original_index, minimum_needed_parameters))
  in
  List.fold_right
    subproduct_elements
    ~init:initial_expression
    ~f:(fun (element, original_index, minimum_needed_parameters) acc ->
      let type_ = Specific_implementation.to_type element in
      let manifest_type, params =
        match type_.ptyp_desc with
        | Ptyp_constr (ident, params) ->
          let clean_params =
            List.init (List.length params) ~f:(fun i ->
              ptyp_var [%string "t%{(i + 1)#Int}"], (NoVariance, NoInjectivity))
          in
          ( Some (ptyp_constr ident (List.map clean_params ~f:(fun (f, _) -> f)))
          , clean_params )
        | _ -> Some type_, minimum_needed_parameters
      in
      let module_type =
        let module_type =
          Generic_generator.opaque_signature
            (module Typed_deriver_fields)
            ~loc
            ~manifest_type
            ~original_kind:Ptype_abstract
            ~params
        in
        { module_type with pmty_attributes = [ disable_warning_32 ~loc ] }
      in
      functor_creation_function
        (Named
           ( Some
               (Specific_implementation.name original_index element
                |> String.capitalize
                |> Type_kind_intf.append_functor_parameter)
             |> Located.mk
           , module_type ))
        acc)
;;

(**
   Generates the deep functor signature.
   e.g.

   module Deep
   (Name_subproduct : <type of name's base typed fields>)
   (Constr1: <type of constr1's typed fields>) = <base_module_type>
*)
let deep_functor_signature
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  ~base_module_type
  =
  let open (val Ast_builder.make loc) in
  let deep_module_type_with_functors =
    generic_generate_functor
      (module Specific_implementation)
      ~loc
      ~elements_to_convert
      ~functor_creation_function:pmty_functor
      ~initial_expression:base_module_type
  in
  psig_module
    (module_declaration
       ~name:(Some "Deep" |> Located.mk)
       ~type_:deep_module_type_with_functors)
;;

(**
   Generates the deep functor structure.
   e.g.

   module Deep
   (Name_subproduct : <type of name's base typed fields>)
   (Constr1: <type of constr1's typed fields>) = <module_expression>
*)
let deep_functor_structure
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  ~module_expression
  =
  let open (val Ast_builder.make loc) in
  let deep_module_expression_with_functors =
    generic_generate_functor
      (module Specific_implementation)
      ~loc
      ~elements_to_convert
      ~functor_creation_function:pmod_functor
      ~initial_expression:module_expression
  in
  deep_module_expression_with_functors
;;

let subproduct_module_name
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~index
  ~element
  =
  let element_name = Specific_implementation.name index element |> String.capitalize in
  [%string "%{element_name}_subproduct"]
;;

let generate_parameter_modules
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  List.filter_mapi elements_to_convert ~f:(fun index (element, granularity) ->
    let type_ = Specific_implementation.to_type element in
    match granularity, type_.ptyp_desc with
    | Type_kind_intf.Shallow, _ -> None
    | Type_kind_intf.Deep _, Ptyp_constr ({ txt = ident; _ }, _) ->
      let generate_submodule_name = function
        | "t" -> "Typed_field"
        | other -> "Typed_field_of_" ^ other
      in
      let rec generate_ident = function
        | Lident name -> Lident (generate_submodule_name name)
        | Ldot (other, name) -> Ldot (other, generate_submodule_name name)
        | Lapply (a, b) -> Lapply (a, generate_ident b)
      in
      Some (None, generate_ident ident, index, element)
    | ( Type_kind_intf.Deep { minimum_needed_parameters; original_type_with_attributes; _ }
      , _ ) ->
      let extension_anonymous_module =
        pmod_extension
          ( "typed_fields" |> Located.mk
          , PStr
              [ pstr_type
                  Recursive
                  [ type_declaration
                      ~name:(Located.mk "t")
                      ~params:minimum_needed_parameters
                      ~cstrs:[]
                      ~kind:Ptype_abstract
                      ~private_:Public
                      ~manifest:(Some original_type_with_attributes)
                  ]
              ] )
      in
      let module_name =
        subproduct_module_name (module Specific_implementation) ~index ~element
      in
      Some (Some extension_anonymous_module, Lident module_name, index, element))
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
let full_depth_module
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let parameter_modules =
    generate_parameter_modules (module Specific_implementation) ~loc ~elements_to_convert
    |> List.map ~f:(fun (expr, ident, original_index, original_element) ->
      let expr =
        match expr with
        | None -> None
        | Some expr ->
          let module_name =
            subproduct_module_name
              (module Specific_implementation)
              ~index:original_index
              ~element:original_element
          in
          Some (pstr_module (module_binding ~name:(Some module_name |> Located.mk) ~expr))
      in
      expr, ident)
  in
  let deep_functor_application =
    List.fold
      parameter_modules
      ~init:(pmod_ident (Lident "Deep" |> Located.mk))
      ~f:(fun acc (_, module_ident) ->
        pmod_apply acc (pmod_ident (module_ident |> Located.mk)))
  in
  let parameter_modules = List.filter_map parameter_modules ~f:(fun (f, _) -> f) in
  let full_depth_include = pstr_include (include_infos deep_functor_application) in
  parameter_modules @ [ full_depth_include ]
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
let full_depth_signature
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  let parameter_modules =
    generate_parameter_modules (module Specific_implementation) ~loc ~elements_to_convert
    |> List.map ~f:(fun (expr, ident, original_index, original_element) ->
      let module_sig_item =
        match expr with
        | None -> None
        | Some expr ->
          let module_name =
            subproduct_module_name
              (module Specific_implementation)
              ~index:original_index
              ~element:original_element
          in
          Some
            (psig_module
               (module_declaration
                  ~name:(Some module_name |> Located.mk)
                  ~type_:(pmty_typeof expr)))
      in
      module_sig_item, ident)
  in
  let deep_functor_application =
    List.fold
      parameter_modules
      ~init:(pmod_ident (Lident "Deep" |> Located.mk))
      ~f:(fun acc (_, module_ident) ->
        pmod_apply acc (pmod_ident (module_ident |> Located.mk)))
  in
  let parameter_modules = List.filter_map parameter_modules ~f:(fun (f, _) -> f) in
  let full_depth_include =
    psig_include (include_infos (pmty_typeof deep_functor_application))
  in
  parameter_modules @ [ full_depth_include ]
;;

let generate_base_module_type_for_singleton ~loc ~minimum_needed_parameters ~ctype =
  let open (val Ast_builder.make loc) in
  let core_type_params = List.map minimum_needed_parameters ~f:(fun (f, _) -> f) in
  let unique_id = generate_unique_id core_type_params in
  let upper =
    type_declaration
      ~name:(Located.mk derived_on_name)
      ~params:minimum_needed_parameters
      ~cstrs:[]
      ~kind:Ptype_abstract
      ~private_:Public
      ~manifest:(Some ctype)
  in
  let constructor =
    constructor_declaration
      ~name:(Located.mk "T")
      ~args:(Pcstr_tuple [])
      ~res:(Some (ptyp_constr (Lident "t" |> Located.mk) (core_type_params @ [ ctype ])))
  in
  let t_params =
    minimum_needed_parameters @ [ ptyp_var unique_id, (NoVariance, NoInjectivity) ]
  in
  let t_type_declaration =
    type_declaration
      ~name:("t" |> Located.mk)
      ~params:t_params
      ~cstrs:[]
      ~kind:(Ptype_variant [ constructor ])
      ~private_:Public
      ~manifest:None
  in
  pmty_signature
    ([ psig_type Nonrecursive [ upper ]; psig_type Recursive [ t_type_declaration ] ]
     @ Typed_deriver_fields.generate_include_signature
         ~loc
         ~params:minimum_needed_parameters)
;;

let generate_base_module_expr_for_singleton_for_any_parameter_length
  ~loc
  ~minimum_needed_parameters
  ~ctype
  =
  let open (val Ast_builder.make loc) in
  let core_type_params = List.map minimum_needed_parameters ~f:(fun (f, _) -> f) in
  let unique_id = generate_unique_id core_type_params in
  let ({ upper
       ; t_type_declaration
       ; internal_gadt_declaration
       ; upper_rename
       ; names
       ; name
       ; path
       ; ord
       ; type_ids
       ; packed
       }
        : Singleton_generator.common_items)
    =
    Singleton_generator.common
      ~loc
      ~minimum_needed_parameters
      ~core_type_params
      ~ctype
      ~unique_id
  in
  let creator_type =
    let td =
      generate_creator_type_declaration
        ~loc
        ~unique_parameter_id:unique_id
        ~core_type_params
        ~params:minimum_needed_parameters
        ~t_name:internal_gadt_name
    in
    pstr_type Recursive [ td ]
  in
  let create = [%stri let create { f } = f T] in
  let create_local = [%stri let create_local { f } = f T] in
  let clean_param_names =
    List.init (List.length minimum_needed_parameters) ~f:(fun i ->
      [%string "t%{(i + 1)#Int}"])
  in
  let constr_params =
    List.map clean_param_names ~f:(fun name -> ptyp_constr (Lident name |> Located.mk) [])
  in
  let constr_param_t =
    ptyp_constr
      (Lident "t" |> Located.mk)
      (constr_params @ [ ptyp_constr (Lident unique_id |> Located.mk) [] ])
  in
  let constr_param_derived_on =
    ptyp_constr (Lident derived_on_name |> Located.mk) constr_params
  in
  let get =
    let expr =
      let initial_expression =
        let pattern =
          ppat_constraint (ppat_construct (Lident "T" |> Located.mk) None) constr_param_t
        in
        let expression =
          let pattern =
            ppat_constraint (ppat_var (Located.mk "t")) constr_param_derived_on
          in
          let expression =
            pexp_constraint
              (pexp_ident (Lident "t" |> Located.mk))
              (ptyp_constr (Lident unique_id |> Located.mk) [])
          in
          pexp_fun Nolabel None pattern expression
        in
        pexp_fun Nolabel None pattern expression
      in
      List.fold_right
        clean_param_names
        ~init:(pexp_newtype (Located.mk unique_id) initial_expression)
        ~f:(fun name acc -> pexp_newtype (Located.mk name) acc)
    in
    let vb = value_binding ~pat:(ppat_var (Located.mk "get")) ~expr in
    pstr_value Nonrecursive [ vb ]
  in
  let set =
    let expr =
      let initial_expression =
        let pattern =
          ppat_constraint (ppat_construct (Lident "T" |> Located.mk) None) constr_param_t
        in
        let expression =
          let pattern = ppat_constraint ppat_any constr_param_derived_on in
          let expression =
            let pattern =
              ppat_constraint
                (ppat_var (Located.mk "t"))
                (ptyp_constr (Lident unique_id |> Located.mk) [])
            in
            let expression =
              pexp_constraint
                (pexp_ident (Located.mk (Lident "t")))
                constr_param_derived_on
            in
            pexp_fun Nolabel None pattern expression
          in
          pexp_fun Nolabel None pattern expression
        in
        pexp_fun Nolabel None pattern expression
      in
      List.fold_right
        clean_param_names
        ~init:(pexp_newtype (Located.mk unique_id) initial_expression)
        ~f:(fun name acc -> pexp_newtype (Located.mk name) acc)
    in
    let vb = value_binding ~pat:(ppat_var (Located.mk "set")) ~expr in
    pstr_value Nonrecursive [ vb ]
  in
  pmod_structure
    [ upper
    ; t_type_declaration
    ; internal_gadt_declaration
    ; upper_rename
    ; creator_type
    ; name
    ; path
    ; ord
    ; get
    ; set
    ; create
    ; create_local
    ; type_ids
    ; packed
    ; names
    ]
;;

let generate_base_module_expr_for_singleton ~loc ~minimum_needed_parameters ~ctype =
  let open (val Ast_builder.make loc) in
  let number_of_params = List.length minimum_needed_parameters in
  match number_of_params with
  | 0 | 1 | 2 | 3 | 4 | 5 ->
    let singleton_name =
      match number_of_params with
      | 0 -> "Singleton"
      | other -> [%string "Singleton%{other#Int}"]
    in
    let td =
      type_declaration
        ~name:(Located.mk "t")
        ~params:minimum_needed_parameters
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some ctype)
    in
    pmod_apply
      (pmod_ident (Ldot (Lident "Typed_fields_lib", singleton_name) |> Located.mk))
      (pmod_structure [ pstr_type Recursive [ td ] ])
  | _ ->
    generate_base_module_expr_for_singleton_for_any_parameter_length
      ~loc
      ~minimum_needed_parameters
      ~ctype
;;

let singleton_label_name
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~index
  ~element
  =
  let open (val Ast_builder.make loc) in
  let label =
    let name = Specific_implementation.name index element |> String.lowercase in
    [%string "Singleton_for_%{name}"]
  in
  let name = Some label |> Located.mk in
  label, name
;;

let generate_normalized_constr ~loc ~ident ~params =
  let open (val Ast_builder.make loc) in
  ptyp_constr
    ident
    (List.mapi params ~f:(fun i _ -> ptyp_var [%string "t%{(i + 1)#Int}"]))
;;

let generate_clean_params ~loc ~params =
  let open (val Ast_builder.make loc) in
  List.init (List.length params) ~f:(fun i ->
    ptyp_var [%string "t%{(i + 1)#Int}"], (NoVariance, NoInjectivity))
;;

(*  Generates the signature for the singleton modules sent to Shallow

    [
    module Singleton_for_t_1 : sig ... end;
    module Singleton_for_t_2 : sig ... end;
    ...

    ]
*)
let singleton_modules_signatures
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  List.filter_mapi elements_to_convert ~f:(fun index (element, granularity) ->
    let type_ = Specific_implementation.to_type element in
    match granularity, type_.ptyp_desc with
    | Shallow, _ -> None
    | Deep _, Ptyp_constr (ident, params) ->
      let label, name =
        singleton_label_name (module Specific_implementation) ~loc ~index ~element
      in
      let minimum_needed_parameters = generate_clean_params ~loc ~params in
      let normalized_constr = generate_normalized_constr ~loc ~ident ~params in
      let type_ =
        generate_base_module_type_for_singleton
          ~loc
          ~minimum_needed_parameters
          ~ctype:normalized_constr
      in
      Some (psig_module (module_declaration ~name ~type_), label)
    | Deep { minimum_needed_parameters; _ }, _ ->
      let label, name =
        singleton_label_name (module Specific_implementation) ~loc ~index ~element
      in
      let type_ =
        generate_base_module_type_for_singleton
          ~loc
          ~minimum_needed_parameters
          ~ctype:(Specific_implementation.to_type element)
      in
      Some (psig_module (module_declaration ~name ~type_), label))
;;

(*  Generates the structure for the sigleton modules sent to Shallow

    [
    module Singleton_for_t_1 = struct ... end;
    module Singleton_for_t_2 = struct ... end;
    ...

    ]
*)
let singleton_modules_structures
  (type a)
  (module Specific_implementation : Product_kind_intf.S with type t = a)
  ~loc
  ~elements_to_convert
  =
  let open (val Ast_builder.make loc) in
  List.filter_mapi elements_to_convert ~f:(fun index (element, granularity) ->
    let type_ = Specific_implementation.to_type element in
    match granularity, type_.ptyp_desc with
    | Shallow, _ -> None
    | Deep _, Ptyp_constr (ident, params) ->
      let label, name =
        singleton_label_name (module Specific_implementation) ~loc ~index ~element
      in
      let minimum_needed_parameters = generate_clean_params ~loc ~params in
      let normalized_constr = generate_normalized_constr ~loc ~ident ~params in
      let expr =
        generate_base_module_expr_for_singleton
          ~loc
          ~minimum_needed_parameters
          ~ctype:normalized_constr
      in
      Some (pstr_module (module_binding ~name ~expr), label)
    | Deep { minimum_needed_parameters; _ }, _ ->
      let label, name =
        singleton_label_name (module Specific_implementation) ~loc ~index ~element
      in
      let expr =
        generate_base_module_expr_for_singleton
          ~loc
          ~minimum_needed_parameters
          ~ctype:(Specific_implementation.to_type element)
      in
      Some (pstr_module (module_binding ~name ~expr), label))
;;
