open! Base
open Ppxlib

(* The structure items will be inserted after the type type
   definitions and before any other items.*)
let extra_structure_items_to_insert _ = []

let generate_constructor_declarations ~loc ~elements_to_convert ~core_type_params =
  let open (val Syntax.builder loc) in
  let unique_parameter_name = Type_kind.generate_unique_id core_type_params in
  List.map elements_to_convert ~f:(fun (element, _) ->
    let args =
      match (element : Variant_kind_generator.supported_constructor_declaration) with
      | Single_value_constructor { granularity = Constr_deep { params; _ }; _ } ->
        let subproduct_module_name =
          Variant_kind_generator.supported_constructor_name element
          |> String.capitalize
          |> Variant_kind_generator.append_functor_parameter
        in
        Pcstr_tuple
          [ ptyp_constr
              (Ldot (Lident subproduct_module_name, "t") |> Located.mk)
              (params @ [ ptyp_var unique_parameter_name ])
            |> Ppxlib_jane.Shim.Pcstr_tuple_arg.of_core_type
          ]
      | Single_value_constructor
          { minimum_needed_parameters; granularity = Polymorphic_deep; _ } ->
        let subproduct_module_name =
          Variant_kind_generator.supported_constructor_name element
          |> String.capitalize
          |> Variant_kind_generator.append_functor_parameter
        in
        let core_type_minimum_params =
          List.map minimum_needed_parameters ~f:(fun (f, _) -> f)
        in
        Pcstr_tuple
          [ ptyp_constr
              (Ldot (Lident subproduct_module_name, "t") |> Located.mk)
              (core_type_minimum_params @ [ ptyp_var unique_parameter_name ])
            |> Ppxlib_jane.Shim.Pcstr_tuple_arg.of_core_type
          ]
      | _ -> Pcstr_tuple []
    in
    let last_type =
      match element with
      | Single_value_constructor { granularity = Constr_deep _; _ }
      | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
        ptyp_var unique_parameter_name
      | _ -> Variant_kind_generator.supported_constructor_type element
    in
    ( (element, Type_kind.Shallow)
    , constructor_declaration
        ~name:
          (Variant_kind_generator.supported_constructor_name element
           |> String.capitalize
           |> Located.mk)
        ~args
        ~res:
          (Some
             (ptyp_constr
                (Located.mk (Lident Type_kind.internal_gadt_name))
                (core_type_params @ [ last_type ]))) ))
;;

(* Disables unused variable warning. *)
let disable_warning_27 ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-27") [] ])
;;

(* Disables unused match case warning. *)
let disable_warning_11 ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-11") [] ])
;;

let names_list ~loc ~elements_to_convert:_ =
  [%expr Base.List.map Packed.all ~f:(fun { f = T f } -> name f)]
;;

let generate_subvariant_pattern_if_needed ~loc
  : Variant_kind_generator.supported_constructor_declaration -> pattern option
  =
  let open (val Syntax.builder loc) in
  function
  | Single_value_constructor { granularity = Constr_deep _; _ }
  | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
    Some (ppat_var ("subvariant" |> Located.mk))
  | No_values_constructor _ | Anonymous_record_constructor _ | Tuple_values_constructor _
  | Single_value_constructor { granularity = Shallow; _ } -> None
;;

let generate_subvariant_name element =
  Variant_kind_generator.supported_constructor_name element
  |> String.capitalize
  |> Variant_kind_generator.append_functor_parameter
;;

let generate_subvariant_function ~loc ~element ~name =
  let open (val Syntax.builder loc) in
  let subvariant_name = generate_subvariant_name element in
  pexp_ident (Ldot (Lident subvariant_name, name) |> Located.mk)
;;

let name_function_body ~loc ~elements_to_convert:_ =
  [%expr fun x -> Base.List.last_exn (path x)]
;;

let path_function_body ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let patterns =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let variant_name =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let pattern =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_subvariant_pattern_if_needed ~loc element)
      in
      let rhs =
        let name =
          Variant_kind_generator.supported_constructor_name element
          |> String.lowercase
          |> estring
        in
        match element with
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let path_function = generate_subvariant_function ~loc ~element ~name:"path" in
          [%expr [%e name] :: [%e path_function] subvariant]
        | _ -> [%expr [ [%e name] ]]
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  let match_statement = pexp_match (pexp_ident (Located.mk (Lident "x"))) patterns in
  [%expr fun x -> [%e match_statement]]
;;

let ord_function_body ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let patterns =
    List.mapi elements_to_convert ~f:(fun index (element, _) ->
      let variant_name =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let pattern =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_subvariant_pattern_if_needed ~loc element)
      in
      let rhs =
        let current_ord = eint index in
        match element with
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let ord_function = generate_subvariant_function ~loc ~element ~name:"__ord" in
          [%expr [%e current_ord] :: [%e ord_function] subvariant]
        | _ -> [%expr [ [%e current_ord] ]]
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  let match_statement = pexp_match (pexp_ident (Located.mk (Lident "x"))) patterns in
  [%expr fun x -> [%e match_statement]]
;;

(* Given a list of label declarations, will generate a pattern that destructs the record,

   e.g. given

   a : int
   b : float

   generates:

   {a; b}
*)
let generate_record_pattern ~loc label_declarations =
  let open (val Syntax.builder loc) in
  let record_contents =
    List.map label_declarations ~f:(fun ld ->
      let ident = Lident ld.pld_name.txt |> Located.mk in
      let inner_pattern = ppat_var (ld.pld_name.txt |> Located.mk) in
      ident, inner_pattern)
  in
  ppat_record record_contents Closed
;;

(* Given a list of label declarations, will generate an expression that builds the record,

   e.g. given

   a : int
   b : float

   generates:

   {a; b}
*)
let generate_record_expression ~loc label_declarations =
  let open (val Syntax.builder loc) in
  let record_contents =
    List.map label_declarations ~f:(fun ld ->
      let ident = Lident ld.pld_name.txt |> Located.mk in
      let inner_expr = pexp_ident (Lident ld.pld_name.txt |> Located.mk) in
      ident, inner_expr)
  in
  pexp_record record_contents None
;;

(*Generates a tuple pattern that looks like this:

  x0, x1, x2, ... , xn
*)
let generate_tuple_pattern ~loc number_of_elements =
  let open (val Syntax.builder loc) in
  ppat_tuple
    (List.init number_of_elements ~f:(fun i ->
       None, ppat_var ([%string "x%{i#Int}"] |> Located.mk)))
    Closed
;;

(*Generates a tuple expression that looks like this:

  x0, x1, x2, ... , xn
*)
let generate_tuple_expression ~loc number_of_elements =
  let open (val Syntax.builder loc) in
  pexp_tuple
    (List.init number_of_elements ~f:(fun i ->
       None, pexp_ident (Lident [%string "x%{i#Int}"] |> Located.mk)))
;;

let generate_variant_generic ~loc ~element ~subpattern ~on_construct ~on_variant =
  let open (val Syntax.builder loc) in
  let variant_name =
    Variant_kind_generator.supported_constructor_name element |> String.capitalize
  in
  match element with
  | Tuple_values_constructor _ | Anonymous_record_constructor _
  | No_values_constructor { is_polymorphic = false; _ }
  | Single_value_constructor { is_polymorphic = false; _ } ->
    on_construct (Lident variant_name |> Located.mk) subpattern
  | Single_value_constructor { is_polymorphic = true; _ }
  | No_values_constructor { is_polymorphic = true; _ } ->
    on_variant variant_name subpattern
;;

(*Generates either a constructor pattern or a variant pattern as needed
  e.g. A contents vs `A contents.
*)
let generate_variant_pattern ~loc element subpattern =
  let open (val Syntax.builder loc) in
  generate_variant_generic
    ~loc
    ~element
    ~subpattern
    ~on_construct:ppat_construct
    ~on_variant:ppat_variant
;;

(*Generates either a constructor expression or a variant pattern as needed
  e.g. A contents vs `A contents.
*)
let generate_variant_expression ~loc element subpattern =
  let open (val Syntax.builder loc) in
  generate_variant_generic
    ~loc
    ~element
    ~subpattern
    ~on_construct:pexp_construct
    ~on_variant:pexp_variant
;;

let get_function_body ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let cases =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let variant_name =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let right_subpattern =
        match element with
        | No_values_constructor _ -> None
        | Single_value_constructor _ -> Some (ppat_var ("contents" |> Located.mk))
        | Tuple_values_constructor { tuple_types; _ } ->
          Some (generate_tuple_pattern ~loc (List.length tuple_types))
        | Anonymous_record_constructor { label_declarations; _ } ->
          Some (generate_record_pattern ~loc label_declarations)
      in
      let pattern =
        let left_pattern =
          ppat_construct
            (Located.mk (Lident variant_name))
            (generate_subvariant_pattern_if_needed ~loc element)
        in
        let right_pattern = generate_variant_pattern ~loc element right_subpattern in
        ppat_tuple [ None, left_pattern; None, right_pattern ] Closed
      in
      let rhs =
        match element with
        | No_values_constructor _ -> [%expr Some ()]
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let get_function = generate_subvariant_function ~loc ~element ~name:"get" in
          [%expr [%e get_function] subvariant contents]
        | Single_value_constructor _ -> [%expr Some contents]
        | Tuple_values_constructor { tuple_types; _ } ->
          [%expr Some [%e generate_tuple_expression ~loc (List.length tuple_types)]]
        | Anonymous_record_constructor { label_declarations; _ } ->
          [%expr Some [%e generate_record_expression ~loc label_declarations]]
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  let match_expression =
    pexp_tuple
      [ None, pexp_ident (Lident "t" |> Located.mk)
      ; None, pexp_ident (Lident "variant" |> Located.mk)
      ]
  in
  let catch_all = case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr None] in
  let cases = cases @ [ catch_all ] in
  let function_body = [%expr fun t variant -> [%e pexp_match match_expression cases]] in
  (* Preserve attributes that the parser inserts on [function_body]. Jane Street's
     compiler encodes changes to the parsetree via attributes, so we don't want to drop
     these. Ppxlib and Jane Street's compiler logically treat later-occurring attributes
     as "outer", so to get the new attribute to be the outermost one, we append it to the
     end of the list.
  *)
  { function_body with
    pexp_attributes = function_body.pexp_attributes @ [ disable_warning_11 ~loc ]
  }
;;

let create_function_body ~loc ~constructor_declarations ~local:_ =
  let open (val Syntax.builder loc) in
  let cases =
    List.map constructor_declarations ~f:(fun ((element, _), _) ->
      let variant_ident =
        Lident
          (Variant_kind_generator.supported_constructor_name element |> String.capitalize)
        |> Located.mk
      in
      let lhs =
        ppat_construct variant_ident (generate_subvariant_pattern_if_needed ~loc element)
      in
      let rhs =
        match element with
        | No_values_constructor _ -> generate_variant_expression ~loc element None
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let create_function =
            generate_subvariant_function ~loc ~element ~name:"create"
          in
          generate_variant_expression
            ~loc
            element
            (Some [%expr [%e create_function] subvariant value])
        | Single_value_constructor _ ->
          generate_variant_expression
            ~loc
            element
            (Some (pexp_ident (Lident "value" |> Located.mk)))
        | Tuple_values_constructor { tuple_types; _ } ->
          let number_of_elements = List.length tuple_types in
          let unpacking_pattern = generate_tuple_pattern ~loc number_of_elements in
          let packing_expression = generate_tuple_expression ~loc number_of_elements in
          let constructor_expression =
            pexp_construct variant_ident (Some packing_expression)
          in
          [%expr
            let [%p unpacking_pattern] = value in
            [%e constructor_expression]]
        | Anonymous_record_constructor { label_declarations; _ } ->
          let constructor =
            pexp_construct
              variant_ident
              (Some (generate_record_expression ~loc label_declarations))
          in
          List.fold label_declarations ~init:constructor ~f:(fun acc ld ->
            pexp_let
              Nonrecursive
              [ (let pat = ppat_var (ld.pld_name.txt |> Located.mk) in
                 let expr =
                   let record_expr = pexp_ident (Lident "value" |> Located.mk) in
                   pexp_field record_expr (Lident ld.pld_name.txt |> Located.mk)
                 in
                 value_binding ~pat ~expr ~modes:[])
              ]
              acc)
      in
      case ~lhs ~guard:None ~rhs)
  in
  let body =
    [%expr fun t value -> [%e pexp_match (pexp_ident (Located.mk (Lident "t"))) cases]]
  in
  (* Preserve attributes that the parser inserts on [body]. Jane Street's compiler
     encodes changes to the parsetree via attributes, so we don't want to drop these.
     Ppxlib and Jane Street's compiler logically treat later-occurring attributes
     as "outer", so to get the new attribute to be the outermost one, we append
     it to the end of the list.
  *)
  { body with pexp_attributes = body.pexp_attributes @ [ disable_warning_27 ~loc ] }
;;

let type_ids ~loc ~elements_to_convert ~core_type_params =
  let open (val Syntax.builder loc) in
  let param_name_to_index = Type_kind.generate_param_name_to_index ~core_type_params in
  let mapper = Type_kind.create_mapper ~loc param_name_to_index in
  List.map elements_to_convert ~f:(fun (element, _) ->
    let subvariant_name = generate_subvariant_name element in
    let name = Some (subvariant_name ^ "_type_ids") |> Located.mk in
    let functor_name =
      pmod_ident (Ldot (Lident subvariant_name, "Type_ids") |> Located.mk)
    in
    match element with
    | Single_value_constructor { granularity = Constr_deep { params; _ }; _ } ->
      let expr =
        List.fold params ~init:functor_name ~f:(fun acc param ->
          let param_str =
            let td =
              type_declaration
                ~name:(Located.mk "t")
                ~params:[]
                ~cstrs:[]
                ~kind:Ptype_abstract
                ~private_:Public
                ~manifest:(Some (mapper#core_type param))
            in
            pmod_structure [ pstr_type Recursive [ td ] ]
          in
          pmod_apply acc param_str)
      in
      pstr_module (module_binding ~name ~expr)
    | Single_value_constructor
        { minimum_needed_parameter_ids; granularity = Polymorphic_deep; _ } ->
      let expr =
        List.fold
          (List.sort ~compare:Int.compare minimum_needed_parameter_ids)
          ~init:functor_name
          ~f:(fun acc param ->
            let param_str =
              pmod_ident (Lident [%string "T%{(param + 1)#Int}"] |> Located.mk)
            in
            pmod_apply acc param_str)
      in
      pstr_module (module_binding ~name ~expr)
    | _ ->
      [%stri
        let ([%p
               pvar
                 (Variant_kind_generator.supported_constructor_name element
                  |> String.lowercase)] :
              [%t
                mapper#core_type
                  (Variant_kind_generator.supported_constructor_type element)]
                Base.Type_equal.Id.t)
          =
          Base.Type_equal.Id.create
            ~name:
              [%e
                estring
                  (Variant_kind_generator.supported_constructor_name element
                   |> String.lowercase)]
            Sexplib.Conv.sexp_of_opaque
        ;;])
;;

let type_id_function_body ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let cases =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let variant_name =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let pattern =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_subvariant_pattern_if_needed ~loc element)
      in
      let rhs =
        match element with
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let subvariant_name = generate_subvariant_name element in
          let type_ids_name = subvariant_name ^ "_type_ids" in
          let type_id_function =
            pexp_ident (Ldot (Lident type_ids_name, "type_id") |> Located.mk)
          in
          [%expr [%e type_id_function] subvariant]
        | _ ->
          let type_id = Located.mk (Lident (variant_name |> String.lowercase)) in
          pexp_ident type_id
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  pexp_function cases
;;

let globalize0_function_body ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let cases =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let variant_name =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let lhs =
        ppat_construct
          (Located.mk (Lident variant_name))
          (generate_subvariant_pattern_if_needed ~loc element)
      in
      let rhs =
        match element with
        | Single_value_constructor { granularity = Constr_deep _ | Polymorphic_deep; _ }
          ->
          let module_name = generate_subvariant_name element in
          let f = pexp_ident (Located.mk (Ldot (Lident module_name, "globalize0"))) in
          pexp_construct
            (Located.mk (Lident variant_name))
            (Some [%expr [%e f] subvariant])
        | _ -> pexp_construct (Located.mk (Lident variant_name)) None
      in
      case ~lhs ~guard:None ~rhs)
  in
  pexp_function cases
;;

let globalize_packed_function_body ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let cases =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let variant_name =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let lhs =
        let pat =
          ppat_construct
            (Located.mk (Lident variant_name))
            (generate_subvariant_pattern_if_needed ~loc element)
        in
        [%pat? { f = T [%p pat] }]
      in
      let rhs =
        match element with
        | Single_value_constructor { granularity = Polymorphic_deep | Constr_deep _; _ }
          ->
          let module_name = generate_subvariant_name element in
          let f name = evar (String.concat ~sep:"." [ module_name; name ]) in
          let exp =
            pexp_construct (Located.mk (Lident variant_name)) (Some [%expr subvariant])
          in
          [%expr
            let subvariant = [%e f "Packed.pack"] ([%e f "globalize0"] subvariant) in
            { f =
                (let { f = T subvariant } = subvariant in
                 T [%e exp])
            }]
        | _ ->
          let exp = pexp_construct (Located.mk (Lident variant_name)) None in
          [%expr { f = T [%e exp] }]
      in
      case ~lhs ~guard:None ~rhs)
  in
  pexp_function cases
;;

let sexp_of_t_body ~loc ~elements_to_convert ~stack =
  let open (val Syntax.builder loc) in
  let cases =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let variant_name =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let pattern =
        [%pat?
          { f =
              T
                [%p
                  ppat_construct
                    (Located.mk (Lident variant_name))
                    (generate_subvariant_pattern_if_needed ~loc element)]
          }]
      in
      let rhs =
        match element with
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let subvariant_name = generate_subvariant_name element in
          let sexp_of_t_function =
            pexp_ident
              (Ldot
                 ( Ldot (Lident subvariant_name, "Packed")
                 , Names.stackify "sexp_of_t" ~stack )
               |> Located.mk)
          in
          let pack_function =
            pexp_ident
              (Ldot
                 ( Ldot (Lident subvariant_name, "Packed")
                 , Names.localize "pack" ~local:stack )
               |> Located.mk)
          in
          [%expr
            Sexplib.Sexp.List
              [ Sexplib.Sexp.Atom
                  [%e estring (Variant_kind_generator.supported_constructor_name element)]
              ; [%e sexp_of_t_function] ([%e pack_function] subvariant)
              ]]
        | _ -> [%expr Sexplib.Sexp.Atom [%e estring variant_name]]
      in
      case ~lhs:pattern ~guard:None ~rhs:(Type_kind.exclave_if_stack ~loc ~stack rhs))
  in
  pexp_match [%expr packed] cases
;;

let all_body ~loc ~constructor_declarations =
  let open (val Syntax.builder loc) in
  let packed_fields =
    List.map constructor_declarations ~f:(fun ((element, _), _) ->
      match (element : Variant_kind_generator.supported_constructor_declaration) with
      | Single_value_constructor { granularity = Constr_deep _; _ }
      | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
        let subvariant_name = generate_subvariant_name element in
        let all_expr =
          pexp_ident (Ldot (Ldot (Lident subvariant_name, "Packed"), "all") |> Located.mk)
        in
        let inner_constructor =
          pexp_construct
            (Lident (Variant_kind_generator.supported_constructor_name element)
             |> Located.mk)
            (Some [%expr subvariant])
        in
        [%expr
          Base.List.map [%e all_expr] ~f:(fun { f = subvariant } ->
            { f =
                (let (T subvariant) = subvariant in
                 T [%e inner_constructor])
            })]
      | _ ->
        [%expr
          [ { f =
                T
                  [%e
                    pexp_construct
                      (Lident (Variant_kind_generator.supported_constructor_name element)
                       |> Located.mk)
                      None]
            }
          ]])
  in
  [%expr Base.List.concat [%e elist packed_fields]]
;;

let wrap_t_struct_around_expression ~loc expression =
  let open (val Syntax.builder loc) in
  pexp_record [ Lident "f" |> Located.mk, expression ] None
;;

let pack_body ~loc ~elements_to_convert ~local =
  let open (val Syntax.builder loc) in
  let cases =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let constructor_name =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let lhs =
        ppat_construct
          (Lident constructor_name |> Located.mk)
          (generate_subvariant_pattern_if_needed ~loc element)
      in
      let rhs =
        match element with
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let subvariant_name = generate_subvariant_name element in
          let pack_function =
            pexp_ident
              (Ldot (Ldot (Lident subvariant_name, "Packed"), Names.localize "pack" ~local)
               |> Located.mk)
          in
          let inner_constructor =
            pexp_construct
              (Lident (Variant_kind_generator.supported_constructor_name element)
               |> Located.mk)
              (Some [%expr subvariant])
          in
          [%expr
            let subvariant = [%e pack_function] subvariant in
            { f =
                (let { f = T subvariant } = subvariant in
                 T [%e inner_constructor])
            }]
        | _ ->
          pexp_construct
            (Lident "T" |> Located.mk)
            (Some (pexp_construct (Lident constructor_name |> Located.mk) None))
          |> wrap_t_struct_around_expression ~loc
      in
      case ~lhs ~guard:None ~rhs:(Type_kind.exclave_if_local ~loc ~local rhs))
  in
  pexp_function cases
;;

let t_of_sexp_body ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let cases =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let constructor =
        Variant_kind_generator.supported_constructor_name element |> String.capitalize
      in
      let acceptable_sexp_atoms = [ constructor; constructor |> String.uncapitalize ] in
      let pattern =
        let sexp_pattern =
          List.map acceptable_sexp_atoms ~f:(fun sexp_atom -> pstring sexp_atom)
          |> Type_kind.or_patterns ~loc
        in
        match element with
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let variant_atom_name = [%pat? Sexplib.Sexp.Atom [%p sexp_pattern]] in
          [%pat? Sexplib.Sexp.List ([%p variant_atom_name] :: subvariant_sexp_list)]
        | _ -> [%pat? Sexplib.Sexp.Atom [%p sexp_pattern]]
      in
      let rhs =
        match element with
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let subvariant_name = generate_subvariant_name element in
          let t_of_sexp_function =
            pexp_ident
              (Ldot (Ldot (Lident subvariant_name, "Packed"), "t_of_sexp") |> Located.mk)
          in
          let inner_constructor =
            pexp_construct
              (Lident (Variant_kind_generator.supported_constructor_name element)
               |> Located.mk)
              (Some [%expr subvariant_constructor])
          in
          [%expr
            let subvariant_constructor =
              [%e t_of_sexp_function]
                (Typed_fields_lib.Private.list_to_sexp subvariant_sexp_list)
            in
            { f =
                (let { f = T subvariant_constructor } = subvariant_constructor in
                 T [%e inner_constructor])
            }]
        | _ ->
          [%expr { f = T [%e pexp_construct (Located.mk (Lident constructor)) None] }]
      in
      case ~lhs:pattern ~guard:None ~rhs)
  in
  let catch_all = case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr assert false] in
  let cases = cases @ [ catch_all ] in
  pexp_match [%expr sexp] cases
;;

let which_function_body ~loc ~elements_to_convert ~number_of_params:_ =
  let open (val Syntax.builder loc) in
  let cases =
    List.map elements_to_convert ~f:(fun (element, _) ->
      let variant_name = Variant_kind_generator.supported_constructor_name element in
      let lhs =
        let inner_constructor =
          match element with
          | No_values_constructor _ -> None
          | Single_value_constructor { granularity = Constr_deep _; _ }
          | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
            Some (ppat_var (Located.mk "contents"))
          | Single_value_constructor _
          | Tuple_values_constructor _
          | Anonymous_record_constructor _ -> Some ppat_any
        in
        generate_variant_pattern ~loc element inner_constructor
      in
      let rhs =
        match element with
        | Single_value_constructor { granularity = Constr_deep _; _ }
        | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
          let which_function = generate_subvariant_function ~loc ~element ~name:"which" in
          let inner_constructor =
            pexp_construct
              (Lident variant_name |> Located.mk)
              (Some [%expr subvariant_constructor])
          in
          [%expr
            let subvariant_constructor = [%e which_function] contents in
            { f =
                (let { f = T subvariant_constructor } = subvariant_constructor in
                 T [%e inner_constructor])
            }]
        | _ ->
          [%expr { f = T [%e pexp_construct (Lident variant_name |> Located.mk) None] }]
      in
      case ~lhs ~guard:None ~rhs)
  in
  pexp_function cases
;;

(* Disables unused function warning *)
let disable_warning_32 ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-32") [] ])
;;

module Subproduct_element = struct
  type t =
    | Constr of
        { element : Variant_kind_generator.supported_constructor_declaration
        ; ident : longident_loc
        ; params : core_type list
        }
    | Poly of
        { element : Variant_kind_generator.supported_constructor_declaration
        ; minimum_needed_parameters : (core_type * (variance * injectivity)) list
        }
end

(*
   Generates a Deep functor by repeatedly applying the functor function for
   each subvariant in elements to convert. This is generic since the same
   functor must be produced for both the signature and the struct implementation.

   e.g. (Name_subvariant : <type>) (Name2_subvariant : <type>) = <inital_expression>
*)
let generic_generate_functor
  ~loc
  ~elements_to_convert
  ~functor_creation_function
  ~initial_expression
  =
  let open (val Syntax.builder loc) in
  let subproduct_elements =
    List.filter_map elements_to_convert ~f:(fun element : Subproduct_element.t option ->
      match (element : Variant_kind_generator.supported_constructor_declaration) with
      | Single_value_constructor { minimum_needed_parameters; granularity; _ } ->
        (match granularity with
         | Shallow -> None
         | Constr_deep { ident; params } -> Some (Constr { element; ident; params })
         | Polymorphic_deep -> Some (Poly { element; minimum_needed_parameters }))
      | _ -> None)
  in
  List.fold_right
    subproduct_elements
    ~init:initial_expression
    ~f:(fun functor_parameter acc ->
      let manifest_type, params =
        match functor_parameter with
        | Constr { element = _; ident; params } ->
          let clean_params =
            List.init (List.length params) ~f:(fun i ->
              ptyp_var [%string "t%{(i + 1)#Int}"], (NoVariance, NoInjectivity))
          in
          ( Some (ptyp_constr ident (List.map clean_params ~f:(fun (f, _) -> f)))
          , clean_params )
        | Poly { element; minimum_needed_parameters } ->
          ( Some (Variant_kind_generator.supported_constructor_type element)
          , minimum_needed_parameters )
      in
      let module_type =
        let module_type =
          Generic_generator.opaque_signature
            (module Typed_deriver_variants)
            ~loc
            ~manifest_type
            ~original_kind:Ptype_abstract
            ~params
        in
        { module_type with pmty_attributes = [ disable_warning_32 ~loc ] }
      in
      let element =
        match functor_parameter with
        | Constr { element; _ } | Poly { element; _ } -> element
      in
      functor_creation_function
        (Named
           ( Some
               (Variant_kind_generator.supported_constructor_name element
                |> String.capitalize
                |> Variant_kind_generator.append_functor_parameter)
             |> Located.mk
           , module_type
           , [] )
         |> Ppxlib_jane.Shim.Functor_parameter.to_parsetree)
        acc)
;;

(** Generates the deep functor signature. e.g.

    module Deep (Name_subproduct : <type of name's base typed fields>) (Constr1: <type of
    constr1's typed fields>) = <base_module_type> *)
let deep_functor_signature ~loc ~elements_to_convert ~base_module_type =
  let open (val Syntax.builder loc) in
  let deep_module_type_with_functors =
    generic_generate_functor
      ~loc
      ~elements_to_convert
      ~functor_creation_function:pmty_functor
      ~initial_expression:base_module_type
  in
  psig_module
    (module_declaration (Some "Deep" |> Located.mk) deep_module_type_with_functors)
;;

(** Generates the deep functor structure. e.g.

    module Deep (Name_subproduct : <type of name's base typed fields>) (Constr1: <type of
    constr1's typed fields>) = <module_expression> *)
let deep_functor_structure ~loc ~elements_to_convert ~module_expression =
  let open (val Syntax.builder loc) in
  let deep_module_expression_with_functors =
    generic_generate_functor
      ~loc
      ~elements_to_convert
      ~functor_creation_function:pmod_functor
      ~initial_expression:module_expression
  in
  pstr_module
    (module_binding
       ~name:(Some "Deep" |> Located.mk)
       ~expr:deep_module_expression_with_functors)
;;

module Parameter_module = struct
  type t =
    | Constr of { module_ident : longident }
    | Poly of
        { expr : module_expr
        ; name : string
        }
end

let generate_parameter_modules ~loc ~elements_to_convert ~expand_typed_variants =
  let open (val Syntax.builder loc) in
  List.filter_map elements_to_convert ~f:(fun element : Parameter_module.t option ->
    let generate_submodule_name = function
      | "t" -> "Typed_variant"
      | other -> "Typed_variant_of_" ^ other
    in
    match (element : Variant_kind_generator.supported_constructor_declaration) with
    | Single_value_constructor
        { granularity = Constr_deep { ident = { txt = ident; _ }; _ }; _ } ->
      let rec generate_ident = function
        | Lident name -> Lident (generate_submodule_name name)
        | Ldot (other, name) -> Ldot (other, generate_submodule_name name)
        | Lapply (a, b) -> Lapply (a, generate_ident b)
      in
      Some (Constr { module_ident = generate_ident ident })
    | Single_value_constructor
        { minimum_needed_parameters
        ; return_value_type_with_original_attributes
        ; granularity = Polymorphic_deep
        ; _
        } ->
      let extension_anonymous_module =
        expand_typed_variants
          ~loc
          Recursive
          [ type_declaration
              ~name:(Located.mk "t")
              ~params:minimum_needed_parameters
              ~cstrs:[]
              ~kind:Ptype_abstract
              ~private_:Public
              ~manifest:(Some return_value_type_with_original_attributes)
          ]
      in
      let name =
        generate_submodule_name
          (Variant_kind_generator.supported_constructor_name element |> String.lowercase)
      in
      Some (Poly { expr = extension_anonymous_module; name })
    | _ -> None)
;;

(** Generates the full depth module of a structure, e.g.
    [ module Constr1_subproduct = [%typed_field ...]; module Name_subproduct = [%typed_field ...]; ...; include Deep (Constr1_subproduct) (Name_subproduct) ] *)
let full_depth_module ~loc ~elements_to_convert ~expand_typed_variants =
  let open (val Syntax.builder loc) in
  let parameter_modules =
    generate_parameter_modules ~loc ~elements_to_convert ~expand_typed_variants
  in
  let module_definitions =
    List.filter_map parameter_modules ~f:(function
      | Poly { expr; name } ->
        Some (pstr_module (module_binding ~name:(Some name |> Located.mk) ~expr))
      | Constr _ -> None)
  in
  let deep_functor_application =
    List.fold
      parameter_modules
      ~init:(pmod_ident (Lident "Deep" |> Located.mk))
      ~f:(fun acc possible_module ->
        match possible_module with
        | Constr { module_ident } ->
          pmod_apply acc (pmod_ident (module_ident |> Located.mk))
        | Poly { expr = _; name } ->
          pmod_apply acc (pmod_ident (Lident name |> Located.mk)))
  in
  let full_depth_include =
    pstr_include (include_infos deep_functor_application ~kind:Structure)
  in
  module_definitions @ [ full_depth_include ]
;;

(** Generates the full_depth module's signature. e.g.

    [ module Constr1_subproduct : module type of [%typed_field ...]; module Name_subproduct : module type of [%typed_field ...]; ...; include module type of Deep (Constr1_subproduct) (Name_subproduct) ] *)
let full_depth_signature ~loc ~elements_to_convert ~expand_typed_variants =
  let open (val Syntax.builder loc) in
  let parameter_modules =
    generate_parameter_modules ~loc ~elements_to_convert ~expand_typed_variants
  in
  let module_definitions =
    List.filter_map parameter_modules ~f:(function
      | Poly { expr; name } ->
        let type_ = pmty_typeof expr in
        Some (psig_module (module_declaration (Some name |> Located.mk) type_))
      | Constr _ -> None)
  in
  let deep_functor_application =
    List.fold
      parameter_modules
      ~init:(pmod_ident (Lident "Deep" |> Located.mk))
      ~f:(fun acc possible_module ->
        match possible_module with
        | Constr { module_ident } ->
          pmod_apply acc (pmod_ident (module_ident |> Located.mk))
        | Poly { expr = _; name } ->
          pmod_apply acc (pmod_ident (Lident name |> Located.mk)))
  in
  let full_depth_include =
    psig_include
      (include_infos (pmty_typeof deep_functor_application) ~kind:Structure)
      ~modalities:[]
  in
  module_definitions @ [ full_depth_include ]
;;

let generate_base_module_type_for_singleton ~loc ~minimum_needed_parameters ~ctype =
  let open (val Syntax.builder loc) in
  let core_type_params = List.map minimum_needed_parameters ~f:(fun (f, _) -> f) in
  let unique_id = Type_kind.generate_unique_id core_type_params in
  let upper =
    type_declaration
      ~name:(Located.mk Names.derived_on_name)
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
    (signature
       ([ psig_type Nonrecursive [ upper ]; psig_type Recursive [ t_type_declaration ] ]
        @ Typed_deriver_variants.generate_include_signature
            ~loc
            ~params:minimum_needed_parameters))
;;

let generate_base_module_expr_for_singleton_for_any_arity
  ~loc
  ~minimum_needed_parameters
  ~ctype
  =
  let open (val Syntax.builder loc) in
  let core_type_params = List.map minimum_needed_parameters ~f:fst in
  let unique_id = Type_kind.generate_unique_id core_type_params in
  let ({ upper
       ; t_type_declaration
       ; internal_gadt_declaration
       ; upper_rename
       ; names
       ; name
       ; path
       ; ord
       ; globalize0
       ; globalize
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
    ptyp_constr (Lident Names.derived_on_name |> Located.mk) constr_params
  in
  let create =
    let expr =
      let initial_expression =
        let pattern =
          ppat_constraint
            (ppat_construct (Lident "T" |> Located.mk) None)
            (Some constr_param_t)
            []
        in
        let expression =
          let pattern =
            ppat_constraint
              (ppat_var (Located.mk "t"))
              (Some (ptyp_constr (Lident unique_id |> Located.mk) []))
              []
          in
          let expression =
            pexp_constraint
              (pexp_ident (Lident "t" |> Located.mk))
              (Some constr_param_derived_on)
              []
          in
          pexp_fun Nolabel None pattern expression
        in
        pexp_fun Nolabel None pattern expression
      in
      List.fold_right
        clean_param_names
        ~init:(pexp_newtype (Located.mk unique_id) None initial_expression)
        ~f:(fun name acc -> pexp_newtype (Located.mk name) None acc)
    in
    let vb = value_binding ~pat:(ppat_var (Located.mk "create")) ~expr ~modes:[] in
    pstr_value Nonrecursive [ vb ]
  in
  let get =
    let expr =
      let initial_expression =
        let pattern =
          ppat_constraint
            (ppat_construct (Lident "T" |> Located.mk) None)
            (Some constr_param_t)
            []
        in
        let expression =
          let pattern =
            ppat_constraint (ppat_var (Located.mk "t")) (Some constr_param_derived_on) []
          in
          let expression =
            pexp_constraint
              (pexp_construct
                 (Lident "Some" |> Located.mk)
                 (Some (pexp_ident (Lident "t" |> Located.mk))))
              (Some
                 (ptyp_constr
                    (Lident "option" |> Located.mk)
                    [ ptyp_constr (Lident unique_id |> Located.mk) [] ]))
              []
          in
          pexp_fun Nolabel None pattern expression
        in
        pexp_fun Nolabel None pattern expression
      in
      List.fold_right
        clean_param_names
        ~init:(pexp_newtype (Located.mk unique_id) None initial_expression)
        ~f:(fun name acc -> pexp_newtype (Located.mk name) None acc)
    in
    let vb = value_binding ~pat:(ppat_var (Located.mk "get")) ~expr ~modes:[] in
    pstr_value Nonrecursive [ vb ]
  in
  let which = [%stri let which _ = { Packed.f = Packed.T T }] in
  pmod_structure
    [ upper
    ; t_type_declaration
    ; internal_gadt_declaration
    ; upper_rename
    ; name
    ; path
    ; ord
    ; get
    ; create
    ; type_ids
    ; globalize0
    ; globalize
    ; packed
    ; names
    ; which
    ]
;;

let generate_base_module_expr_for_singleton ~loc ~minimum_needed_parameters ~ctype =
  let open (val Syntax.builder loc) in
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
      (pmod_ident (Ldot (Lident "Typed_variants_lib", singleton_name) |> Located.mk))
      (pmod_structure [ pstr_type Recursive [ td ] ])
  | _ ->
    generate_base_module_expr_for_singleton_for_any_arity
      ~loc
      ~minimum_needed_parameters
      ~ctype
;;

let singleton_name ~loc element =
  let open (val Syntax.builder loc) in
  let name =
    Variant_kind_generator.supported_constructor_name element |> String.lowercase
  in
  Some [%string "Singleton_for_%{name}"] |> Located.mk
;;

let generate_normalized_constr ~loc ~ident ~params =
  let open (val Syntax.builder loc) in
  ptyp_constr
    ident
    (List.mapi params ~f:(fun i _ -> ptyp_var [%string "t%{(i + 1)#Int}"]))
;;

let generate_clean_params ~loc ~params =
  let open (val Syntax.builder loc) in
  List.init (List.length params) ~f:(fun i ->
    ptyp_var [%string "t%{(i + 1)#Int}"], (NoVariance, NoInjectivity))
;;

(*  Generates the signature for the sigleton modules sent to Shallow

    [
    module Singleton_for_t_1 : sig ... end;
    module Singleton_for_t_2 : sig ... end;
    ...

    ]
*)
let singleton_modules_signatures ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  List.filter_map elements_to_convert ~f:(fun element ->
    match (element : Variant_kind_generator.supported_constructor_declaration) with
    | Single_value_constructor { granularity = Constr_deep { params; ident }; _ } ->
      let name = singleton_name ~loc element in
      let minimum_needed_parameters = generate_clean_params ~loc ~params in
      let normalized_constr = generate_normalized_constr ~loc ~ident ~params in
      let type_ =
        generate_base_module_type_for_singleton
          ~loc
          ~minimum_needed_parameters
          ~ctype:normalized_constr
      in
      Some (psig_module (module_declaration name type_))
    | Single_value_constructor
        { minimum_needed_parameters; granularity = Polymorphic_deep; _ } ->
      let name = singleton_name ~loc element in
      let type_ =
        generate_base_module_type_for_singleton
          ~loc
          ~minimum_needed_parameters
          ~ctype:(Variant_kind_generator.supported_constructor_type element)
      in
      Some (psig_module (module_declaration name type_))
    | _ -> None)
;;

(*  Generates the structure for the sigleton modules sent to Shallow

    [
    module Singleton_for_t_1 = struct ... end;
    module Singleton_for_t_2 = struct ... end;
    ...

    ]
*)
let singleton_modules_structures ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  List.filter_map elements_to_convert ~f:(fun element ->
    match (element : Variant_kind_generator.supported_constructor_declaration) with
    | Single_value_constructor { granularity = Constr_deep { params; ident }; _ } ->
      let name = singleton_name ~loc element in
      let minimum_needed_parameters = generate_clean_params ~loc ~params in
      let normalized_constr = generate_normalized_constr ~loc ~ident ~params in
      let expr =
        generate_base_module_expr_for_singleton
          ~loc
          ~minimum_needed_parameters
          ~ctype:normalized_constr
      in
      Some (pstr_module (module_binding ~name ~expr))
    | Single_value_constructor
        { minimum_needed_parameters; granularity = Polymorphic_deep; _ } ->
      let name = singleton_name ~loc element in
      let expr =
        generate_base_module_expr_for_singleton
          ~loc
          ~minimum_needed_parameters
          ~ctype:(Variant_kind_generator.supported_constructor_type element)
      in
      Some (pstr_module (module_binding ~name ~expr))
    | _ -> None)
;;
