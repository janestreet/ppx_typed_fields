open! Base
open Ppxlib

let gen_sig_t ~loc ~params =
  let open (val Syntax.builder loc) in
  let unique_id =
    Type_kind.generate_unique_id (Type_kind.generate_core_type_params params)
  in
  let t_params = params @ [ ptyp_var unique_id, (NoVariance, NoInjectivity) ] in
  psig_type
    Nonrecursive
    [ type_declaration
        ~name:(Located.mk "t")
        ~params:t_params
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:None
    ]
;;

let deriving_compare_equals_attribute ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "deriving")
    ~payload:
      (PStr
         [ pstr_eval
             (pexp_tuple
                [ None, [%expr compare ~localize]; None, [%expr equal ~localize] ])
             []
         ])
;;

let generate_packed_with_value_type ~loc ~params ~core_type_params ~unique_parameter_id =
  let open (val Syntax.builder loc) in
  type_declaration
    ~name:(Located.mk "packed_with_value")
    ~params
    ~cstrs:[]
    ~private_:Public
    ~manifest:None
    ~kind:
      (Ptype_variant
         [ constructor_declaration
             ~name:(Located.mk "T")
             ~args:
               (Pcstr_tuple
                  ([ ptyp_constr
                       (Lident "t" |> Located.mk)
                       (core_type_params @ [ ptyp_var unique_parameter_id ])
                   ; ptyp_var unique_parameter_id
                   ]
                   |> List.map ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.of_core_type))
             ~res:
               (Some
                  (ptyp_constr
                     (Lident "packed_with_value" |> Located.mk)
                     core_type_params))
         ])
;;

(** Generates a partial signature without the upper level t and without the upper level
    variant. *)
let gen_partial_sig ~loc ~params =
  let open (val Syntax.builder loc) in
  let unique_parameter_id =
    Type_kind.generate_unique_id (Type_kind.generate_core_type_params params)
  in
  let core_type_params = Type_kind.generate_core_type_params params in
  let t_params = core_type_params @ [ ptyp_var unique_parameter_id ] in
  let t_type_constr = ptyp_constr (Lident "t" |> Located.mk) t_params in
  let variant_type_constr =
    ptyp_constr (Lident Names.derived_on_name |> Located.mk) core_type_params
  in
  let unique_parameter_type_var = ptyp_var unique_parameter_id in
  let names = [%sigi: val names : string list] in
  let name = [%sigi: val name : [%t t_type_constr] @ local -> string] in
  let path = [%sigi: val path : [%t t_type_constr] @ local -> string list] in
  let ord = [%sigi: val __ord : [%t t_type_constr] @ local -> int list] in
  let get =
    [%sigi:
      val get
        :  [%t t_type_constr] @ local
        -> [%t variant_type_constr]
        -> [%t unique_parameter_type_var] option]
  in
  let create =
    [%sigi:
      val create
        :  [%t t_type_constr] @ local
        -> [%t unique_parameter_type_var]
        -> [%t variant_type_constr]]
  in
  let globalize0 =
    [%sigi: val globalize0 : [%t t_type_constr] @ local -> [%t t_type_constr]]
  in
  let type_ids =
    let signature =
      let t_type =
        ptyp_constr
          (Lident "t" |> Located.mk)
          (List.mapi core_type_params ~f:(fun index _ ->
             ptyp_constr
               (Ldot (Lident [%string "T%{(index + 1)#Int}"], "t") |> Located.mk)
               [])
           @ [ unique_parameter_type_var ])
      in
      pmty_signature
        (signature
           [ [%sigi:
               val type_id
                 :  [%t t_type] @ local
                 -> [%t unique_parameter_type_var] Base.Type_equal.Id.t]
           ])
    in
    let number_of_parameters = List.length core_type_params in
    let signature_with_functors =
      List.foldi core_type_params ~init:signature ~f:(fun index acc _ ->
        pmty_functor
          (Named
             ( Some [%string "T%{(number_of_parameters - index)#Int}"] |> Located.mk
             , pmty_ident (Ldot (Lident "Base", "T") |> Located.mk)
             , [] )
           |> Ppxlib_jane.Shim.Functor_parameter.to_parsetree)
          acc)
    in
    psig_module
      (module_declaration (Some "Type_ids" |> Located.mk) signature_with_functors)
  in
  let packed =
    let signature =
      let field_type_declaration =
        let td =
          Typed_deriver.generate_packed_field_type_declaration
            ~loc
            ~params
            ~unique_parameter_id
            ~t_type_constr
        in
        psig_type Recursive [ td ]
      in
      let field_type = ptyp_constr (Lident "field" |> Located.mk) t_params in
      let t_prime_type_declaration =
        let td =
          Typed_deriver.generate_packed_t_prime_type_declaration
            ~loc
            ~params
            ~core_type_params
            ~field_type
        in
        let td =
          { td with
            ptype_attributes =
              Typed_deriver.disable_warning_37 ~loc :: td.ptype_attributes
          }
        in
        psig_type Recursive [ td ]
      in
      let t_type_declaration =
        let td =
          Typed_deriver.generate_packed_t_type_declaration ~loc ~core_type_params
        in
        let td =
          { td with
            ptype_attributes =
              deriving_compare_equals_attribute ~loc :: td.ptype_attributes
          }
        in
        psig_type Recursive [ td ]
      in
      let sexp_of_t = [%sigi: val sexp_of_t : t -> Sexplib.Sexp.t] in
      let sexp_of_t__local =
        [%sigi: val sexp_of_t__local : t @ local -> Sexplib.Sexp.t @ local]
      in
      let t_of_sexp = [%sigi: val t_of_sexp : Sexplib.Sexp.t -> t] in
      let all = [%sigi: val all : t list] in
      let pack =
        let field_type_constr = ptyp_constr (Lident "field" |> Located.mk) t_params in
        [%sigi: val pack : [%t field_type_constr] -> t]
      in
      let pack__local =
        let field_type_constr = ptyp_constr (Lident "field" |> Located.mk) t_params in
        [%sigi: val pack__local : [%t field_type_constr] @ local -> t @ local]
      in
      pmty_signature
        (signature
           [ field_type_declaration
           ; t_prime_type_declaration
           ; t_type_declaration
           ; pack
           ; pack__local
           ; sexp_of_t
           ; sexp_of_t__local
           ; t_of_sexp
           ; all
           ])
    in
    psig_module (module_declaration (Some "Packed" |> Located.mk) signature)
  in
  let which = [%sigi: val which : [%t variant_type_constr] -> Packed.t] in
  [ name; path; ord; get; create; type_ids; globalize0; packed; which; names ]
;;

(** Either generates either `include Typed_variants_lib.SN with type original := original`
    or the fully generated partial signature if the number of parameter is above 5. *)
let generate_include_signature_for_opaque ~loc ~params =
  match List.length params with
  | 0 -> [ [%sigi: include Typed_variants_lib.S with type derived_on := derived_on] ]
  | 1 ->
    [ [%sigi: include Typed_variants_lib.S1 with type 't1 derived_on := 't1 derived_on] ]
  | 2 ->
    [ [%sigi:
        include
          Typed_variants_lib.S2 with type ('t1, 't2) derived_on := ('t1, 't2) derived_on]
    ]
  | 3 ->
    [ [%sigi:
        include
          Typed_variants_lib.S3
          with type ('t1, 't2, 't3) derived_on := ('t1, 't2, 't3) derived_on]
    ]
  | 4 ->
    [ [%sigi:
        include
          Typed_variants_lib.S4
          with type ('t1, 't2, 't3, 't4) derived_on := ('t1, 't2, 't3, 't4) derived_on]
    ]
  | 5 ->
    [ [%sigi:
        include
          Typed_variants_lib.S5
          with type ('t1, 't2, 't3, 't4, 't5) derived_on :=
            ('t1, 't2, 't3, 't4, 't5) derived_on]
    ]
  | _ -> [ gen_sig_t ~loc ~params ] @ gen_partial_sig ~loc ~params
;;

let generate_include_signature ~loc ~params =
  match List.length params with
  | 0 ->
    [ [%sigi:
        include
          Typed_variants_lib.S with type 'a t := 'a t and type derived_on := derived_on]
    ]
  | 1 ->
    [ [%sigi:
        include
          Typed_variants_lib.S1
          with type ('t1, 'a) t := ('t1, 'a) t
           and type 't1 derived_on := 't1 derived_on]
    ]
  | 2 ->
    [ [%sigi:
        include
          Typed_variants_lib.S2
          with type ('t1, 't2, 'a) t := ('t1, 't2, 'a) t
           and type ('t1, 't2) derived_on := ('t1, 't2) derived_on]
    ]
  | 3 ->
    [ [%sigi:
        include
          Typed_variants_lib.S3
          with type ('t1, 't2, 't3, 'a) t := ('t1, 't2, 't3, 'a) t
           and type ('t1, 't2, 't3) derived_on := ('t1, 't2, 't3) derived_on]
    ]
  | 4 ->
    [ [%sigi:
        include
          Typed_variants_lib.S4
          with type ('t1, 't2, 't3, 't4, 'a) t := ('t1, 't2, 't3, 't4, 'a) t
           and type ('t1, 't2, 't3, 't4) derived_on := ('t1, 't2, 't3, 't4) derived_on]
    ]
  | 5 ->
    [ [%sigi:
        include
          Typed_variants_lib.S5
          with type ('t1, 't2, 't3, 't4, 't5, 'a) t := ('t1, 't2, 't3, 't4, 't5, 'a) t
           and type ('t1, 't2, 't3, 't4, 't5) derived_on :=
            ('t1, 't2, 't3, 't4, 't5) derived_on]
    ]
  | _ -> gen_partial_sig ~loc ~params
;;

(* Disables unused type warning. *)
let disable_warning_34 ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-34") [] ])
;;

let typed_fields_attribute ~loc =
  let open (val Syntax.builder loc) in
  attribute
    ~name:(Located.mk "deriving")
    ~payload:(PStr [ pstr_eval (pexp_ident (Lident "typed_fields" |> Located.mk)) [] ])
;;

(** Generates a list of type declarations for for each anonymous record in the
    constructor. *)
let generate_anonymous_record_type_declarations ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  List.filter_map elements_to_convert ~f:(fun element ->
    match (element : Variant_kind_generator.supported_constructor_declaration) with
    | Single_value_constructor _ | Tuple_values_constructor _ | No_values_constructor _ ->
      None
    | Anonymous_record_constructor
        { constructor_name
        ; minimum_needed_parameters
        ; label_declarations
        ; typed_fields
        ; _
        } ->
      let td =
        type_declaration
          ~name:(constructor_name |> String.lowercase |> Located.mk)
          ~params:minimum_needed_parameters
          ~cstrs:[]
          ~private_:Public
          ~manifest:None
          ~kind:(Ptype_record label_declarations)
      in
      let attributes = if typed_fields then [ typed_fields_attribute ~loc ] else [] in
      let td = { td with ptype_attributes = attributes } in
      Some td)
;;

module Name_and_arity = struct
  module T = struct
    type t =
      { name : string
      ; arity : int
      }
    [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

let sanitize_type_declarations ~loc (type_declarations : type_declaration list) =
  let open (val Syntax.builder loc) in
  let names_taken_by_constructors =
    List.fold
      type_declarations
      ~init:(Set.empty (module String))
      ~f:(fun acc td -> Set.add acc td.ptype_name.txt)
  in
  let types_that_need_a_different_name =
    let find_duplicates =
      object
        inherit [Set.M(Name_and_arity).t] Ast_traverse.fold as super

        method! core_type ctype acc =
          let acc =
            match ctype.ptyp_desc with
            | Ptyp_constr ({ txt = Lident name; _ }, ctype_list)
              when Set.mem names_taken_by_constructors name ->
              let arity = List.length ctype_list in
              Set.add acc { name; arity }
            | _ -> acc
          in
          super#core_type ctype acc
      end
    in
    List.fold
      type_declarations
      ~init:(Set.empty (module Name_and_arity))
      ~f:(fun acc td -> find_duplicates#type_declaration td acc)
  in
  let all_taken_names =
    let find_all_taken_names =
      object
        inherit [Set.M(String).t] Ast_traverse.fold as super
        method! string s acc = super#string s (Set.add acc s)
      end
    in
    List.fold
      type_declarations
      ~init:(Set.empty (module String))
      ~f:(fun acc type_declaration ->
        find_all_taken_names#type_declaration type_declaration acc)
  in
  let rec find_safe_name name taken_names =
    match Set.mem taken_names name with
    | true -> find_safe_name (name ^ "_") taken_names
    | false -> name
  in
  let unsafe_name_to_safe_name, _ =
    Set.fold
      types_that_need_a_different_name
      ~init:(Map.empty (module Name_and_arity), all_taken_names)
      ~f:(fun (mapping, taken) ({ name; _ } as key) ->
        let safe_name = find_safe_name name taken in
        let mapping' = Map.set mapping ~key ~data:safe_name in
        let taken' = Set.add taken safe_name in
        mapping', taken')
  in
  let rename_type_declarations =
    Map.fold unsafe_name_to_safe_name ~init:[] ~f:(fun ~key ~data:safe_name acc ->
      let params =
        List.init key.arity ~f:(fun i ->
          ptyp_var ("t" ^ Int.to_string i), (NoVariance, NoInjectivity))
      in
      let manifest =
        ptyp_constr (Lident key.name |> Located.mk) (List.map params ~f:fst)
      in
      let new_td =
        type_declaration
          ~name:(Located.mk safe_name)
          ~params
          ~cstrs:[]
          ~kind:Ptype_abstract
          ~private_:Public
          ~manifest:(Some manifest)
      in
      new_td :: acc)
  in
  let type_declarations_with_updated_names =
    let update_names =
      object
        inherit Ast_traverse.map as super

        method! core_type ctype =
          let ctype =
            match ctype.ptyp_desc with
            | Ptyp_constr ({ txt = Lident name; loc }, ctype_list) ->
              let arity = List.length ctype_list in
              (match Map.find unsafe_name_to_safe_name { name; arity } with
               | None -> ctype
               | Some name -> ptyp_constr { txt = Lident name; loc } ctype_list)
            | _ -> ctype
          in
          super#core_type ctype
      end
    in
    List.map type_declarations ~f:update_names#type_declaration
  in
  rename_type_declarations @ type_declarations_with_updated_names
;;

(** Generates the anonymous records and gives them a concrete name. e.g.

    [ type rgb = { r : int; g : int; b: int} type 'x rgbx = { r : int; g : int; b: int; x : 'x} ] *)
let generate_anonymous_records_sig ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let tds = generate_anonymous_record_type_declarations ~loc ~elements_to_convert in
  let sanitized = sanitize_type_declarations ~loc tds in
  List.map sanitized ~f:(fun td -> psig_type Recursive [ td ])
;;

(** Generates the anonymous records and gives them a concrete name. e.g.

    [ type rgb = { r : int; g : int; b: int} type 'x rgbx = { r : int; g : int; b: int; x : 'x} ] *)
let generate_anonymous_records_str ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let tds = generate_anonymous_record_type_declarations ~loc ~elements_to_convert in
  let sanitized = sanitize_type_declarations ~loc tds in
  List.map sanitized ~f:(fun td -> pstr_type Recursive [ td ])
;;

(** Generates a list of type declarations for for each tuple in the constructor. *)
let generate_tuple_type_declarations ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  List.filter_map elements_to_convert ~f:(fun element ->
    match (element : Variant_kind_generator.supported_constructor_declaration) with
    | Single_value_constructor
        { return_value_type = { ptyp_desc = Ptyp_tuple _; _ } as return_value_type
        ; minimum_needed_parameters
        ; constructor_name
        ; typed_fields
        ; _
        }
    | Tuple_values_constructor
        { return_value_type
        ; minimum_needed_parameters
        ; constructor_name
        ; typed_fields
        ; _
        } ->
      let name = constructor_name |> String.lowercase |> Located.mk in
      let td =
        type_declaration
          ~name
          ~params:minimum_needed_parameters
          ~cstrs:[]
          ~private_:Public
          ~manifest:(Some return_value_type)
          ~kind:Ptype_abstract
      in
      let attributes =
        if typed_fields
        then [ typed_fields_attribute ~loc; disable_warning_34 ~loc ]
        else [ disable_warning_34 ~loc ]
      in
      let td = { td with ptype_attributes = attributes } in
      Some td
    | Single_value_constructor _
    | No_values_constructor _
    | Anonymous_record_constructor _ -> None)
;;

(** Generates the tuples module and gives them a concrete name. e.g. (Also attaches
    [@@deriving typed_fields] if needed.)

    {[
      type rgb = int * int * string
      type 'x rgbx = 'x * float * 'x [@@deriving typed_fields]
    ]} *)
let generate_tuples_sig ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let tds = generate_tuple_type_declarations ~loc ~elements_to_convert in
  let sanitized = sanitize_type_declarations ~loc tds in
  List.map sanitized ~f:(fun td -> psig_type Recursive [ td ])
;;

(** Generates the tuples module and gives them a concrete name. e.g. (Also attaches
    [@@deriving typed_fields] if needed.)

    {[
      type rgb = int * int * string
      type 'x rgbx = 'x * float * 'x [@@deriving typed_fields]
    ]} *)
let generate_tuples_str ~loc ~elements_to_convert =
  let open (val Syntax.builder loc) in
  let tds = generate_tuple_type_declarations ~loc ~elements_to_convert in
  let sanitized = sanitize_type_declarations ~loc tds in
  List.map sanitized ~f:(fun td -> pstr_type Recursive [ td ])
;;

let generate_str_body
  (module Specific_generator : Variant_kind_generator.S)
  ~original_type
  ~original_kind
  ~loc
  ~elements_to_convert
  ~params
  =
  let open (val Syntax.builder loc) in
  let elements_to_convert =
    List.map elements_to_convert ~f:(fun el -> el, Type_kind.Shallow)
  in
  let ({ gadt_t = t; upper; constructor_declarations; internal_gadt_rename }
        : Variant_kind_generator.supported_constructor_declaration Type_kind.gen_t_result)
    =
    Generic_generator.gen_t
      ~loc
      ~generate_constructors:Specific_generator.generate_constructor_declarations
      ~original_type
      ~original_kind
      ~elements_to_convert
      ~params
      ~upper_name:"typed_common_original"
  in
  let upper = pstr_type Nonrecursive [ upper ] in
  let t = pstr_type Recursive [ t ] in
  let core_type_params = Type_kind.generate_core_type_params params in
  let unique_parameter_id = Type_kind.generate_unique_id core_type_params in
  let names =
    let names = Specific_generator.names_list ~loc ~elements_to_convert in
    [%stri let names = [%e names]]
  in
  let name =
    let function_body = Specific_generator.name_function_body ~loc ~elements_to_convert in
    let arrow_type = ptyp_constr (Lident "string" |> Located.mk) [] in
    Typed_deriver.generate_new_typed_function
      ~loc
      ~function_name:"name"
      ~core_type_params
      ~unique_parameter_id
      ~arg_modes:Ppxlib_jane.Shim.Modes.local
      ~constr_arrow_type:arrow_type
      ~var_arrow_type:arrow_type
      ~function_body
      ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
      ()
  in
  let path =
    let function_body = Specific_generator.path_function_body ~loc ~elements_to_convert in
    let arrow_type =
      ptyp_constr
        (Lident "list" |> Located.mk)
        [ ptyp_constr (Lident "string" |> Located.mk) [] ]
    in
    Typed_deriver.generate_new_typed_function
      ~loc
      ~function_name:"path"
      ~core_type_params
      ~unique_parameter_id
      ~arg_modes:Ppxlib_jane.Shim.Modes.local
      ~constr_arrow_type:arrow_type
      ~var_arrow_type:arrow_type
      ~function_body
      ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
      ()
  in
  let ord =
    let function_body = Specific_generator.ord_function_body ~loc ~elements_to_convert in
    let arrow_type =
      ptyp_constr
        (Lident "list" |> Located.mk)
        [ ptyp_constr (Lident "int" |> Located.mk) [] ]
    in
    Typed_deriver.generate_new_typed_function
      ~loc
      ~function_name:"__ord"
      ~core_type_params
      ~unique_parameter_id
      ~arg_modes:Ppxlib_jane.Shim.Modes.local
      ~constr_arrow_type:arrow_type
      ~var_arrow_type:arrow_type
      ~function_body
      ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
      ()
  in
  let constr_variant_type =
    ptyp_constr
      (Lident Names.derived_on_name |> Located.mk)
      (List.filter_map core_type_params ~f:(fun { ptyp_desc; _ } ->
         match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
         | Ptyp_var (name, _) -> Some (ptyp_constr (Lident name |> Located.mk) [])
         | _ -> None))
  in
  let var_variant_type =
    ptyp_constr (Lident Names.derived_on_name |> Located.mk) core_type_params
  in
  let get =
    let function_body = Specific_generator.get_function_body ~loc ~elements_to_convert in
    Typed_deriver.generate_new_typed_function
      ~loc
      ~function_name:"get"
      ~core_type_params
      ~unique_parameter_id
      ~arg_modes:Ppxlib_jane.Shim.Modes.local
      ~constr_arrow_type:
        (ptyp_arrow
           { arg_label = Nolabel; arg_type = constr_variant_type; arg_modes = [] }
           { result_type =
               ptyp_constr
                 (Lident "option" |> Located.mk)
                 [ ptyp_constr (Lident unique_parameter_id |> Located.mk) [] ]
           ; result_modes = []
           })
      ~var_arrow_type:
        (ptyp_arrow
           { arg_label = Nolabel; arg_type = var_variant_type; arg_modes = [] }
           { result_type =
               ptyp_constr
                 (Lident "option" |> Located.mk)
                 [ ptyp_var unique_parameter_id ]
           ; result_modes = []
           })
      ~function_body
      ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
      ()
  in
  let create =
    let function_body =
      Specific_generator.create_function_body ~loc ~constructor_declarations ~local:false
    in
    Typed_deriver.generate_new_typed_function
      ~loc
      ~function_name:"create"
      ~core_type_params
      ~unique_parameter_id
      ~arg_modes:Ppxlib_jane.Shim.Modes.local
      ~constr_arrow_type:
        (ptyp_arrow
           { arg_label = Nolabel
           ; arg_type = ptyp_constr (Lident unique_parameter_id |> Located.mk) []
           ; arg_modes = []
           }
           { result_type = constr_variant_type; result_modes = [] })
      ~var_arrow_type:
        (ptyp_arrow
           { arg_label = Nolabel
           ; arg_type = ptyp_var unique_parameter_id
           ; arg_modes = []
           }
           { result_type = var_variant_type; result_modes = [] })
      ~function_body
      ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
      ()
  in
  let type_ids =
    let type_ids =
      Specific_generator.type_ids ~loc ~elements_to_convert ~core_type_params
    in
    let type_id =
      let function_body =
        Specific_generator.type_id_function_body ~loc ~elements_to_convert
      in
      let type_equal_t =
        List.fold
          [ "Type_equal"; "Id"; "t" ]
          ~init:(Lident "Base")
          ~f:(fun acc new_label -> Ldot (acc, new_label))
        |> Located.mk
      in
      Typed_deriver.generate_new_typed_function
        ~loc
        ~function_name:"type_id"
        ~core_type_params:
          (List.init (List.length core_type_params) ~f:(fun index ->
             ptyp_constr
               (Ldot (Lident [%string "T%{(index + 1)#Int}"], "t") |> Located.mk)
               []))
        ~unique_parameter_id
        ~function_body
        ~arg_modes:Ppxlib_jane.Shim.Modes.local
        ~constr_arrow_type:
          (ptyp_constr
             type_equal_t
             [ ptyp_constr (Lident unique_parameter_id |> Located.mk) [] ])
        ~var_arrow_type:(ptyp_constr type_equal_t [ ptyp_var unique_parameter_id ])
        ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
        ()
    in
    let number_of_parameters = List.length core_type_params in
    let functor_expression =
      List.foldi
        core_type_params
        ~init:(pmod_structure (type_ids @ [ type_id ]))
        ~f:(fun index acc _ ->
          pmod_functor
            (Named
               ( Some [%string "T%{(number_of_parameters - index)#Int}"] |> Located.mk
               , pmty_ident (Ldot (Lident "Base", "T") |> Located.mk)
               , [] )
             |> Ppxlib_jane.Shim.Functor_parameter.to_parsetree)
            acc)
    in
    [%stri module Type_ids = [%m functor_expression]]
  in
  let t_params = core_type_params @ [ ptyp_var unique_parameter_id ] in
  let t_type_constr =
    ptyp_constr (Lident Type_kind.internal_gadt_name |> Located.mk) t_params
  in
  let field_type = ptyp_constr (Lident "field" |> Located.mk) t_params in
  let globalize0 =
    let var_arrow_type = t_type_constr in
    let constr_arrow_type =
      ptyp_constr
        (Lident Type_kind.internal_gadt_name |> Located.mk)
        (List.map t_params ~f:(fun core_type ->
           match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
           | Ptyp_var (name, _) ->
             { core_type with ptyp_desc = Ptyp_constr (Located.mk (Lident name), []) }
           | _ -> core_type))
    in
    Typed_deriver.generate_new_typed_function
      ~loc
      ~function_name:"globalize0"
      ~core_type_params
      ~unique_parameter_id
      ~arg_modes:Ppxlib_jane.Shim.Modes.local
      ~result_modes:[]
      ~var_arrow_type
      ~constr_arrow_type
      ~function_body:
        (Specific_generator.globalize0_function_body ~loc ~elements_to_convert)
      ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
      ()
  in
  let globalize =
    let body =
      eabstract
        (List.map t_params ~f:(fun _ -> ppat_any))
        [%expr fun (t @ local) -> globalize0 t]
    in
    [%stri let globalize = [%e body]]
  in
  let packed =
    let packed_field =
      let td =
        Typed_deriver.generate_packed_field_type_declaration
          ~loc
          ~params
          ~unique_parameter_id
          ~t_type_constr
      in
      pstr_type Recursive [ td ]
    in
    let t_prime_type_declaration =
      let td =
        Typed_deriver.generate_packed_t_prime_type_declaration
          ~loc
          ~params
          ~core_type_params
          ~field_type
      in
      let td = { td with ptype_attributes = [ Typed_deriver.disable_warning_37 ~loc ] } in
      pstr_type Recursive [ td ]
    in
    let t_type_declaration =
      let td = Typed_deriver.generate_packed_t_type_declaration ~loc ~core_type_params in
      pstr_type Recursive [ td ]
    in
    let all =
      let all_list_expression =
        Specific_generator.all_body ~loc ~constructor_declarations
      in
      [%stri let all = [%e all_list_expression]]
    in
    let compare =
      [%stri
        let compare { f = T x1 } { f = T x2 } =
          Base.List.compare Base.Int.compare (__ord x1) (__ord x2)
        ;;]
    in
    let compare__local =
      [%stri
        let compare__local ({ f = T x1 } @ local) ({ f = T x2 } @ local) =
          Base.List.compare__local Base.Int.compare__local (__ord x1) (__ord x2)
        ;;]
    in
    let equal =
      [%stri let equal packed_1 packed_2 = compare packed_1 packed_2 |> Base.Int.equal 0]
    in
    let equal__local =
      [%stri
        let equal__local (packed_1 @ local) (packed_2 @ local) =
          compare__local packed_1 packed_2 |> Base.Int.equal 0
        ;;]
    in
    let pack ~local =
      let function_body = Specific_generator.pack_body ~loc ~elements_to_convert ~local in
      let arrow_type = ptyp_constr (Lident "t" |> Located.mk) [] in
      let modes = if local then Ppxlib_jane.Shim.Modes.local else [] in
      Typed_deriver.generate_new_typed_function
        ~loc
        ~function_name:(Names.localize "pack" ~local)
        ~core_type_params
        ~unique_parameter_id
        ~arg_modes:modes
        ~result_modes:modes
        ~constr_arrow_type:arrow_type
        ~var_arrow_type:arrow_type
        ~function_body
        ~name_of_first_parameter:(Lident "field")
        ()
    in
    let globalize_packed =
      [%stri
        let globalize : t @ local -> t =
          [%e Specific_generator.globalize_packed_function_body ~loc ~elements_to_convert]
        ;;]
    in
    let sexp_of_packed ~local =
      let function_body =
        Specific_generator.sexp_of_t_body ~loc ~elements_to_convert ~local
      in
      let name = Names.localize "sexp_of_t" ~local in
      let pat =
        match local with
        | false -> [%pat? packed]
        | true -> ppat_constraint [%pat? packed] None Ppxlib_jane.Shim.Modes.local
      in
      [%stri let [%p pvar name] = fun [%p pat] -> [%e function_body]]
    in
    let packed_of_sexp =
      let function_body = Specific_generator.t_of_sexp_body ~loc ~elements_to_convert in
      [%stri let t_of_sexp sexp = [%e function_body]]
    in
    let comparator =
      [%stri
        include Base.Comparator.Make (struct
            type nonrec t = t

            let compare = compare
            let sexp_of_t = sexp_of_t
          end)]
    in
    pstr_module
      (module_binding
         ~name:(Some "Packed" |> Located.mk)
         ~expr:
           (pmod_structure
              [ packed_field
              ; t_prime_type_declaration
              ; t_type_declaration
              ; all
              ; compare
              ; compare__local
              ; equal
              ; equal__local
              ; pack ~local:false
              ; pack ~local:true
              ; globalize_packed
              ; sexp_of_packed ~local:false
              ; sexp_of_packed ~local:true
              ; packed_of_sexp
              ; comparator
              ]))
  in
  let which =
    let which_function_body =
      Specific_generator.which_function_body
        ~loc
        ~elements_to_convert
        ~number_of_params:(List.length params)
    in
    [%stri let which : [%t var_variant_type] -> Packed.t = [%e which_function_body]]
  in
  let upper_rename =
    let td =
      type_declaration
        ~name:(Located.mk Names.derived_on_name)
        ~params
        ~cstrs:[]
        ~private_:Public
        ~kind:Ptype_abstract
        ~manifest:
          (Some
             (ptyp_constr (Lident "typed_common_original" |> Located.mk) core_type_params))
    in
    pstr_type Recursive [ td ]
  in
  let internal_gadt_rename = pstr_type Recursive [ internal_gadt_rename ] in
  [ upper; t; upper_rename ]
  @ Specific_generator.extra_structure_items_to_insert loc
  @ [ path
    ; name
    ; ord
    ; get
    ; create
    ; type_ids
    ; globalize0
    ; globalize
    ; packed
    ; which
    ; names
    ; internal_gadt_rename
    ]
;;

let generate_anonymous_record_str ~loc td_case =
  let open (val Syntax.builder loc) in
  let td_str_items =
    match (td_case : Variant_kind_generator.type_case) with
    | Opaque _ | Unknown | Nothing _ -> []
    | Variant (constructors_to_convert, _) ->
      generate_anonymous_records_str ~loc ~elements_to_convert:constructors_to_convert
  in
  pstr_module
    (module_binding
       ~name:(Some "Typed_variant_anonymous_records" |> Located.mk)
       ~expr:(pmod_structure td_str_items))
;;

(** Generates a structure with the two submodules, Shallow, and Deep and exposes the
    deepest version of Deep. *)
let gen_str
  (module Specific_generator : Variant_kind_generator.S)
  ~original_type
  ~original_kind
  ~loc
  ~elements_to_convert
  ~expand_typed_variants
  ~params
  ~td_case
  =
  let open (val Syntax.builder loc) in
  let anonymous_record_module = generate_anonymous_record_str ~loc td_case in
  let tuples_module =
    let type_items = generate_tuples_str ~loc ~elements_to_convert in
    pstr_module
      (module_binding
         ~name:(Some "Typed_variant_tuples" |> Located.mk)
         ~expr:(pmod_structure type_items))
  in
  let singleton_modules =
    Specific_generator.singleton_modules_structures ~loc ~elements_to_convert
  in
  let shallow_module =
    let expr =
      List.fold
        elements_to_convert
        ~init:(pmod_ident (Lident "Deep" |> Located.mk))
        ~f:(fun acc element ->
          match element with
          | Single_value_constructor { granularity = Constr_deep _; _ }
          | Single_value_constructor { granularity = Polymorphic_deep; _ } ->
            let name =
              Variant_kind_generator.supported_constructor_name element
              |> String.lowercase
            in
            let singleton_name = "Singleton_for_" ^ name in
            pmod_apply acc (pmod_ident (Lident singleton_name |> Located.mk))
          | _ -> acc)
    in
    pstr_module (module_binding ~name:(Some "Shallow" |> Located.mk) ~expr)
  in
  let deep_module =
    let deep_contents =
      pmod_structure
        (generate_str_body
           (module Specific_generator)
           ~original_type
           ~original_kind
           ~loc
           ~elements_to_convert
           ~params)
    in
    Specific_generator.deep_functor_structure
      ~loc
      ~elements_to_convert
      ~module_expression:deep_contents
  in
  let full_depth =
    Specific_generator.full_depth_module ~loc ~elements_to_convert ~expand_typed_variants
  in
  [ anonymous_record_module; tuples_module; deep_module ]
  @ singleton_modules
  @ [ shallow_module ]
  @ full_depth
;;
