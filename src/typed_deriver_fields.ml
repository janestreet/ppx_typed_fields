open Base
open Import
open Ppxlib
open Type_kind_intf
open Typed_deriver_intf

let gen_sig_t ~loc ~params =
  let open (val Ast_builder.make loc) in
  let unique_id = generate_unique_id (generate_core_type_params params) in
  let t_params = params @ [ ptyp_var unique_id, (NoVariance, NoInjectivity) ] in
  let t =
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
  in
  [ t ]
;;

let deriving_compare_equals_attribute ~loc =
  let open (val Ast_builder.make loc) in
  attribute
    ~name:(Located.mk "deriving")
    ~payload:(PStr [ pstr_eval (pexp_tuple [ [%expr compare]; [%expr equal] ]) [] ])
;;

(**
   Generates a partial signature without the upper level t and without the upper level
   record.
*)
let gen_partial_sig ~loc ~params ~t_name =
  let open (val Ast_builder.make loc) in
  let unique_parameter_id = generate_unique_id (generate_core_type_params params) in
  let core_type_params = generate_core_type_params params in
  let t_params = core_type_params @ [ ptyp_var unique_parameter_id ] in
  let t_type_constr = ptyp_constr (Lident "t" |> Located.mk) t_params in
  let record_type_constr =
    ptyp_constr (Lident derived_on_name |> Located.mk) core_type_params
  in
  let unique_parameter_type_var = ptyp_var unique_parameter_id in
  let creator =
    psig_type
      Recursive
      [ generate_creator_type_declaration
          ~loc
          ~unique_parameter_id
          ~core_type_params
          ~params
          ~t_name
      ]
  in
  let names = [%sigi: val names : string list] in
  let name = [%sigi: val name : [%t t_type_constr] -> string] in
  let path = [%sigi: val path : [%t t_type_constr] -> string list] in
  let ord = [%sigi: val __ord : [%t t_type_constr] -> int list] in
  let get =
    let get_type =
      generate_arrow_type
        ~loc
        ~types_before_last:[ t_type_constr; record_type_constr ]
        ~last_type:unique_parameter_type_var
    in
    [%sigi: val get : [%t get_type]]
  in
  let set =
    let set_type =
      generate_arrow_type
        ~loc
        ~types_before_last:
          [ t_type_constr; record_type_constr; unique_parameter_type_var ]
        ~last_type:record_type_constr
    in
    [%sigi: val set : [%t set_type]]
  in
  let creator_type_constr =
    ptyp_constr (Lident "creator" |> Located.mk) core_type_params
  in
  let create =
    let create_type =
      generate_arrow_type
        ~loc
        ~types_before_last:[ creator_type_constr ]
        ~last_type:record_type_constr
    in
    [%sigi: val create : [%t create_type]]
  in
  let create_local =
    [%sigi:
      val create_local : local_ [%t creator_type_constr] -> local_ [%t record_type_constr]]
  in
  let type_ids =
    let signature =
      let type_id_type =
        let t_type =
          ptyp_constr
            (Lident "t" |> Located.mk)
            (List.mapi core_type_params ~f:(fun index _ ->
               ptyp_constr
                 (Ldot (Lident [%string "T%{(index + 1)#Int}"], "t") |> Located.mk)
                 [])
             @ [ unique_parameter_type_var ])
        in
        generate_arrow_type
          ~loc
          ~types_before_last:[ t_type ]
          ~last_type:
            (ptyp_constr
               (Ldot (Ldot (Ldot (Lident "Base", "Type_equal"), "Id"), "t") |> Located.mk)
               [ unique_parameter_type_var ])
      in
      pmty_signature [ [%sigi: val type_id : [%t type_id_type]] ]
    in
    let number_of_parameters = List.length core_type_params in
    let signature_with_functors =
      List.foldi core_type_params ~init:signature ~f:(fun index acc _ ->
        pmty_functor
          (Named
             ( Some [%string "T%{(number_of_parameters - index)#Int}"] |> Located.mk
             , pmty_ident (Ldot (Lident "Base", "T") |> Located.mk) ))
          acc)
    in
    psig_module
      (module_declaration
         ~name:(Some "Type_ids" |> Located.mk)
         ~type_:signature_with_functors)
  in
  let packed =
    let signature =
      let field_type_declaration =
        let td =
          generate_packed_field_type_declaration
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
          generate_packed_t_prime_type_declaration
            ~loc
            ~params
            ~core_type_params
            ~field_type
        in
        let td =
          { td with ptype_attributes = disable_warning_37 ~loc :: td.ptype_attributes }
        in
        psig_type Recursive [ td ]
      in
      let t_type_declaration =
        let td = generate_packed_t_type_declaration ~loc ~core_type_params in
        let td =
          { td with
            ptype_attributes =
              deriving_compare_equals_attribute ~loc :: td.ptype_attributes
          }
        in
        psig_type Recursive [ td ]
      in
      let sexp_of_t = [%sigi: val sexp_of_t : t -> Sexplib.Sexp.t] in
      let t_of_sexp = [%sigi: val t_of_sexp : Sexplib.Sexp.t -> t] in
      let all = [%sigi: val all : t list] in
      let pack =
        let field_type_constr = ptyp_constr (Lident "field" |> Located.mk) t_params in
        [%sigi: val pack : [%t field_type_constr] -> t]
      in
      pmty_signature
        [ field_type_declaration
        ; t_prime_type_declaration
        ; t_type_declaration
        ; pack
        ; sexp_of_t
        ; t_of_sexp
        ; all
        ]
    in
    psig_module (module_declaration ~name:(Some "Packed" |> Located.mk) ~type_:signature)
  in
  [ creator; name; path; ord; get; set; create; create_local; type_ids; packed; names ]
;;

(**
   Either generates either
   `include Typed_fields_lib.SN with type original := original`
   or
   the fully generated partial signature if the number of parameter is above 5.
*)
let generate_include_signature_for_opaque ~loc ~params =
  let open (val Ast_builder.make loc) in
  match List.length params with
  | 0 -> [ [%sigi: include Typed_fields_lib.S with type derived_on := derived_on] ]
  | 1 ->
    [ [%sigi: include Typed_fields_lib.S1 with type 't1 derived_on := 't1 derived_on] ]
  | 2 ->
    [ [%sigi:
        include
          Typed_fields_lib.S2 with type ('t1, 't2) derived_on := ('t1, 't2) derived_on]
    ]
  | 3 ->
    [ [%sigi:
        include
          Typed_fields_lib.S3
          with type ('t1, 't2, 't3) derived_on := ('t1, 't2, 't3) derived_on]
    ]
  | 4 ->
    [ [%sigi:
        include
          Typed_fields_lib.S4
          with type ('t1, 't2, 't3, 't4) derived_on := ('t1, 't2, 't3, 't4) derived_on]
    ]
  | 5 ->
    [ [%sigi:
        include
          Typed_fields_lib.S5
          with type ('t1, 't2, 't3, 't4, 't5) derived_on :=
            ('t1, 't2, 't3, 't4, 't5) derived_on]
    ]
  | _ -> gen_sig_t ~loc ~params @ gen_partial_sig ~loc ~params ~t_name:"t"
;;

let generate_include_signature ~loc ~params =
  let open (val Ast_builder.make loc) in
  match List.length params with
  | 0 ->
    [ [%sigi:
        include
          Typed_fields_lib.S with type 'a t := 'a t and type derived_on := derived_on]
    ]
  | 1 ->
    [ [%sigi:
        include
          Typed_fields_lib.S1
          with type ('t1, 'a) t := ('t1, 'a) t
           and type 't1 derived_on := 't1 derived_on]
    ]
  | 2 ->
    [ [%sigi:
        include
          Typed_fields_lib.S2
          with type ('t1, 't2, 'a) t := ('t1, 't2, 'a) t
           and type ('t1, 't2) derived_on := ('t1, 't2) derived_on]
    ]
  | 3 ->
    [ [%sigi:
        include
          Typed_fields_lib.S3
          with type ('t1, 't2, 't3, 'a) t := ('t1, 't2, 't3, 'a) t
           and type ('t1, 't2, 't3) derived_on := ('t1, 't2, 't3) derived_on]
    ]
  | 4 ->
    [ [%sigi:
        include
          Typed_fields_lib.S4
          with type ('t1, 't2, 't3, 't4, 'a) t := ('t1, 't2, 't3, 't4, 'a) t
           and type ('t1, 't2, 't3, 't4) derived_on := ('t1, 't2, 't3, 't4) derived_on]
    ]
  | 5 ->
    [ [%sigi:
        include
          Typed_fields_lib.S5
          with type ('t1, 't2, 't3, 't4, 't5, 'a) t := ('t1, 't2, 't3, 't4, 't5, 'a) t
           and type ('t1, 't2, 't3, 't4, 't5) derived_on :=
            ('t1, 't2, 't3, 't4, 't5) derived_on]
    ]
  | _ -> gen_partial_sig ~loc ~params ~t_name:"t"
;;

let generate_str_body
  (type a)
  (module Specific_generator : Type_kind_intf.S with type t = a)
  ~original_type
  ~original_kind
  ~loc
  ~(elements_to_convert : (a * granularity) list)
  ~params
  =
  let open (val Ast_builder.make loc) in
  let ({ gadt_t = t; upper; constructor_declarations; internal_gadt_rename }
        : a gen_t_result)
    =
    Generic_generator.gen_t
      ~loc
      ~generate_constructors:Specific_generator.constructor_declarations
      ~original_type
      ~original_kind
      ~elements_to_convert
      ~params
      ~upper_name:"typed_common_original"
  in
  let upper = pstr_type Nonrecursive [ upper ] in
  let t = pstr_type Recursive [ t ] in
  let internal_gadt_rename = pstr_type Recursive [ internal_gadt_rename ] in
  let core_type_params = generate_core_type_params params in
  let unique_parameter_id = generate_unique_id core_type_params in
  let creator_type =
    pstr_type
      Recursive
      [ generate_creator_type_declaration
          ~loc
          ~unique_parameter_id
          ~core_type_params
          ~params
          ~t_name:internal_gadt_name
      ]
  in
  let names =
    let names = Specific_generator.names_list ~loc ~elements_to_convert in
    [%stri let names = [%e names]]
  in
  let name =
    let function_body = Specific_generator.name_function_body ~loc in
    let arrow_type = ptyp_constr (Lident "string" |> Located.mk) [] in
    generate_new_typed_function
      ~loc
      ~function_name:"name"
      ~core_type_params
      ~unique_parameter_id
      ~constr_arrow_type:arrow_type
      ~var_arrow_type:arrow_type
      ~function_body
      ~name_of_first_parameter:internal_gadt_name
  in
  let path =
    let function_body = Specific_generator.path_function_body ~loc ~elements_to_convert in
    let arrow_type =
      ptyp_constr
        (Lident "list" |> Located.mk)
        [ ptyp_constr (Lident "string" |> Located.mk) [] ]
    in
    generate_new_typed_function
      ~loc
      ~function_name:"path"
      ~core_type_params
      ~unique_parameter_id
      ~constr_arrow_type:arrow_type
      ~var_arrow_type:arrow_type
      ~function_body
      ~name_of_first_parameter:internal_gadt_name
  in
  let ord =
    let function_body = Specific_generator.ord_function_body ~loc ~elements_to_convert in
    let arrow_type =
      ptyp_constr
        (Lident "list" |> Located.mk)
        [ ptyp_constr (Lident "int" |> Located.mk) [] ]
    in
    generate_new_typed_function
      ~loc
      ~function_name:"__ord"
      ~core_type_params
      ~unique_parameter_id
      ~constr_arrow_type:arrow_type
      ~var_arrow_type:arrow_type
      ~function_body
      ~name_of_first_parameter:internal_gadt_name
  in
  let constr_record_type =
    ptyp_constr
      (Lident derived_on_name |> Located.mk)
      (List.filter_map core_type_params ~f:(fun { ptyp_desc; _ } ->
         match ptyp_desc with
         | Ptyp_var name -> Some (ptyp_constr (Lident name |> Located.mk) [])
         | _ -> None))
  in
  let var_record_type =
    ptyp_constr (Lident derived_on_name |> Located.mk) core_type_params
  in
  let get =
    let function_body = Specific_generator.get_function_body ~loc ~elements_to_convert in
    generate_new_typed_function
      ~loc
      ~function_name:"get"
      ~core_type_params
      ~unique_parameter_id
      ~constr_arrow_type:
        (ptyp_arrow
           Nolabel
           constr_record_type
           (ptyp_constr (Lident unique_parameter_id |> Located.mk) []))
      ~var_arrow_type:(ptyp_arrow Nolabel var_record_type (ptyp_var unique_parameter_id))
      ~function_body
      ~name_of_first_parameter:internal_gadt_name
  in
  let set =
    let function_body = Specific_generator.set_function_body ~loc ~elements_to_convert in
    generate_new_typed_function
      ~loc
      ~function_name:"set"
      ~core_type_params
      ~unique_parameter_id
      ~constr_arrow_type:
        (ptyp_arrow
           Nolabel
           constr_record_type
           (ptyp_arrow
              Nolabel
              (ptyp_constr (Lident unique_parameter_id |> Located.mk) [])
              constr_record_type))
      ~var_arrow_type:
        (ptyp_arrow
           Nolabel
           var_record_type
           (ptyp_arrow Nolabel (ptyp_var unique_parameter_id) var_record_type))
      ~function_body
      ~name_of_first_parameter:internal_gadt_name
  in
  let create =
    let body =
      Specific_generator.create_function_body ~loc ~constructor_declarations ~local:false
    in
    let creator_constr_type =
      ptyp_constr (Lident "creator" |> Located.mk) core_type_params
    in
    match constructor_declarations with
    | [] ->
      [%stri
        let create ({ f = _ } : [%t creator_constr_type]) : [%t var_record_type] =
          [%e body]
        ;;]
    | _ :: _ ->
      [%stri
        let create ({ f = __ppx_typed_fields_creator_f } : [%t creator_constr_type])
          : [%t var_record_type]
          =
          [%e body]
        ;;]
  in
  let create_local =
    let body =
      Specific_generator.create_function_body ~loc ~constructor_declarations ~local:true
    in
    let creator_constr_type =
      ptyp_constr (Lident "creator" |> Located.mk) core_type_params
    in
    match constructor_declarations with
    | [] ->
      [%stri
        let create_local (local_ ({ f = _ } : [%t creator_constr_type]))
          : [%t var_record_type]
          = exclave_
          [%e body]
        ;;]
    | _ :: _ ->
      [%stri
        let create_local
          (local_ ({ f = __ppx_typed_fields_creator_f } : [%t creator_constr_type]))
          : [%t var_record_type]
          = exclave_
          [%e body]
        ;;]
  in
  let type_ids =
    let type_ids =
      Specific_generator.type_ids ~loc ~elements_to_convert ~core_type_params
    in
    let subproduct_type_id_modules =
      Specific_generator.subproduct_type_id_modules
        ~loc
        ~elements_to_convert
        ~core_type_params
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
      generate_new_typed_function
        ~loc
        ~function_name:"type_id"
        ~core_type_params:
          (List.init (List.length core_type_params) ~f:(fun index ->
             ptyp_constr
               (Ldot (Lident [%string "T%{(index + 1)#Int}"], "t") |> Located.mk)
               []))
        ~unique_parameter_id
        ~function_body
        ~constr_arrow_type:
          (ptyp_constr
             type_equal_t
             [ ptyp_constr (Lident unique_parameter_id |> Located.mk) [] ])
        ~var_arrow_type:(ptyp_constr type_equal_t [ ptyp_var unique_parameter_id ])
        ~name_of_first_parameter:internal_gadt_name
    in
    let number_of_parameters = List.length core_type_params in
    let functor_expression =
      List.foldi
        core_type_params
        ~init:(pmod_structure (type_ids @ subproduct_type_id_modules @ [ type_id ]))
        ~f:(fun index acc _ ->
          pmod_functor
            (Named
               ( Some [%string "T%{(number_of_parameters - index)#Int}"] |> Located.mk
               , pmty_ident (Ldot (Lident "Base", "T") |> Located.mk) ))
            acc)
    in
    [%stri module Type_ids = [%m functor_expression]]
  in
  let t_params = core_type_params @ [ ptyp_var unique_parameter_id ] in
  let t_type_constr = ptyp_constr (Lident internal_gadt_name |> Located.mk) t_params in
  let field_type = ptyp_constr (Lident "field" |> Located.mk) t_params in
  let packed =
    let packed_field =
      let td =
        generate_packed_field_type_declaration
          ~loc
          ~params
          ~unique_parameter_id
          ~t_type_constr
      in
      pstr_type Recursive [ td ]
    in
    let t_prime_type_declaration =
      let td =
        generate_packed_t_prime_type_declaration
          ~loc
          ~params
          ~core_type_params
          ~field_type
      in
      let td = { td with ptype_attributes = [ disable_warning_37 ~loc ] } in
      pstr_type Recursive [ td ]
    in
    let t_type_declaration =
      let td = generate_packed_t_type_declaration ~loc ~core_type_params in
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
    let equal =
      [%stri let equal packed_1 packed_2 = Base.Int.equal 0 (compare packed_1 packed_2)]
    in
    let pack =
      let function_body = Specific_generator.pack_body ~loc ~elements_to_convert in
      let arrow_type = ptyp_constr (Lident "t" |> Located.mk) [] in
      generate_new_typed_function
        ~loc
        ~function_name:"pack"
        ~core_type_params
        ~unique_parameter_id
        ~constr_arrow_type:arrow_type
        ~var_arrow_type:arrow_type
        ~function_body
        ~name_of_first_parameter:"field"
    in
    let sexp_of_packed =
      let function_body = Specific_generator.sexp_of_t_body ~loc ~elements_to_convert in
      [%stri let sexp_of_t packed = [%e function_body]]
    in
    let packed_of_sexp =
      let function_body = Specific_generator.t_of_sexp_body ~loc ~elements_to_convert in
      [%stri let t_of_sexp sexp = [%e function_body]]
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
              ; equal
              ; pack
              ; sexp_of_packed
              ; packed_of_sexp
              ]))
  in
  let upper_rename =
    let td =
      type_declaration
        ~name:(Located.mk derived_on_name)
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
  [ upper; t; upper_rename ]
  @ Specific_generator.extra_structure_items_to_insert loc
  @ [ creator_type
    ; path
    ; name
    ; ord
    ; get
    ; set
    ; create
    ; create_local
    ; type_ids
    ; packed
    ; names
    ; internal_gadt_rename
    ]
;;

(**
   Generates a structure with the two submodules, Shallow, Deep and exposes the full depth
   version of Deep.
*)
let gen_str
  (type a)
  (module Specific_generator : Type_kind_intf.S with type t = a)
  ~original_type
  ~original_kind
  ~loc
  ~(elements_to_convert : (a * granularity) list)
  ~params
  =
  let open (val Ast_builder.make loc) in
  let shallow_module =
    let singleton_modules =
      Specific_generator.singleton_modules_structures ~loc ~elements_to_convert
    in
    let deep_application_expr =
      List.fold
        singleton_modules
        ~init:(pmod_ident (Lident "Deep" |> Located.mk))
        ~f:(fun acc (_, ident) ->
          pmod_apply acc (pmod_ident (Lident ident |> Located.mk)))
    in
    List.map singleton_modules ~f:(fun (f, _) -> f)
    @ [ pstr_module
          (module_binding
             ~name:(Some "Shallow" |> Located.mk)
             ~expr:deep_application_expr)
      ]
  in
  let deep_module =
    let module_expr =
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
    pstr_module (module_binding ~name:(Some "Deep" |> Located.mk) ~expr:module_expr)
  in
  let full_depth = Specific_generator.full_depth_module ~loc ~elements_to_convert in
  (deep_module :: shallow_module) @ full_depth
;;
