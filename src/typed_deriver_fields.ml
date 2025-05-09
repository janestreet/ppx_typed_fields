open! Base
open Ppxlib

let gen_sig_t ~loc ~params =
  let open (val Syntax.builder loc) in
  let unique_id =
    Type_kind.generate_unique_id (Type_kind.generate_core_type_params params)
  in
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

(** Generates a partial signature without the upper level t and without the upper level
    record. *)
let gen_partial_sig ~loc ~params ~t_name =
  let open (val Syntax.builder loc) in
  let unique_parameter_id =
    Type_kind.generate_unique_id (Type_kind.generate_core_type_params params)
  in
  let core_type_params = Type_kind.generate_core_type_params params in
  let t_params = core_type_params @ [ ptyp_var unique_parameter_id ] in
  let t_type_constr = ptyp_constr (Lident "t" |> Located.mk) t_params in
  let record_type_constr =
    ptyp_constr (Lident Names.derived_on_name |> Located.mk) core_type_params
  in
  let unique_parameter_type_var = ptyp_var unique_parameter_id in
  let creator =
    psig_type
      Recursive
      [ Type_kind.generate_creator_type_declaration
          ~loc
          ~unique_parameter_id
          ~core_type_params
          ~params
          ~t_name
      ]
  in
  let names = [%sigi: val names : string list] in
  let name = [%sigi: val name : [%t t_type_constr] @ local -> string] in
  let path = [%sigi: val path : [%t t_type_constr] @ local -> string list] in
  let ord = [%sigi: val __ord : [%t t_type_constr] @ local -> int list] in
  let get =
    [%sigi:
      val get
        :  [%t t_type_constr] @ local
        -> [%t record_type_constr]
        -> [%t unique_parameter_type_var]]
  in
  let set =
    [%sigi:
      val set
        :  [%t t_type_constr] @ local
        -> [%t record_type_constr]
        -> [%t unique_parameter_type_var]
        -> [%t record_type_constr]]
  in
  let globalize0 =
    [%sigi: val globalize0 : [%t t_type_constr] @ local -> [%t t_type_constr]]
  in
  let creator_type_constr =
    ptyp_constr (Lident "creator" |> Located.mk) core_type_params
  in
  let create =
    [%sigi: val create : [%t creator_type_constr] -> [%t record_type_constr]]
  in
  let create_local =
    [%sigi:
      val create_local : local_ [%t creator_type_constr] -> local_ [%t record_type_constr]]
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
  [ creator
  ; name
  ; path
  ; ord
  ; get
  ; set
  ; create
  ; create_local
  ; type_ids
  ; globalize0
  ; packed
  ; names
  ]
;;

(** Either generates either `include Typed_fields_lib.SN with type original := original`
    or the fully generated partial signature if the number of parameter is above 5. *)
let generate_include_signature_for_opaque ~loc ~params =
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
  (module Specific_generator : Type_kind.S with type t = a)
  ~original_type
  ~original_kind
  ~loc
  ~(elements_to_convert : (a * Type_kind.granularity) list)
  ~params
  =
  let open (val Syntax.builder loc) in
  let ({ gadt_t = t; upper; constructor_declarations; internal_gadt_rename }
        : a Type_kind.gen_t_result)
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
  let core_type_params = Type_kind.generate_core_type_params params in
  let unique_parameter_id = Type_kind.generate_unique_id core_type_params in
  let creator_type =
    pstr_type
      Recursive
      [ Type_kind.generate_creator_type_declaration
          ~loc
          ~unique_parameter_id
          ~core_type_params
          ~params
          ~t_name:Type_kind.internal_gadt_name
      ]
  in
  let names =
    let names = Specific_generator.names_list ~loc ~elements_to_convert in
    [%stri let names = [%e names]]
  in
  let name =
    let function_body = Specific_generator.name_function_body ~loc in
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
  let constr_record_type =
    ptyp_constr
      (Lident Names.derived_on_name |> Located.mk)
      (List.filter_map core_type_params ~f:(fun { ptyp_desc; _ } ->
         match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
         | Ptyp_var (name, _) -> Some (ptyp_constr (Lident name |> Located.mk) [])
         | _ -> None))
  in
  let var_record_type =
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
           { arg_label = Nolabel; arg_type = constr_record_type; arg_modes = [] }
           { result_type = ptyp_constr (Lident unique_parameter_id |> Located.mk) []
           ; result_modes = []
           })
      ~var_arrow_type:
        (ptyp_arrow
           { arg_label = Nolabel; arg_type = var_record_type; arg_modes = [] }
           { result_type = ptyp_var unique_parameter_id; result_modes = [] })
      ~function_body
      ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
      ()
  in
  let set =
    let function_body = Specific_generator.set_function_body ~loc ~elements_to_convert in
    Typed_deriver.generate_new_typed_function
      ~loc
      ~function_name:"set"
      ~core_type_params
      ~unique_parameter_id
      ~arg_modes:Ppxlib_jane.Shim.Modes.local
      ~constr_arrow_type:
        (ptyp_arrow
           { arg_label = Nolabel; arg_type = constr_record_type; arg_modes = [] }
           { result_type =
               ptyp_arrow
                 { arg_label = Nolabel
                 ; arg_type = ptyp_constr (Lident unique_parameter_id |> Located.mk) []
                 ; arg_modes = []
                 }
                 { result_type = constr_record_type; result_modes = [] }
           ; result_modes = []
           })
      ~var_arrow_type:
        (ptyp_arrow
           { arg_label = Nolabel; arg_type = var_record_type; arg_modes = [] }
           { result_type =
               ptyp_arrow
                 { arg_label = Nolabel
                 ; arg_type = ptyp_var unique_parameter_id
                 ; arg_modes = []
                 }
                 { result_type = var_record_type; result_modes = [] }
           ; result_modes = []
           })
      ~function_body
      ~name_of_first_parameter:(Lident Type_kind.internal_gadt_name)
      ()
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
        ~init:(pmod_structure (type_ids @ subproduct_type_id_modules @ [ type_id ]))
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
      [%stri let equal packed_1 packed_2 = Base.Int.equal 0 (compare packed_1 packed_2)]
    in
    let equal__local =
      [%stri
        let equal__local (packed_1 @ local) (packed_2 @ local) =
          Base.Int.equal 0 (compare__local packed_1 packed_2)
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
    ; globalize0
    ; globalize
    ; packed
    ; names
    ; internal_gadt_rename
    ]
;;

(** Generates a structure with the two submodules, Shallow, Deep and exposes the full
    depth version of Deep. *)
let gen_str
  (type a)
  (module Specific_generator : Type_kind.S with type t = a)
  ~original_type
  ~original_kind
  ~loc
  ~(elements_to_convert : (a * Type_kind.granularity) list)
  ~params
  =
  let open (val Syntax.builder loc) in
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
