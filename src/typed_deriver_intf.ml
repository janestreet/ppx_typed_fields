open Base
open Ppxlib

(**
   Generates a multi-arrow call.

   Instead of writing something like:
   arrow_type (a (arrow_type b (arrow_type c (arrow_type d))))

   allows the writing of:
   generate_arrow_type [a; b; c] d

*)
let generate_arrow_type ~loc ~types_before_last ~last_type =
  let open (val Ast_builder.make loc) in
  List.fold_right types_before_last ~init:last_type ~f:(fun type_ acc ->
    ptyp_arrow Nolabel type_ acc)
;;

let generate_packed_field_type_declaration
      ~loc
      ~params
      ~unique_parameter_id
      ~t_type_constr
  =
  let open (val Ast_builder.make loc) in
  let field_params =
    params @ [ ptyp_var unique_parameter_id, (NoVariance, NoInjectivity) ]
  in
  type_declaration
    ~name:(Located.mk "field")
    ~params:field_params
    ~cstrs:[]
    ~kind:Ptype_abstract
    ~private_:Public
    ~manifest:(Some t_type_constr)
;;

let generate_packed_t_prime_type_declaration ~loc ~params ~core_type_params ~field_type =
  let open (val Ast_builder.make loc) in
  type_declaration
    ~name:(Located.mk "t'")
    ~params
    ~cstrs:[]
    ~private_:Public
    ~manifest:None
    ~kind:
      (Ptype_variant
         [ constructor_declaration
             ~name:(Located.mk "T")
             ~args:(Pcstr_tuple [ field_type ])
             ~res:(Some (ptyp_constr (Lident "t'" |> Located.mk) core_type_params))
         ])
;;

let generate_packed_t_type_declaration ~loc ~core_type_params =
  let open (val Ast_builder.make loc) in
  let ty =
    type_declaration
      ~name:(Located.mk "t")
      ~cstrs:[]
      ~private_:Public
      ~manifest:None
      ~params:[]
      ~kind:
        (Ptype_record
           [ label_declaration
               ~name:(Located.mk "f")
               ~mutable_:Immutable
               ~type_:
                 (ptyp_poly
                    (List.filter_map core_type_params ~f:(fun param ->
                       match param.ptyp_desc with
                       | Ptyp_var name -> Some (Located.mk name)
                       | _ -> None))
                    (ptyp_constr (Lident "t'" |> Located.mk) core_type_params))
           ])
  in
  let attribute = attribute ~name:(Located.mk "unboxed") ~payload:(PStr []) in
  { ty with ptype_attributes = [ attribute ] }
;;

let disable_warning_37 ~loc =
  let open (val Ast_builder.make loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-37") [] ])
;;

(**
   Generates

   let <function_name> :
   type <unique_parameter_id>. <core_type_params @ [unique_parameter_id]> t
   -> <arrow_type> = <function_body>


   e.g. let name : type a_. ('a, a_) t -> string = fun x -> match x with ...
*)
let generate_new_typed_function
      ~loc
      ~function_name
      ~core_type_params
      ~unique_parameter_id
      ~var_arrow_type
      ~constr_arrow_type
      ~function_body
      ~name_of_first_parameter
  =
  let open (val Ast_builder.make loc) in
  let parameter_names =
    List.filter_map core_type_params ~f:(fun { ptyp_desc; _ } ->
      match ptyp_desc with
      | Ptyp_var name -> Some (Located.mk name)
      | _ -> None)
  in
  let t_type_parameters = parameter_names @ [ Located.mk unique_parameter_id ] in
  let function_type =
    ptyp_poly
      t_type_parameters
      (ptyp_arrow
         Nolabel
         (ptyp_constr
            (Located.mk (Lident name_of_first_parameter))
            (core_type_params @ [ ptyp_var unique_parameter_id ]))
         var_arrow_type)
  in
  let function_expression =
    let parameters_as_constrs =
      List.map core_type_params ~f:(fun type_ ->
        match type_.ptyp_desc with
        | Ptyp_var name -> ptyp_constr (Lident name |> Located.mk) []
        | _ -> type_)
    in
    let inner_new_type =
      pexp_newtype
        (Located.mk unique_parameter_id)
        (pexp_constraint
           function_body
           (ptyp_arrow
              Nolabel
              (ptyp_constr
                 (Located.mk (Lident name_of_first_parameter))
                 (parameters_as_constrs
                  @ [ ptyp_constr (Located.mk (Lident unique_parameter_id)) [] ]))
              constr_arrow_type))
    in
    List.fold_right parameter_names ~init:inner_new_type ~f:(fun name acc ->
      pexp_newtype name acc)
  in
  pstr_value
    Nonrecursive
    [ value_binding
        ~pat:(ppat_constraint (ppat_var (Located.mk function_name)) function_type)
        ~expr:function_expression
    ]
;;

let at_least_one_subproduct elements_to_convert =
  List.exists elements_to_convert ~f:(fun (_, granularity) ->
    match granularity with
    | Type_kind_intf.Shallow -> false
    | Type_kind_intf.Deep _ -> true)
;;

module type S = sig
  (**
     Either generates either
     `include Typed_fields_lib.SN with type record := record`
     or
     the fully generated partial signature if the number of parameter is above 5.
  *)
  val generate_include_signature_for_opaque
    :  loc:location
    -> params:(core_type * (variance * injectivity)) list
    -> signature_item list

  (**
     Either generates either
     `include Typed_fields_lib.SN with type record := record and type t := t`
     or
     the fully generated partial signature if the number of parameter is above 5.
  *)
  val generate_include_signature
    :  loc:location
    -> params:(core_type * (variance * injectivity)) list
    -> signature_item list
end
