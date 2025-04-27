open! Base
include Variant_kind_generator_intf.Definitions

let append_functor_parameter original_name = original_name ^ "_subvariant"

let supported_constructor_name = function
  | Anonymous_record_constructor { constructor_name; _ }
  | No_values_constructor { constructor_name; _ }
  | Single_value_constructor { constructor_name; _ }
  | Tuple_values_constructor { constructor_name; _ } -> constructor_name
;;

let supported_constructor_type = function
  | Anonymous_record_constructor { return_value_type; _ }
  | Tuple_values_constructor { return_value_type; _ }
  | No_values_constructor { return_value_type; _ }
  | Single_value_constructor { return_value_type; _ } -> return_value_type
;;

let strip_depth_from_supported_declaration declaration =
  match declaration with
  | Anonymous_record_constructor _ | No_values_constructor _ | Tuple_values_constructor _
    -> declaration
  | Single_value_constructor contents ->
    Single_value_constructor { contents with granularity = Shallow }
;;

let strip_depth_from_td_case td_case =
  match td_case with
  | Nothing _ | Opaque _ | Unknown -> td_case
  | Variant (declarations, params) ->
    Variant (List.map declarations ~f:strip_depth_from_supported_declaration, params)
;;

let at_least_one_subvariant constructor_declarations =
  List.exists constructor_declarations ~f:(fun cd ->
    match cd with
    | Single_value_constructor { granularity = Constr_deep _; _ }
    | Single_value_constructor { granularity = Polymorphic_deep; _ } -> true
    | _ -> false)
;;
