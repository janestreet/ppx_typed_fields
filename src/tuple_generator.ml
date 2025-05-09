open! Base
open Ppxlib

type t = core_type

let name index _ = [%string "t_%{(index + 1)#Int}"]
let to_type = Fn.id

let get_rhs_expression ~loc ~index ~element:_ ~number_of_elements =
  let open (val Syntax.builder loc) in
  let pattern =
    ppat_tuple
      (List.init number_of_elements ~f:(fun i ->
         None, if i = index then ppat_var (Located.mk "x") else ppat_any))
      Closed
  in
  pexp_let
    Nonrecursive
    [ value_binding ~pat:pattern ~expr:[%expr record] ~modes:[] ]
    [%expr x]
;;

let set_rhs_expression ~loc ~index ~element:_ ~number_of_elements ~expression_to_set =
  let open (val Syntax.builder loc) in
  let generate_temp_idenfier i = [%string "x%{i#Int}"] in
  let pattern =
    ppat_tuple
      (List.init number_of_elements ~f:(fun i ->
         ( None
         , if i = index
           then ppat_any
           else generate_temp_idenfier i |> Located.mk |> ppat_var )))
      Closed
  in
  let tuple_building_expression =
    pexp_tuple
      (List.init number_of_elements ~f:(fun i ->
         ( None
         , if i = index
           then expression_to_set
           else Lident (generate_temp_idenfier i) |> Located.mk |> pexp_ident )))
  in
  pexp_let
    Nonrecursive
    [ value_binding ~pat:pattern ~expr:[%expr record] ~modes:[] ]
    tuple_building_expression
;;

let create_expression ~loc ~constructor_declarations ~local =
  let open (val Syntax.builder loc) in
  let number_of_declarations = List.length constructor_declarations in
  let generate_temp_idenfier i = [%string "x%{i#Int}"] in
  let create_tuple =
    pexp_tuple
      (List.init number_of_declarations ~f:(fun i ->
         None, Lident (generate_temp_idenfier i) |> Located.mk |> pexp_ident))
  in
  (* create fields and then creates a tuple. *)
  List.foldi
    (List.rev constructor_declarations)
    ~init:create_tuple
    ~f:(fun index acc ((element, granularity), constructor) ->
      let unreversed_index = number_of_declarations - index - 1 in
      let expr =
        match granularity with
        | Type_kind.Shallow ->
          [%expr __ppx_typed_fields_creator_f [%e econstruct constructor None]]
        | Type_kind.Deep _ ->
          let constructor_expression =
            pexp_construct
              (Lident (name unreversed_index element |> String.capitalize) |> Located.mk)
              (Some (pexp_ident (Lident "x" |> Located.mk)))
          in
          let subproduct_function =
            let subproduct_module_name =
              name unreversed_index element
              |> String.capitalize
              |> Type_kind.append_functor_parameter
            in
            match local with
            | false ->
              pexp_ident (Ldot (Lident subproduct_module_name, "create") |> Located.mk)
            | true ->
              pexp_ident
                (Ldot (Lident subproduct_module_name, "create_local") |> Located.mk)
          in
          [%expr
            [%e subproduct_function]
              { f =
                  (fun (x @ local) ->
                    __ppx_typed_fields_creator_f [%e constructor_expression] [@nontail])
              }]
      in
      pexp_let
        Nonrecursive
        [ value_binding
            ~pat:(pvar (generate_temp_idenfier unreversed_index))
            ~expr
            ~modes:[]
        ]
        acc)
;;
