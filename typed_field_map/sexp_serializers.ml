open! Core

module Make (Map : The_map_intf.S_plain) = struct
  module Typed_field = Map.Key

  type to_sexper = { f : 'a. 'a Typed_field.t -> 'a Map.Data.t -> Sexp.t }
  type of_sexper = { f : 'a. 'a Map.Key.t -> Sexp.t -> 'a Map.Data.t }
  type defaulter = { f : 'a. 'a Map.Key.t -> 'a Map.Data.t option }

  let default_defaulter = { f = (fun _ -> None) }

  let sexp_of_t (to_sexper : to_sexper) (t : Map.t) : Sexp.t =
    let module P = Typed_field.Packed in
    let sexp_of_field { P.f = T key } =
      let key_sexp = P.sexp_of_t { f = T key } in
      let data_sexp = to_sexper.f key (Map.find t key) in
      Sexp.List [ key_sexp; data_sexp ]
    in
    List (List.map Typed_field.Packed.all ~f:sexp_of_field)
  ;;

  module Optional_map = struct
    module T = struct
      type 'a t = 'a Map.Data.t option
    end

    include The_map.Make (Map.Key) (T)

    let empty = create { f = (fun _ -> None) }
  end

  let raise_invalid_sexp sexp =
    raise_s
      [%message
        "Cannot deserialize typed_field_map. Reason: Unrecognized sexp:"
          ~_:(sexp : Sexp.t)]
  ;;

  let t_of_sexp ?(default = default_defaulter) (of_sexper : of_sexper) (sexp : Sexp.t)
    : Map.t
    =
    match sexp with
    | Sexp.Atom _ -> raise_invalid_sexp sexp
    | List l ->
      let map_with_parsed_results =
        List.fold l ~init:Optional_map.empty ~f:(fun acc -> function
          | List [ key_sexp; data_sexp ] ->
            let { f = T key } = Typed_field.Packed.t_of_sexp key_sexp in
            let data = of_sexper.f key data_sexp in
            (match Optional_map.find acc key with
             | None -> Optional_map.set acc ~key ~data:(Some data)
             | Some _ ->
               let duplicate_field = Typed_field.name key in
               raise_s
                 [%message
                   "Cannot deserialize typed_field_map. Reason: duplicate field:"
                     (duplicate_field : string)])
          | _ -> raise_invalid_sexp sexp)
      in
      Map.create
        { Map.f =
            (fun f ->
              match Optional_map.find map_with_parsed_results f with
              | None ->
                (match default.f f with
                 | Some data -> data
                 | None ->
                   let missing_field = Typed_field.name f in
                   raise_s
                     [%message
                       "Cannot deserialize typed_field_map. Reason: missing field:"
                         (missing_field : string)])
              | Some data -> data)
        }
  ;;
end
