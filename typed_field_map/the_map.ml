open! Base
include The_map_intf

module Make_plain (Key : Typed_fields_lib.Common.S) (Data : Data) = struct
  module Key_mod = struct
    include Key

    let type_id = Type_ids.type_id
    let sexp_of_t _ t = Packed.sexp_of_t { f = T t }
  end

  include
    Univ_map.Make
      (Key_mod)
      (struct
        include Data

        let sexp_of_t _ = Sexplib.Conv.sexp_of_opaque
      end)

  module Key = Key_mod

  let find t key = find_exn t (Key.globalize0 key)
  let change t key ~f = change_exn t (Key.globalize0 key) ~f
  let set t ~key ~data = set t ~key:(Key.globalize0 key) ~data

  type creator = { f : 'a. 'a Key.t @ local -> 'a Data.t }

  let create creator =
    List.fold Key.Packed.all ~init:empty ~f:(fun acc { f = Key.Packed.T t } ->
      add_exn acc ~key:t ~data:(creator.f t))
  ;;
end

module Make (Key : Typed_fields_lib.Common.S) (Data : Data) = struct
  module Key = Key
  module Data = Data
  module Base = Make_plain (Key) (Data)

  type creator = Base.creator = { f : 'a. 'a Key.t @ local -> 'a Data.t }

  type sexper =
    { individual : 'a. 'a Key.t @ local -> 'a -> Sexp.t
    ; container : 'a. ('a -> Sexp.t) -> 'a Data.t -> Sexp.t
    }

  type t =
    { base : Base.t
    ; sexper : sexper option
    }

  let create ?sexper creator = { base = Base.create creator; sexper }
  let set t ~key ~data = { t with base = Base.set t.base ~key ~data }
  let change t key ~f = { t with base = Base.change t.base key ~f }
  let find t key = Base.find t.base key

  module As_applicative = struct
    module type S = sig
      type 'a t = 'a Data.t

      val map : 'a t -> f:('a -> 'b) -> 'b t
      val all : 'a t list -> 'a list t
    end

    module type S_for_other_map = sig
      type 'a t

      val map : 'a t -> f:('a -> 'b) -> 'b t
      val all : 'a t list -> 'a list t

      type 'a s

      val translate : 'a Data.t -> 'a s t
    end

    module Id = struct
      type 'a t = 'a
    end

    module Id_map = Make_plain (Key) (Id)

    type creator = { f : 'a. 'a Key.t @ local -> 'a }

    let transpose (module A : S) t ~create =
      t.base
      |> Base.to_alist
      |> List.map ~f:(function T (key, a) ->
        A.map a ~f:(fun a -> Id_map.Packed.T (key, a)))
      |> A.all
      |> A.map ~f:(fun all ->
        let map = Id_map.of_alist_exn all in
        create { f = (fun k -> Id_map.find map (Key.globalize0 k)) })
    ;;

    module To_other_map
        (A : S_for_other_map)
        (M : S_plain with type 'a Key.t = 'a Key.t and type 'a Data.t = 'a A.s) =
    struct
      module Inner =
        Make_plain
          (Key)
          (struct
            type 'a t = 'a A.s
          end)

      let run t =
        t.base
        |> Base.to_alist
        |> List.map ~f:(function T (key, a) ->
          A.map (A.translate a) ~f:(fun a -> Inner.Packed.T (key, a)))
        |> A.all
        |> A.map ~f:(fun alist ->
          let m = Inner.of_alist_exn alist in
          M.create { f = (fun k -> Inner.find m (Key.globalize0 k)) })
      ;;
    end
  end

  let sexp_of_t t =
    match t.sexper with
    | None -> Base.sexp_of_t t.base
    | Some sexpers ->
      t.base
      |> Base.to_alist
      |> List.map ~f:(function T (k, v) ->
        let sexp_of_a = sexpers.container [%eta1 sexpers.individual k] v in
        Sexp.List [ Key.Packed.sexp_of_t { f = T k }; sexp_of_a ])
      |> Sexp.List
  ;;
end

module Make_for_records (Key : Typed_fields_lib.S) (Data : Data) = struct
  let create_derived_on = Key.create

  module Original_key = Key
  include Make (Key) (Data)

  let transpose_applicative { f } (module A : As_applicative.S) =
    let t = create { f } in
    As_applicative.transpose (module A) t ~create:(fun { f } -> create_derived_on { f })
  ;;

  (* Re-export Key as Typed_fields_lib.S *)
  module Key = Original_key
end
