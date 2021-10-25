open! Core

module Make (Leaf_data : T1) = struct
  module rec Tree : sig
    type 'a t =
      | Leaf : 'a Leaf_data.t -> 'a t
      | Branch : (module Branch.S with type Typed_field.derived_on = 'a) -> 'a t
  end =
    Tree

  and Branch : sig
    module type S = sig
      module Typed_field : Typed_fields_lib.S

      module Map :
        The_map.S with module Key := Typed_field and type 'a Data.t := 'a Tree.t

      val map : Map.t
    end
  end =
    Branch

  type 'a t = 'a Tree.t

  let leaf kind = Tree.Leaf kind

  module type S = sig
    module Typed_field : Typed_fields_lib.S

    val children : 'a Typed_field.t -> 'a Tree.t
  end

  let branch (type a) (module N : S with type Typed_field.derived_on = a) =
    let module M = struct
      module Typed_field = N.Typed_field
      module Map = The_map.Make (Typed_field) (Tree)

      let map = Map.create { f = N.children }
    end
    in
    Tree.Branch (module M)
  ;;
end
