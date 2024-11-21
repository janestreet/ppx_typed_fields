open! Base

module Make (Leaf_data : T1) : sig
  module rec Tree : sig
    type 'a t = private
      | Leaf : 'a Leaf_data.t -> 'a t
      | Branch : (module Branch.S with type Typed_field.derived_on = 'a) -> 'a t
  end

  and Branch : sig
    module type S = sig
      module Typed_field : Typed_fields_lib.S

      module Map :
        The_map.S with module Key := Typed_field and type 'a Data.t := 'a Tree.t

      val map : Map.t
    end
  end

  module type S = sig
    module Typed_field : Typed_fields_lib.S

    val children : 'a Typed_field.t -> 'a Tree.t
  end

  type 'a t = 'a Tree.t

  val leaf : 'a Leaf_data.t -> 'a t
  val branch : (module S with type Typed_field.derived_on = 'a) -> 'a t
end
