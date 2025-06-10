open! Base

module type Data = T1

(** The reason that S_plain exists is because the To_other_map functor needs to take one
    as an argument, so they need to be split.

    One of these maps is isomorphic to the record that they are built out of. When
    creating the map via the creator function, you provide all possible keys and values.

    This means that [find] will always succeed. *)
module type S_plain = sig
  type t [@@deriving sexp_of]

  module Key : Typed_fields_lib.Common.S
  module Data : Data

  type sexper =
    { individual : 'a. 'a Key.t @ local -> 'a -> Sexp.t
    ; container : 'a. ('a -> Sexp.t) -> 'a Data.t -> Sexp.t
    }

  type creator = { f : 'a. 'a Key.t @ local -> 'a Data.t }

  val create : ?sexper:sexper -> creator -> t
  val set : t -> key:'a Key.t @ local -> data:'a Data.t -> t

  (** Find will always succeed *)
  val find : t -> 'a Key.t @ local -> 'a Data.t

  val change : t -> 'a Key.t @ local -> f:('a Data.t -> 'a Data.t) -> t

  module As_applicative : sig
    module type S = sig
      type 'a t = 'a Data.t

      val map : 'a t -> f:('a -> 'b) -> 'b t
      val all : 'a t list -> 'a list t
    end

    type creator = { f : 'a. 'a Key.t @ local -> 'a }

    val transpose : (module S) -> t -> create:(creator -> 'a) -> 'a Data.t
  end
end

module type S = sig
  include S_plain

  module As_applicative : sig
    include module type of As_applicative

    module type S_for_other_map = sig
      (** This module is basically "Applicative", but with an additional type that can be
          used to translate to the applicative type.

          Typically these types will be something like

          {[
            type 'a t = 'a Value.t
            type 'a s = 'a Form.t

            val translate : 'a Form.t Value.t -> 'a Form.t Value.t
          ]}

          [translate] probably doesn't need to do any work. It just exists to expose a
          type equality between ['a Data.t] and ['a s t]. *)

      type 'a t

      val map : 'a t -> f:('a -> 'b) -> 'b t
      val all : 'a t list -> 'a list t

      type 'a s

      val translate : 'a Data.t -> 'a s t
    end

    module To_other_map
        (A : S_for_other_map)
        (M : S_plain with type 'a Key.t = 'a Key.t and type 'a Data.t = 'a A.s) : sig
      val run : t -> M.t A.t
    end
  end
end

module For_records = struct
  (** This module type just extends [S] with a more ergonomic way to call
      [transpose_applicative] that works for records.

      It goes from:
      {[
        let module Map = Typed_field_map.Make (Typed_field) (Deferred) in
        let map = Map.create { f } in
        Map.As_applicative.transpose (module Deferred) map ~create:(fun { f } ->
          Typed_field.create { f })
      ]}

      To:
      {[
        let module Map = Typed_field_map.Make_for_records (M.Typed_field) (Deferred) in
        Map.transpose_applicative { f } (module Deferred)
      ]} *)
  module type S = sig
    module Key : Typed_fields_lib.S
    include S with module Key := Key

    val transpose_applicative
      :  creator
      -> (module As_applicative.S)
      -> Key.derived_on Data.t
  end
end

module type The_map = sig
  module type Data = Data
  module type S = S
  module type S_plain = S_plain

  module Make (Key : Typed_fields_lib.Common.S) (Data : Data) :
    S with module Key = Key and module Data = Data

  module Make_for_records (Key : Typed_fields_lib.S) (Data : Data) :
    For_records.S with module Key = Key and module Data = Data
end
