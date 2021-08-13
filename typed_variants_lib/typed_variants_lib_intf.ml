(** [@@deriving typed_variants] will derive a module that satisfies module type .

    For example, this
    {[
      type t =
        | Rgb of (int * int * int)
        | Rgba of { r: int; g: int; b: int; a: int }
      [@@deriving typed_variants]
    ]}

    will generate a sub-module that looks something like this
    {[
      module Typed_variants : sig
        type nonrec derived_on = t

        module Typed_variant_anonymous_records : sig
          type rgba_record = {
            r: int;
            g: int;
            b: int;
            a: int;
          }
        end

        type _ t =
          | Rgb: (int * int * int) t
          | Rgba:  Typed_variant_anonymous_records.rgba_record t

        include Typed_variants.S with type 'a t := 'a t and type derived_on := derived_on
      end
    ]}

    Exposing this as a module type allows one to write functors over the derived
    `Typed_variants` module.
*)

open Base

module type S = sig
  include Typed_fields_lib.Common.S

  val get : 'a t -> derived_on -> 'a option
  val create : 'a t -> 'a -> derived_on
  val which : derived_on -> Packed.t
end

module type S1 = sig
  include Typed_fields_lib.Common.S1

  val get : ('t1, 'a) t -> 't1 derived_on -> 'a option
  val create : ('t1, 'a) t -> 'a -> 't1 derived_on
  val which : 't1 derived_on -> Packed.t
end

module type S2 = sig
  include Typed_fields_lib.Common.S2

  val get : ('t1, 't2, 'a) t -> ('t1, 't2) derived_on -> 'a option
  val create : ('t1, 't2, 'a) t -> 'a -> ('t1, 't2) derived_on
  val which : ('t1, 't2) derived_on -> Packed.t
end

module type S3 = sig
  include Typed_fields_lib.Common.S3

  val get : ('t1, 't2, 't3, 'a) t -> ('t1, 't2, 't3) derived_on -> 'a option
  val create : ('t1, 't2, 't3, 'a) t -> 'a -> ('t1, 't2, 't3) derived_on
  val which : ('t1, 't2, 't3) derived_on -> Packed.t
end

module type S4 = sig
  include Typed_fields_lib.Common.S4

  val get : ('t1, 't2, 't3, 't4, 'a) t -> ('t1, 't2, 't3, 't4) derived_on -> 'a option
  val create : ('t1, 't2, 't3, 't4, 'a) t -> 'a -> ('t1, 't2, 't3, 't4) derived_on
  val which : ('t1, 't2, 't3, 't4) derived_on -> Packed.t
end

module type S5 = sig
  include Typed_fields_lib.Common.S5

  val get
    :  ('t1, 't2, 't3, 't4, 't5, 'a) t
    -> ('t1, 't2, 't3, 't4, 't5) derived_on
    -> 'a option

  val create
    :  ('t1, 't2, 't3, 't4, 't5, 'a) t
    -> 'a
    -> ('t1, 't2, 't3, 't4, 't5) derived_on

  val which : ('t1, 't2, 't3, 't4, 't5) derived_on -> Packed.t
end

module type Typed_variants_lib = sig
  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3
  module type S4 = S4
  module type S5 = S5

  module Singleton (T : T) : sig
    type 'a t = T : T.t t

    include S with type derived_on = T.t and type 'a t := 'a t
  end

  module Singleton1 (T1 : T1) : sig
    type ('a, 'r) t = T : ('a, 'a T1.t) t

    include S1 with type 'a derived_on = 'a T1.t and type ('a, 'r) t := ('a, 'r) t
  end

  module Singleton2 (T2 : T2) : sig
    type ('a, 'b, 'r) t = T : ('a, 'b, ('a, 'b) T2.t) t

    include
      S2
      with type ('a, 'b) derived_on = ('a, 'b) T2.t
       and type ('a, 'b, 'r) t := ('a, 'b, 'r) t
  end

  module Singleton3 (T3 : T3) : sig
    type ('a, 'b, 'c, 'r) t = T : ('a, 'b, 'c, ('a, 'b, 'c) T3.t) t

    include
      S3
      with type ('a, 'b, 'c) derived_on = ('a, 'b, 'c) T3.t
       and type ('a, 'b, 'c, 'r) t := ('a, 'b, 'c, 'r) t
  end

  module Singleton4 (T4 : sig
      type ('a, 'b, 'c, 'd) t
    end) : sig
    type ('t1, 't2, 't3, 't4, 'r) t =
      | T : ('t1, 't2, 't3, 't4, ('t1, 't2, 't3, 't4) T4.t) t

    include
      S4
      with type ('t1, 't2, 't3, 't4) derived_on = ('t1, 't2, 't3, 't4) T4.t
       and type ('t1, 't2, 't3, 't4, 'r) t := ('t1, 't2, 't3, 't4, 'r) t
  end

  module Singleton5 (T5 : sig
      type ('a, 'b, 'c, 'd, 'e) t
    end) : sig
    type ('t1, 't2, 't3, 't4, 't5, 'r) t =
      | T : ('t1, 't2, 't3, 't4, 't5, ('t1, 't2, 't3, 't4, 't5) T5.t) t

    include
      S5
      with type ('t1, 't2, 't3, 't4, 't5) derived_on = ('t1, 't2, 't3, 't4, 't5) T5.t
       and type ('t1, 't2, 't3, 't4, 't5, 'r) t := ('t1, 't2, 't3, 't4, 't5, 'r) t
  end

  module Nothing : sig
    type derived_on = |
    type 'a t = |

    include S with type derived_on := derived_on and type 'a t := 'a t
  end
end
