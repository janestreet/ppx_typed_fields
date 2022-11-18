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

module S_of_S1 (M : S1) (T : T) :
  S with type 'a t = (T.t, 'a) M.t and type derived_on = T.t M.derived_on = struct
  include M

  type 'a t = (T.t, 'a) M.t
  type derived_on = T.t M.derived_on

  module Type_ids = Type_ids (T)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)
    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)
    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let pack field = { f = T field }
  end

  let which t = Packed.packed_of_m (M.which t)
end

module S_of_S2 (M : S2) (T1 : T) (T2 : T) :
  S with type 'a t = (T1.t, T2.t, 'a) M.t and type derived_on = (T1.t, T2.t) M.derived_on =
struct
  include M

  type 'a t = (T1.t, T2.t, 'a) M.t
  type derived_on = (T1.t, T2.t) M.derived_on

  module Type_ids = Type_ids (T1) (T2)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)
    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)
    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let pack field = { f = T field }
  end

  let which t = Packed.packed_of_m (M.which t)
end

module S_of_S3 (M : S3) (T1 : T) (T2 : T) (T3 : T) :
  S
  with type 'a t = (T1.t, T2.t, T3.t, 'a) M.t
   and type derived_on = (T1.t, T2.t, T3.t) M.derived_on = struct
  include M

  type 'a t = (T1.t, T2.t, T3.t, 'a) M.t
  type derived_on = (T1.t, T2.t, T3.t) M.derived_on

  module Type_ids = Type_ids (T1) (T2) (T3)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)
    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)
    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let pack field = { f = T field }
  end

  let which t = Packed.packed_of_m (M.which t)
end

module S_of_S4 (M : S4) (T1 : T) (T2 : T) (T3 : T) (T4 : T) :
  S
  with type 'a t = (T1.t, T2.t, T3.t, T4.t, 'a) M.t
   and type derived_on = (T1.t, T2.t, T3.t, T4.t) M.derived_on = struct
  include M

  type 'a t = (T1.t, T2.t, T3.t, T4.t, 'a) M.t
  type derived_on = (T1.t, T2.t, T3.t, T4.t) M.derived_on

  module Type_ids = Type_ids (T1) (T2) (T3) (T4)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)
    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)
    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let pack field = { f = T field }
  end

  let which t = Packed.packed_of_m (M.which t)
end

module S_of_S5 (M : S5) (T1 : T) (T2 : T) (T3 : T) (T4 : T) (T5 : T) :
  S
  with type 'a t = (T1.t, T2.t, T3.t, T4.t, T5.t, 'a) M.t
   and type derived_on = (T1.t, T2.t, T3.t, T4.t, T5.t) M.derived_on = struct
  include M

  type 'a t = (T1.t, T2.t, T3.t, T4.t, T5.t, 'a) M.t
  type derived_on = (T1.t, T2.t, T3.t, T4.t, T5.t) M.derived_on

  module Type_ids = Type_ids (T1) (T2) (T3) (T4) (T5)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)
    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)
    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let pack field = { f = T field }
  end

  let which t = Packed.packed_of_m (M.which t)
end

module type Typed_variants_lib = sig
  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3
  module type S4 = S4
  module type S5 = S5

  module S_of_S1 (M : S1) (T : T) :
    S with type 'a t = (T.t, 'a) M.t and type derived_on = T.t M.derived_on

  module S_of_S2 (M : S2) (T1 : T) (T2 : T) :
    S
    with type 'a t = (T1.t, T2.t, 'a) M.t
     and type derived_on = (T1.t, T2.t) M.derived_on

  module S_of_S3 (M : S3) (T1 : T) (T2 : T) (T3 : T) :
    S
    with type 'a t = (T1.t, T2.t, T3.t, 'a) M.t
     and type derived_on = (T1.t, T2.t, T3.t) M.derived_on

  module S_of_S4 (M : S4) (T1 : T) (T2 : T) (T3 : T) (T4 : T) :
    S
    with type 'a t = (T1.t, T2.t, T3.t, T4.t, 'a) M.t
     and type derived_on = (T1.t, T2.t, T3.t, T4.t) M.derived_on

  module S_of_S5 (M : S5) (T1 : T) (T2 : T) (T3 : T) (T4 : T) (T5 : T) :
    S
    with type 'a t = (T1.t, T2.t, T3.t, T4.t, T5.t, 'a) M.t
     and type derived_on = (T1.t, T2.t, T3.t, T4.t, T5.t) M.derived_on

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
