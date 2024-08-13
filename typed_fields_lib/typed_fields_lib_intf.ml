(** [@@deriving typed_fields] will derive a module that satisfies module type S.

    For example, this
    {[
      type t =
        { name : string
        ; age : int
        }
      [@@deriving typed_fields]
    ]}

    will generate a sub-module that looks something like this
    {[
      module Typed_fields : sig
        type nonrec derived_on = t

        type _ t =
          | Name: string t
          | Age: int t

        include Typed_fields.S with type 'a t := 'a t and type derived_on := derived_on
      end
    ]}

    Exposing this as a module type allows one to write functors over the derived
    `Typed_fields` module.
*)
open Base

module type S = sig
  include Typed_common_lib_intf.S

  type creator = { f : 'a. 'a t -> 'a }

  val get : 'a t -> derived_on -> 'a
  val set : 'a t -> derived_on -> 'a -> derived_on
  val create : creator -> derived_on
  val create_local : creator -> derived_on
end

module type S1 = sig
  include Typed_common_lib_intf.S1

  type 't1 creator = { f : 'a. ('t1, 'a) t -> 'a }

  val get : ('t1, 'a) t -> 't1 derived_on -> 'a
  val set : ('t1, 'a) t -> 't1 derived_on -> 'a -> 't1 derived_on
  val create : 't1 creator -> 't1 derived_on
  val create_local : 't1 creator -> 't1 derived_on
end

module type S2 = sig
  include Typed_common_lib_intf.S2

  type ('t1, 't2) creator = { f : 'a. ('t1, 't2, 'a) t -> 'a }

  val get : ('t1, 't2, 'a) t -> ('t1, 't2) derived_on -> 'a
  val set : ('t1, 't2, 'a) t -> ('t1, 't2) derived_on -> 'a -> ('t1, 't2) derived_on
  val create : ('t1, 't2) creator -> ('t1, 't2) derived_on
  val create_local : ('t1, 't2) creator -> ('t1, 't2) derived_on
end

module type S3 = sig
  include Typed_common_lib_intf.S3

  type ('t1, 't2, 't3) creator = { f : 'a. ('t1, 't2, 't3, 'a) t -> 'a }

  val get : ('t1, 't2, 't3, 'a) t -> ('t1, 't2, 't3) derived_on -> 'a

  val set
    :  ('t1, 't2, 't3, 'a) t
    -> ('t1, 't2, 't3) derived_on
    -> 'a
    -> ('t1, 't2, 't3) derived_on

  val create : ('t1, 't2, 't3) creator -> ('t1, 't2, 't3) derived_on
  val create_local : ('t1, 't2, 't3) creator -> ('t1, 't2, 't3) derived_on
end

module type S4 = sig
  include Typed_common_lib_intf.S4

  type ('t1, 't2, 't3, 't4) creator = { f : 'a. ('t1, 't2, 't3, 't4, 'a) t -> 'a }

  val get : ('t1, 't2, 't3, 't4, 'a) t -> ('t1, 't2, 't3, 't4) derived_on -> 'a

  val set
    :  ('t1, 't2, 't3, 't4, 'a) t
    -> ('t1, 't2, 't3, 't4) derived_on
    -> 'a
    -> ('t1, 't2, 't3, 't4) derived_on

  val create : ('t1, 't2, 't3, 't4) creator -> ('t1, 't2, 't3, 't4) derived_on
  val create_local : ('t1, 't2, 't3, 't4) creator -> ('t1, 't2, 't3, 't4) derived_on
end

module type S5 = sig
  include Typed_common_lib_intf.S5

  type ('t1, 't2, 't3, 't4, 't5) creator =
    { f : 'a. ('t1, 't2, 't3, 't4, 't5, 'a) t -> 'a }

  val get : ('t1, 't2, 't3, 't4, 't5, 'a) t -> ('t1, 't2, 't3, 't4, 't5) derived_on -> 'a

  val set
    :  ('t1, 't2, 't3, 't4, 't5, 'a) t
    -> ('t1, 't2, 't3, 't4, 't5) derived_on
    -> 'a
    -> ('t1, 't2, 't3, 't4, 't5) derived_on

  val create : ('t1, 't2, 't3, 't4, 't5) creator -> ('t1, 't2, 't3, 't4, 't5) derived_on

  val create_local
    :  ('t1, 't2, 't3, 't4, 't5) creator
    -> ('t1, 't2, 't3, 't4, 't5) derived_on
end

module S_of_S1 (M : S1) (T : T) :
  S with type 'a t = (T.t, 'a) M.t and type derived_on = T.t M.derived_on = struct
  include M

  type 'a t = (T.t, 'a) M.t
  type derived_on = T.t M.derived_on
  type creator = { f : 'a. 'a t -> 'a }

  let create ({ f } : creator) =
    let m_creator_f : type a. (T.t, a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create m_creator
  ;;

  let create_local ({ f } : creator) =
    let m_creator_f : type a. (T.t, a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create_local m_creator
  ;;

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
end

module S_of_S2 (M : S2) (T1 : T) (T2 : T) :
  S with type 'a t = (T1.t, T2.t, 'a) M.t and type derived_on = (T1.t, T2.t) M.derived_on =
struct
  include M

  type 'a t = (T1.t, T2.t, 'a) M.t
  type derived_on = (T1.t, T2.t) M.derived_on
  type creator = { f : 'a. 'a t -> 'a }

  let create ({ f } : creator) =
    let m_creator_f : type a. (T1.t, T2.t, a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create m_creator
  ;;

  let create_local ({ f } : creator) =
    let m_creator_f : type a. (T1.t, T2.t, a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create_local m_creator
  ;;

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
end

module S_of_S3 (M : S3) (T1 : T) (T2 : T) (T3 : T) :
  S
  with type 'a t = (T1.t, T2.t, T3.t, 'a) M.t
   and type derived_on = (T1.t, T2.t, T3.t) M.derived_on = struct
  include M

  type 'a t = (T1.t, T2.t, T3.t, 'a) M.t
  type derived_on = (T1.t, T2.t, T3.t) M.derived_on
  type creator = { f : 'a. 'a t -> 'a }

  let create ({ f } : creator) =
    let m_creator_f : type a. (T1.t, T2.t, T3.t, a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create m_creator
  ;;

  let create_local ({ f } : creator) =
    let m_creator_f : type a. (T1.t, T2.t, T3.t, a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create_local m_creator
  ;;

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
end

module S_of_S4 (M : S4) (T1 : T) (T2 : T) (T3 : T) (T4 : T) :
  S
  with type 'a t = (T1.t, T2.t, T3.t, T4.t, 'a) M.t
   and type derived_on = (T1.t, T2.t, T3.t, T4.t) M.derived_on = struct
  include M

  type 'a t = (T1.t, T2.t, T3.t, T4.t, 'a) M.t
  type derived_on = (T1.t, T2.t, T3.t, T4.t) M.derived_on
  type creator = { f : 'a. 'a t -> 'a }

  let create ({ f } : creator) =
    let m_creator_f : type a. (T1.t, T2.t, T3.t, T4.t, a) M.t -> a =
      fun field -> f field
    in
    let m_creator = { M.f = m_creator_f } in
    M.create m_creator
  ;;

  let create_local ({ f } : creator) =
    let m_creator_f : type a. (T1.t, T2.t, T3.t, T4.t, a) M.t -> a =
      fun field -> f field
    in
    let m_creator = { M.f = m_creator_f } in
    M.create_local m_creator
  ;;

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
end

module S_of_S5 (M : S5) (T1 : T) (T2 : T) (T3 : T) (T4 : T) (T5 : T) :
  S
  with type 'a t = (T1.t, T2.t, T3.t, T4.t, T5.t, 'a) M.t
   and type derived_on = (T1.t, T2.t, T3.t, T4.t, T5.t) M.derived_on = struct
  include M

  type 'a t = (T1.t, T2.t, T3.t, T4.t, T5.t, 'a) M.t
  type derived_on = (T1.t, T2.t, T3.t, T4.t, T5.t) M.derived_on
  type creator = { f : 'a. 'a t -> 'a }

  let create ({ f } : creator) =
    let m_creator_f : type a. (T1.t, T2.t, T3.t, T4.t, T5.t, a) M.t -> a =
      fun field -> f field
    in
    let m_creator = { M.f = m_creator_f } in
    M.create m_creator
  ;;

  let create_local ({ f } : creator) =
    let m_creator_f : type a. (T1.t, T2.t, T3.t, T4.t, T5.t, a) M.t -> a =
      fun field -> f field
    in
    let m_creator = { M.f = m_creator_f } in
    M.create_local m_creator
  ;;

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
end

module type Typed_fields_lib = sig
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
    type ('t1, 'r) t = T : ('t1, 't1 T1.t) t

    include S1 with type 't1 derived_on = 't1 T1.t and type ('t1, 'r) t := ('t1, 'r) t
  end

  module Singleton2 (T2 : T2) : sig
    type ('t1, 't2, 'r) t = T : ('t1, 't2, ('t1, 't2) T2.t) t

    include
      S2
      with type ('t1, 't2) derived_on = ('t1, 't2) T2.t
       and type ('t1, 't2, 'r) t := ('t1, 't2, 'r) t
  end

  module Singleton3 (T3 : T3) : sig
    type ('t1, 't2, 't3, 'r) t = T : ('t1, 't2, 't3, ('t1, 't2, 't3) T3.t) t

    include
      S3
      with type ('t1, 't2, 't3) derived_on = ('t1, 't2, 't3) T3.t
       and type ('t1, 't2, 't3, 'r) t := ('t1, 't2, 't3, 'r) t
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

  (** This is a convenient module for deriving typed_fields on unit, which you can
      conceptually think of as a record with no fields.  OCaml does not support actual
      record types with no fields. *)
  module Unit : sig
    (* unit has no fields *)
    type 'a t = |

    include S with type derived_on = unit and type 'a t := 'a t
  end

  module Common : sig
    module type S = Typed_common_lib_intf.S
    module type S1 = Typed_common_lib_intf.S1
    module type S2 = Typed_common_lib_intf.S2
    module type S3 = Typed_common_lib_intf.S3
    module type S4 = Typed_common_lib_intf.S4
    module type S5 = Typed_common_lib_intf.S5
  end

  module Private : sig
    val list_to_sexp : Sexp.t list -> Sexp.t
  end
end
