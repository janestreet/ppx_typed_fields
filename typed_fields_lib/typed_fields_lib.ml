open Base
include Typed_fields_lib_intf

module Unit = struct
  type nonrec derived_on = unit
  type _ t = |
  type creator = { f : 'a. 'a t -> 'a }

  let unreachable_code = function
    | (_ : _ t) -> .
  ;;

  let names = []
  let name : type a. a t -> string = unreachable_code
  let path : type a. a t -> string list = unreachable_code
  let __ord : type a. a t -> int list = unreachable_code
  let get : type a. a t -> derived_on -> a = unreachable_code
  let set : type a. a t -> derived_on -> a -> derived_on = fun t _ _ -> unreachable_code t
  let create ({ f = _ } : creator) : derived_on = ()
  let create_local (local_ ({ f = _ } : creator)) : derived_on = exclave_ ()

  module Type_ids = struct
    let type_id : type a. a t -> a Type_equal.Id.t = unreachable_code
  end

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let all = []

    let compare { f = T x1 } { f = T x2 } =
      Base.List.compare Base.Int.compare (__ord x1) (__ord x2)
    ;;

    let equal p1 p2 = compare p1 p2 = 0
    let pack : type a. a field -> t = unreachable_code

    let sexp_of_t packed =
      match packed with
      | (_ : t) -> .
    ;;

    let t_of_sexp sexp =
      raise_s
        (Sexp.List
           [ Sexp.Atom "Unit has no fields, so cannot convert sexp to field."; sexp ])
    ;;
  end
end

module Common = Typed_common_lib_intf

module Singleton (T : T) = struct
  type nonrec derived_on = T.t
  type _ t = T : T.t t
  type creator = { f : 'a. 'a t -> 'a }

  let names = [ "this" ]
  let name _ = "this"
  let path _ = []
  let __ord _ = [ 0 ]
  let get (type a) (T : a t) (t : derived_on) : a = t
  let set (type a) (T : a t) (_ : derived_on) (t : a) : derived_on = t
  let create { f } = f T
  let create_local { f } = f T

  module Type_ids = struct
    let type_id : T.t Type_equal.Id.t =
      Type_equal.Id.create ~name:"this" (fun _ -> Sexp.Atom "<opaque>")
    ;;

    let type_id (type a) (T : a t) : a Type_equal.Id.t = type_id
  end

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let compare _ _ = 0
    let equal _ _ = true
    let all = [ { f = T T } ]
    let sexp_of_t _ = Sexp.Atom "this"
    let t_of_sexp _ = { f = T T }
    let pack _ = { f = T T }
  end
end

module Singleton1 (T1 : T1) = struct
  type nonrec 't1 derived_on = 't1 T1.t
  type ('t1, 'r) t = T : ('t1, 't1 T1.t) t
  type 't1 creator = { f : 'a. ('t1, 'a) t -> 'a }

  let names = [ "this" ]
  let name _ = "this"
  let path _ = []
  let __ord _ = [ 0 ]
  let get (type t1 a) (T : (t1, a) t) (t : t1 derived_on) : a = t
  let set (type t1 a) (T : (t1, a) t) (_ : t1 derived_on) (t : a) : t1 derived_on = t
  let create { f } = f T
  let create_local { f } = f T

  module Type_ids (Type_id_T1 : T) = struct
    let type_id : Type_id_T1.t T1.t Type_equal.Id.t =
      Type_equal.Id.create ~name:"this" (fun _ -> Sexp.Atom "<opaque>")
    ;;

    let type_id (type a) (T : (Type_id_T1.t, a) t) : a Type_equal.Id.t = type_id
  end

  module Packed = struct
    type ('t1, 'r) field = ('t1, 'r) t
    type 't1 t' = T : ('t1, 'r) field -> 't1 t'
    type t = { f : 't1. 't1 t' } [@@unboxed]

    let compare _ _ = 0
    let equal _ _ = true
    let all = [ { f = T T } ]
    let sexp_of_t _ = Sexp.Atom "this"
    let t_of_sexp _ = { f = T T }
    let pack _ = { f = T T }
  end
end

module Singleton2 (T2 : T2) = struct
  type nonrec ('t1, 't2) derived_on = ('t1, 't2) T2.t
  type ('t1, 't2, 'r) t = T : ('t1, 't2, ('t1, 't2) T2.t) t
  type ('t1, 't2) creator = { f : 'a. ('t1, 't2, 'a) t -> 'a }

  let names = [ "this" ]
  let name _ = "this"
  let path _ = []
  let __ord _ = [ 0 ]
  let get (type t1 t2 a) (T : (t1, t2, a) t) (t : (t1, t2) derived_on) : a = t

  let set (type t1 t2 a) (T : (t1, t2, a) t) (_ : (t1, t2) derived_on) (t : a)
    : (t1, t2) derived_on
    =
    t
  ;;

  let create { f } = f T
  let create_local { f } = f T

  module Type_ids (Typed_id_T1 : T) (Type_id_T2 : T) = struct
    let type_id : (Typed_id_T1.t, Type_id_T2.t) T2.t Type_equal.Id.t =
      Type_equal.Id.create ~name:"this" (fun _ -> Sexp.Atom "<opaque>")
    ;;

    let type_id (type a) (T : (Typed_id_T1.t, Type_id_T2.t, a) t) : a Type_equal.Id.t =
      type_id
    ;;
  end

  module Packed = struct
    type ('t1, 't2, 'r) field = ('t1, 't2, 'r) t
    type ('t1, 't2) t' = T : ('t1, 't2, 'r) field -> ('t1, 't2) t'
    type t = { f : 't1 't2. ('t1, 't2) t' } [@@unboxed]

    let compare _ _ = 0
    let equal _ _ = true
    let all = [ { f = T T } ]
    let sexp_of_t _ = Sexp.Atom "this"
    let t_of_sexp _ = { f = T T }
    let pack _ = { f = T T }
  end
end

module Singleton3 (T3 : T3) = struct
  type nonrec ('t1, 't2, 't3) derived_on = ('t1, 't2, 't3) T3.t
  type ('t1, 't2, 't3, 'r) t = T : ('t1, 't2, 't3, ('t1, 't2, 't3) T3.t) t
  type ('t1, 't2, 't3) creator = { f : 'a. ('t1, 't2, 't3, 'a) t -> 'a }

  let names = [ "this" ]
  let name _ = "this"
  let path _ = []
  let __ord _ = [ 0 ]
  let get (type t1 t2 t3 a) (T : (t1, t2, t3, a) t) (t : (t1, t2, t3) derived_on) : a = t

  let set (type t1 t2 t3 a) (T : (t1, t2, t3, a) t) (_ : (t1, t2, t3) derived_on) (t : a)
    : (t1, t2, t3) derived_on
    =
    t
  ;;

  let create { f } = f T
  let create_local { f } = f T

  module Type_ids (Type_id_T1 : T) (Type_id_T2 : T) (Type_id_T3 : T) = struct
    let type_id : (Type_id_T1.t, Type_id_T2.t, Type_id_T3.t) T3.t Type_equal.Id.t =
      Type_equal.Id.create ~name:"this" (fun _ -> Sexp.Atom "<opaque>")
    ;;

    let type_id (type a) (T : (Type_id_T1.t, Type_id_T2.t, Type_id_T3.t, a) t)
      : a Type_equal.Id.t
      =
      type_id
    ;;
  end

  module Packed = struct
    type ('t1, 't2, 't3, 'r) field = ('t1, 't2, 't3, 'r) t
    type ('t1, 't2, 't3) t' = T : ('t1, 't2, 't3, 'r) field -> ('t1, 't2, 't3) t'
    type t = { f : 't1 't2 't3. ('t1, 't2, 't3) t' } [@@unboxed]

    let compare _ _ = 0
    let equal _ _ = true
    let all = [ { f = T T } ]
    let sexp_of_t _ = Sexp.Atom "this"
    let t_of_sexp _ = { f = T T }
    let pack _ = { f = T T }
  end
end

module Singleton4 (T4 : sig
    type ('a, 'b, 'c, 'd) t
  end) =
struct
  type nonrec ('t1, 't2, 't3, 't4) derived_on = ('t1, 't2, 't3, 't4) T4.t

  type ('t1, 't2, 't3, 't4, 'r) t =
    | T : ('t1, 't2, 't3, 't4, ('t1, 't2, 't3, 't4) T4.t) t

  type ('t1, 't2, 't3, 't4) creator = { f : 'a. ('t1, 't2, 't3, 't4, 'a) t -> 'a }

  let names = [ "this" ]
  let name _ = "this"
  let path _ = []
  let __ord _ = [ 0 ]

  let get
    (type t1 t2 t3 t4 a)
    (T : (t1, t2, t3, t4, a) t)
    (t : (t1, t2, t3, t4) derived_on)
    : a
    =
    t
  ;;

  let set
    (type t1 t2 t3 t4 a)
    (T : (t1, t2, t3, t4, a) t)
    (_ : (t1, t2, t3, t4) derived_on)
    (t : a)
    : (t1, t2, t3, t4) derived_on
    =
    t
  ;;

  let create { f } = f T
  let create_local { f } = f T

  module Type_ids (Type_id_T1 : T) (Type_id_T2 : T) (Type_id_T3 : T) (Type_id_T4 : T) =
  struct
    let type_id
      : (Type_id_T1.t, Type_id_T2.t, Type_id_T3.t, Type_id_T4.t) T4.t Type_equal.Id.t
      =
      Type_equal.Id.create ~name:"this" (fun _ -> Sexp.Atom "<opaque>")
    ;;

    let type_id
      (type a)
      (T : (Type_id_T1.t, Type_id_T2.t, Type_id_T3.t, Type_id_T4.t, a) t)
      : a Type_equal.Id.t
      =
      type_id
    ;;
  end

  module Packed = struct
    type ('t1, 't2, 't3, 't4, 'r) field = ('t1, 't2, 't3, 't4, 'r) t

    type ('t1, 't2, 't3, 't4) t' =
      | T : ('t1, 't2, 't3, 't4, 'r) field -> ('t1, 't2, 't3, 't4) t'

    type t = { f : 't1 't2 't3 't4. ('t1, 't2, 't3, 't4) t' } [@@unboxed]

    let compare _ _ = 0
    let equal _ _ = true
    let all = [ { f = T T } ]
    let sexp_of_t _ = Sexp.Atom "this"
    let t_of_sexp _ = { f = T T }
    let pack _ = { f = T T }
  end
end

module Singleton5 (T5 : sig
    type ('a, 'b, 'c, 'd, 'e) t
  end) =
struct
  type nonrec ('t1, 't2, 't3, 't4, 't5) derived_on = ('t1, 't2, 't3, 't4, 't5) T5.t

  type ('t1, 't2, 't3, 't4, 't5, 'r) t =
    | T : ('t1, 't2, 't3, 't4, 't5, ('t1, 't2, 't3, 't4, 't5) T5.t) t

  type ('t1, 't2, 't3, 't4, 't5) creator =
    { f : 'a. ('t1, 't2, 't3, 't4, 't5, 'a) t -> 'a }

  let names = [ "this" ]
  let name _ = "this"
  let path _ = []
  let __ord _ = [ 0 ]

  let get
    (type t1 t2 t3 t4 t5 a)
    (T : (t1, t2, t3, t4, t5, a) t)
    (t : (t1, t2, t3, t4, t5) derived_on)
    : a
    =
    t
  ;;

  let set
    (type t1 t2 t3 t4 t5 a)
    (T : (t1, t2, t3, t4, t5, a) t)
    (_ : (t1, t2, t3, t4, t5) derived_on)
    (t : a)
    : (t1, t2, t3, t4, t5) derived_on
    =
    t
  ;;

  let create { f } = f T
  let create_local { f } = f T

  module Type_ids
      (Type_id_T1 : T)
      (Type_id_T2 : T)
      (Type_id_T3 : T)
      (Type_id_T4 : T)
      (Type_id_T5 : T) =
  struct
    let type_id
      : (Type_id_T1.t, Type_id_T2.t, Type_id_T3.t, Type_id_T4.t, Type_id_T5.t) T5.t
          Type_equal.Id.t
      =
      Type_equal.Id.create ~name:"this" (fun _ -> Sexp.Atom "<opaque>")
    ;;

    let type_id
      (type a)
      (T : (Type_id_T1.t, Type_id_T2.t, Type_id_T3.t, Type_id_T4.t, Type_id_T5.t, a) t)
      : a Type_equal.Id.t
      =
      type_id
    ;;
  end

  module Packed = struct
    type ('t1, 't2, 't3, 't4, 't5, 'r) field = ('t1, 't2, 't3, 't4, 't5, 'r) t

    type ('t1, 't2, 't3, 't4, 't5) t' =
      | T : ('t1, 't2, 't3, 't4, 't5, 'r) field -> ('t1, 't2, 't3, 't4, 't5) t'

    type t = { f : 't1 't2 't3 't4 't5. ('t1, 't2, 't3, 't4, 't5) t' } [@@unboxed]

    let compare _ _ = 0
    let equal _ _ = true
    let all = [ { f = T T } ]
    let sexp_of_t _ = Sexp.Atom "this"
    let t_of_sexp _ = { f = T T }
    let pack _ = { f = T T }
  end
end

module Private = struct
  let list_to_sexp = function
    | [] -> assert false
    | [ some_sexp ] -> some_sexp
    | other -> Sexp.List other
  ;;
end
