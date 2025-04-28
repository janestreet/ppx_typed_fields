(*$
  open Base
  open Stdio
  open Typed_fields_lib_cinaps
$*)

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
  let create_local ({ f = _ } : creator) : derived_on = ()

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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

module Common = Typed_common_lib_intf

(*$
  for n = 0 to 5 do
    [%string
      {|

module %{this n "Singleton"} (%{this n "T"} : sig
      type %{params n "'t%i"} t
    end) =
struct
  type nonrec %{params n "'t%i"} derived_on = %{params n "'t%i"} %{this n "T"}.t

  type (%{each n "'t%i,"} 'r) t =
    | T : (%{each n "'t%i,"} %{params n "'t%i"} %{this n "T"}.t) t

  type %{params n "'t%i"} creator = { f : 'a. (%{each n "'t%i,"} 'a) t -> 'a }

  let names = [ "this" ]
  let name _ = "this"
  let path _ = []
  let __ord _ = [ 0 ]

  let get
      (type %{each n "t%i "} a)
      (T : (%{each n "t%i,"} a) t)
      (t : %{params n "t%i"} derived_on)
      : a
    = t
  ;;

  let set
      (type %{each n "t%i "} a)
      (T : (%{each n "t%i,"} a) t)
      (_ : %{params n "t%i"} derived_on)
      (t : a)
      : %{params n "t%i"} derived_on
    = t
  ;;

  let create { f } = f T
  let create_local { f } = f T

  module Type_ids %{each n "(Type_id_T%i : T)"} = struct
    let type_id : %{params n "Type_id_T%i.t"} %{this n "T"}.t Type_equal.Id.t =
      Type_equal.Id.create ~name:"this" (fun _ -> Sexp.Atom "<opaque>")
    ;;

    let type_id (type a) (T : (%{each n "Type_id_T%i.t,"} a) t)
      : a Type_equal.Id.t
      = type_id
    ;;
  end

  module Packed = struct
    type (%{each n "'t%i,"} 'r) field = (%{each n "'t%i,"} 'r) t
    type %{params n "'t%i"} t' = T : (%{each n "'t%i,"} 'r) field -> %{params n "'t%i"} t'
    type t = { f : %{poly n "'t%i"} %{params n "'t%i"} t' } [@@unboxed]

    let compare _ _ = 0
    let equal _ _ = true
    let all = [ { f = T T } ]
    let sexp_of_t _ = Sexp.Atom "this"
    let t_of_sexp _ = { f = T T}
    let pack _ = { f = T T }

    include Comparator.Make (struct
        type nonrec t = t
        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

      |}]
    |> print_endline
  done
*)

module Singleton (T : sig
    type t
  end) =
struct
  type nonrec derived_on = T.t
  type 'r t = T : T.t t
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
    type 'r field = 'r t
    type t' = T : 'r field -> t'
    type t = { f : t' } [@@unboxed]

    let compare _ _ = 0
    let equal _ _ = true
    let all = [ { f = T T } ]
    let sexp_of_t _ = Sexp.Atom "this"
    let t_of_sexp _ = { f = T T }
    let pack _ = { f = T T }

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

module Singleton1 (T1 : sig
    type 't1 t
  end) =
struct
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

module Singleton2 (T2 : sig
    type ('t1, 't2) t
  end) =
struct
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

  module Type_ids (Type_id_T1 : T) (Type_id_T2 : T) = struct
    let type_id : (Type_id_T1.t, Type_id_T2.t) T2.t Type_equal.Id.t =
      Type_equal.Id.create ~name:"this" (fun _ -> Sexp.Atom "<opaque>")
    ;;

    let type_id (type a) (T : (Type_id_T1.t, Type_id_T2.t, a) t) : a Type_equal.Id.t =
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

module Singleton3 (T3 : sig
    type ('t1, 't2, 't3) t
  end) =
struct
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

module Singleton4 (T4 : sig
    type ('t1, 't2, 't3, 't4) t
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

module Singleton5 (T5 : sig
    type ('t1, 't2, 't3, 't4, 't5) t
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

(*$
  for n = 1 to 5 do
    [%string
      {|

module %{this n "S_of_S"} (M : %{this n "S"}) %{each n "(T%i : T)"} : S
 with type 'a t = (%{each n "T%i.t,"} 'a) M.t
  and type derived_on = %{params n "T%i.t"} M.derived_on =
struct
  include M

  type 'a t = (%{each n "T%i.t,"} 'a) M.t
  type derived_on = %{params n "T%i.t"} M.derived_on
  type creator = { f : 'a. 'a t -> 'a }

  let create ({ f } : creator) =
    let m_creator_f : type a. (%{each n "T%i.t,"} a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create m_creator
  ;;

  let create_local (local_ { f } : creator) = exclave_
    let m_creator_f : type a. (%{each n "T%i.t,"} a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create_local m_creator
  ;;

  module Type_ids = Type_ids %{each n "(T%i)"}

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

    include Comparator.Make (struct
        type nonrec t = t
        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

      |}]
    |> print_endline
  done
*)

module S_of_S1 (M : S1) (T1 : T) :
  S with type 'a t = (T1.t, 'a) M.t and type derived_on = T1.t M.derived_on = struct
  include M

  type 'a t = (T1.t, 'a) M.t
  type derived_on = T1.t M.derived_on
  type creator = { f : 'a. 'a t -> 'a }

  let create ({ f } : creator) =
    let m_creator_f : type a. (T1.t, a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create m_creator
  ;;

  let create_local ({ f } : creator) =
    let m_creator_f : type a. (T1.t, a) M.t -> a = fun field -> f field in
    let m_creator = { M.f = m_creator_f } in
    M.create_local m_creator
  ;;

  module Type_ids = Type_ids (T1)

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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
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

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end
end

(*$*)

module Private = struct
  let list_to_sexp = function
    | [] -> assert false
    | [ some_sexp ] -> some_sexp
    | other -> Sexp.List other
  ;;
end
