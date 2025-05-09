(*$
  open Base
  open Stdio
  open Typed_fields_lib_cinaps
$*)

open Base
include Typed_variants_lib_intf

module Nothing = struct
  type nonrec derived_on = |
  type _ t = |

  let unreachable_code = function
    | (_ : _ t) -> .
  ;;

  let names = []
  let name : type a. a t -> string = unreachable_code
  let path : type a. a t -> string list = unreachable_code
  let __ord : type a. a t -> int list = unreachable_code
  let get : type a. a t -> derived_on -> a option = unreachable_code
  let create : type a. a t -> a -> derived_on = unreachable_code
  let globalize0 = unreachable_code
  let globalize _ t = globalize0 t

  module Type_ids = struct
    let type_id : type a. a t -> a Type_equal.Id.t = unreachable_code
  end

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let all = []
    let pack : type a. a field -> t = unreachable_code
    let pack__local : type a. a field @ local -> t = unreachable_code
    let globalize { f = T field } = { f = T (globalize0 field) }
    let compare { f = T x1 } { f = T x2 } = List.compare Int.compare (__ord x1) (__ord x2)

    let compare__local { f = T x1 } { f = T x2 } =
      List.compare__local Int.compare__local (__ord x1) (__ord x2)
    ;;

    let equal t1 t2 = compare t1 t2 = 0
    let equal__local t1 t2 = compare__local t1 t2 = 0

    let sexp_of_t__local packed =
      match packed with
      | (_ : t) -> .
    ;;

    let sexp_of_t packed =
      match packed with
      | (_ : t) -> .
    ;;

    let t_of_sexp sexp =
      raise_s
        (Sexp.List
           [ Sexp.Atom "Nothing has no constructors, so cannot convert to variant."
           ; sexp
           ])
    ;;

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end

  let which : derived_on -> Packed.t = function
    | (_ : derived_on) -> .
  ;;
end

(*$
  for n = 0 to 5 do
    [%string
      {|

module %{this n "Singleton"} (%{this n "T"} : sig
    type %{params n "'t%i"} t
  end) =
struct
  include Typed_fields_lib.%{this n "Singleton"} (%{this n "T"})

  let get
      (type %{each n "t%i "} r)
      (T : (%{each n "t%i,"} r) t @@ local)
      (t : %{params n "t%i"} derived_on)
      : r option
    = Some t
  ;;

  let create
      (type %{each n "t%i "} r)
      (T : (%{each n "t%i,"} r) t @@ local)
      (t : r)
      : %{params n "t%i"} derived_on
    = t
  ;;

  let which _ = { Packed.f = Packed.T T }
end

      |}]
    |> print_endline
  done
*)

module Singleton (T : sig
    type t
  end) =
struct
  include Typed_fields_lib.Singleton (T)

  let get (type r) (T : r t @@ local) (t : derived_on) : r option = Some t
  let create (type r) (T : r t @@ local) (t : r) : derived_on = t
  let which _ = { Packed.f = Packed.T T }
end

module Singleton1 (T1 : sig
    type 't1 t
  end) =
struct
  include Typed_fields_lib.Singleton1 (T1)

  let get (type t1 r) (T : (t1, r) t @@ local) (t : t1 derived_on) : r option = Some t
  let create (type t1 r) (T : (t1, r) t @@ local) (t : r) : t1 derived_on = t
  let which _ = { Packed.f = Packed.T T }
end

module Singleton2 (T2 : sig
    type ('t1, 't2) t
  end) =
struct
  include Typed_fields_lib.Singleton2 (T2)

  let get (type t1 t2 r) (T : (t1, t2, r) t @@ local) (t : (t1, t2) derived_on) : r option
    =
    Some t
  ;;

  let create (type t1 t2 r) (T : (t1, t2, r) t @@ local) (t : r) : (t1, t2) derived_on = t
  let which _ = { Packed.f = Packed.T T }
end

module Singleton3 (T3 : sig
    type ('t1, 't2, 't3) t
  end) =
struct
  include Typed_fields_lib.Singleton3 (T3)

  let get (type t1 t2 t3 r) (T : (t1, t2, t3, r) t @@ local) (t : (t1, t2, t3) derived_on)
    : r option
    =
    Some t
  ;;

  let create (type t1 t2 t3 r) (T : (t1, t2, t3, r) t @@ local) (t : r)
    : (t1, t2, t3) derived_on
    =
    t
  ;;

  let which _ = { Packed.f = Packed.T T }
end

module Singleton4 (T4 : sig
    type ('t1, 't2, 't3, 't4) t
  end) =
struct
  include Typed_fields_lib.Singleton4 (T4)

  let get
    (type t1 t2 t3 t4 r)
    (T : (t1, t2, t3, t4, r) t @@ local)
    (t : (t1, t2, t3, t4) derived_on)
    : r option
    =
    Some t
  ;;

  let create (type t1 t2 t3 t4 r) (T : (t1, t2, t3, t4, r) t @@ local) (t : r)
    : (t1, t2, t3, t4) derived_on
    =
    t
  ;;

  let which _ = { Packed.f = Packed.T T }
end

module Singleton5 (T5 : sig
    type ('t1, 't2, 't3, 't4, 't5) t
  end) =
struct
  include Typed_fields_lib.Singleton5 (T5)

  let get
    (type t1 t2 t3 t4 t5 r)
    (T : (t1, t2, t3, t4, t5, r) t @@ local)
    (t : (t1, t2, t3, t4, t5) derived_on)
    : r option
    =
    Some t
  ;;

  let create (type t1 t2 t3 t4 t5 r) (T : (t1, t2, t3, t4, t5, r) t @@ local) (t : r)
    : (t1, t2, t3, t4, t5) derived_on
    =
    t
  ;;

  let which _ = { Packed.f = Packed.T T }
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

  let globalize _ t = globalize0 t

  module Type_ids = Type_ids %{each n "(T%i)"}

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let m_of_packed__local { f = T field } = exclave_ M.Packed.pack__local field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)

    let compare__local a b =
      M.Packed.compare__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)

    let equal__local a b =
      M.Packed.equal__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let sexp_of_t__local t = exclave_ M.Packed.sexp_of_t__local (m_of_packed__local t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let globalize { f = T field } = { f = T (globalize0 field) }
    let pack field = { f = T field }
    let pack__local field = exclave_ { f = T field }

    include Comparator.Make (struct
        type nonrec t = t
        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end

  let which t = Packed.packed_of_m (M.which t)
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

  let globalize _ t = globalize0 t

  module Type_ids = Type_ids (T1)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let m_of_packed__local { f = T field } = exclave_ M.Packed.pack__local field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)

    let compare__local a b =
      M.Packed.compare__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)

    let equal__local a b =
      M.Packed.equal__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let sexp_of_t__local t = exclave_ M.Packed.sexp_of_t__local (m_of_packed__local t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let globalize { f = T field } = { f = T (globalize0 field) }
    let pack field = { f = T field }
    let pack__local field = exclave_ { f = T field }

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end

  let which t = Packed.packed_of_m (M.which t)
end

module S_of_S2 (M : S2) (T1 : T) (T2 : T) :
  S with type 'a t = (T1.t, T2.t, 'a) M.t and type derived_on = (T1.t, T2.t) M.derived_on =
struct
  include M

  type 'a t = (T1.t, T2.t, 'a) M.t
  type derived_on = (T1.t, T2.t) M.derived_on

  let globalize _ t = globalize0 t

  module Type_ids = Type_ids (T1) (T2)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let m_of_packed__local { f = T field } = exclave_ M.Packed.pack__local field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)

    let compare__local a b =
      M.Packed.compare__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)

    let equal__local a b =
      M.Packed.equal__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let sexp_of_t__local t = exclave_ M.Packed.sexp_of_t__local (m_of_packed__local t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let globalize { f = T field } = { f = T (globalize0 field) }
    let pack field = { f = T field }
    let pack__local field = exclave_ { f = T field }

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
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

  let globalize _ t = globalize0 t

  module Type_ids = Type_ids (T1) (T2) (T3)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let m_of_packed__local { f = T field } = exclave_ M.Packed.pack__local field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)

    let compare__local a b =
      M.Packed.compare__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)

    let equal__local a b =
      M.Packed.equal__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let sexp_of_t__local t = exclave_ M.Packed.sexp_of_t__local (m_of_packed__local t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let globalize { f = T field } = { f = T (globalize0 field) }
    let pack field = { f = T field }
    let pack__local field = exclave_ { f = T field }

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
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

  let globalize _ t = globalize0 t

  module Type_ids = Type_ids (T1) (T2) (T3) (T4)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let m_of_packed__local { f = T field } = exclave_ M.Packed.pack__local field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)

    let compare__local a b =
      M.Packed.compare__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)

    let equal__local a b =
      M.Packed.equal__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let sexp_of_t__local t = exclave_ M.Packed.sexp_of_t__local (m_of_packed__local t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let globalize { f = T field } = { f = T (globalize0 field) }
    let pack field = { f = T field }
    let pack__local field = exclave_ { f = T field }

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
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

  let globalize _ t = globalize0 t

  module Type_ids = Type_ids (T1) (T2) (T3) (T4) (T5)

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let m_of_packed { f = T field } = M.Packed.pack field
    let m_of_packed__local { f = T field } = exclave_ M.Packed.pack__local field
    let packed_of_m { M.Packed.f = T field } = { f = T field }
    let compare a b = M.Packed.compare (m_of_packed a) (m_of_packed b)

    let compare__local a b =
      M.Packed.compare__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let equal a b = M.Packed.equal (m_of_packed a) (m_of_packed b)

    let equal__local a b =
      M.Packed.equal__local (m_of_packed__local a) (m_of_packed__local b) [@nontail]
    ;;

    let all = List.map M.Packed.all ~f:packed_of_m
    let sexp_of_t t = M.Packed.sexp_of_t (m_of_packed t)
    let sexp_of_t__local t = exclave_ M.Packed.sexp_of_t__local (m_of_packed__local t)
    let t_of_sexp sexp = packed_of_m (M.Packed.t_of_sexp sexp)
    let globalize { f = T field } = { f = T (globalize0 field) }
    let pack field = { f = T field }
    let pack__local field = exclave_ { f = T field }

    include Comparator.Make (struct
        type nonrec t = t

        let compare = compare
        let sexp_of_t = sexp_of_t
      end)
  end

  let which t = Packed.packed_of_m (M.which t)
end

(*$*)
