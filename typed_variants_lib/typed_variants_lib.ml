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

  module Type_ids = struct
    let to_type_id : type a. a t -> a Type_equal.Id.t = unreachable_code
  end

  module Packed = struct
    type 'a field = 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@unboxed]

    let all = []
    let pack : type a. a field -> t = unreachable_code
    let compare { f = T x1 } { f = T x2 } = List.compare Int.compare (__ord x1) (__ord x2)
    let equal t1 t2 = compare t1 t2 = 0

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
  end

  let which : derived_on -> Packed.t = function
    | (_ : derived_on) -> .
  ;;
end

module Singleton (T : T) = struct
  include Typed_fields_lib.Singleton (T)

  let get (type a) (T : a t) (t : derived_on) : a option = Some t
  let create (type a) (T : a t) (t : a) : derived_on = t
  let which _ = { Packed.f = Packed.T T }
end

module Singleton1 (T1 : T1) = struct
  include Typed_fields_lib.Singleton1 (T1)

  let get (type a r) (T : (a, r) t) (t : a derived_on) : r option = Some t
  let create (type a r) (T : (a, r) t) (t : r) : a derived_on = t
  let which _ = { Packed.f = Packed.T T }
end

module Singleton2 (T2 : T2) = struct
  include Typed_fields_lib.Singleton2 (T2)

  let get (type a b r) (T : (a, b, r) t) (t : (a, b) derived_on) : r option = Some t
  let create (type a b r) (T : (a, b, r) t) (t : r) : (a, b) derived_on = t
  let which _ = { Packed.f = Packed.T T }
end

module Singleton3 (T3 : T3) = struct
  include Typed_fields_lib.Singleton3 (T3)

  let get (type a b c r) (T : (a, b, c, r) t) (t : (a, b, c) derived_on) : r option =
    Some t
  ;;

  let create (type a b c r) (T : (a, b, c, r) t) (t : r) : (a, b, c) derived_on = t
  let which _ = { Packed.f = Packed.T T }
end

module Singleton4 (T4 : sig
    type ('a, 'b, 'c, 'd) t
  end) =
struct
  include Typed_fields_lib.Singleton4 (T4)

  let get (type a b c d r) (T : (a, b, c, d, r) t) (t : (a, b, c, d) derived_on)
    : r option
    =
    Some t
  ;;

  let create (type a b c d r) (T : (a, b, c, d, r) t) (t : r) : (a, b, c, d) derived_on =
    t
  ;;

  let which _ = { Packed.f = Packed.T T }
end

module Singleton5 (T5 : sig
    type ('a, 'b, 'c, 'd, 'e) t
  end) =
struct
  include Typed_fields_lib.Singleton5 (T5)

  let get (type a b c d e r) (T : (a, b, c, d, e, r) t) (t : (a, b, c, d, e) derived_on)
    : r option
    =
    Some t
  ;;

  let create (type a b c d e r) (T : (a, b, c, d, e, r) t) (t : r)
    : (a, b, c, d, e) derived_on
    =
    t
  ;;

  let which _ = { Packed.f = Packed.T T }
end
