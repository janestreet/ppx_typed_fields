(** [@@deriving typed_fields] and [@@deriving typed_variants] will derive a module that
    satisfies Typed_fields_lib.S (or S1, S2, etc) or Typed_variants_lib.S (or S1, S2, etc)
    respectively.

    Each of them have many signature fields in common. In order to facilitate
    operations on both of them generically, this file contains the signature with
    the intersections of typed fields and typed variants. *)

open Base

module type S = sig
  type 'a t
  type derived_on

  val names : string list

  (** The name of the field, e.g. "rgb" from the example above. *)
  val name : 'a t -> string

  (** The path of a field, e.g. ["rgb"] from the example above.
      The list will have multiple elements if the field is a subproduct.  *)
  val path : 'a t -> string list

  val __ord : 'a t -> int list

  module Type_ids : sig
    val type_id : 'a t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type 'a field := 'a t
    type t' = T : 'a field -> t'
    type t = { f : t' } [@@deriving sexp, enumerate, compare, equal] [@@unboxed]

    val pack : 'a field -> t
  end
end

module type S1 = sig
  type ('t1, 'a) t
  type 't1 derived_on

  val names : string list
  val name : _ t -> string
  val path : _ t -> string list
  val __ord : _ t -> int list

  (** The name of the field, e.g. "rgb" from the example at the top of the page. *)
  module Type_ids (T : T) : sig
    val type_id : (T.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 'a) field := ('t1, 'a) t
    type 't1 t' = T : ('t1, 'a) field -> 't1 t'
    type t = { f : 't1. 't1 t' } [@@deriving compare, equal] [@@unboxed]

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val all : t list
    val pack : ('t1, 'a) field -> t
  end
end

module type S2 = sig
  type ('t1, 't2, 'a) t
  type ('t1, 't2) derived_on

  val names : string list
  val name : _ t -> string
  val path : _ t -> string list
  val __ord : _ t -> int list

  (** The name of the field, e.g. "rgb" from the example at the top of the page. *)
  module Type_ids (T1 : T) (T2 : T) : sig
    val type_id : (T1.t, T2.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 't2, 'a) field := ('t1, 't2, 'a) t
    type ('t1, 't2) t' = T : ('t1, 't2, 'a) field -> ('t1, 't2) t'
    type t = { f : 't1 't2. ('t1, 't2) t' } [@@deriving compare, equal] [@@unboxed]

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val all : t list
    val pack : ('t1, 't2, 'a) field -> t
  end
end

module type S3 = sig
  type ('t1, 't2, 't3, 'a) t
  type ('t1, 't2, 't3) derived_on

  val names : string list
  val name : _ t -> string
  val path : _ t -> string list
  val __ord : _ t -> int list

  (** The name of the field, e.g. "rgb" from the example at the top of the page. *)
  module Type_ids (T1 : T) (T2 : T) (T3 : T) : sig
    val type_id : (T1.t, T2.t, T3.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 't2, 't3, 'a) field := ('t1, 't2, 't3, 'a) t
    type ('t1, 't2, 't3) t' = T : ('t1, 't2, 't3, 'a) field -> ('t1, 't2, 't3) t'

    type t = { f : 't1 't2 't3. ('t1, 't2, 't3) t' }
    [@@deriving compare, equal] [@@unboxed]

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val all : t list
    val pack : ('t1, 't2, 't3, 'a) field -> t
  end
end

module type S4 = sig
  type ('t1, 't2, 't3, 't4, 'a) t
  type ('t1, 't2, 't3, 't4) derived_on

  val names : string list
  val name : _ t -> string
  val path : _ t -> string list
  val __ord : _ t -> int list

  (** The name of the field, e.g. "rgb" from the example at the top of the page. *)
  module Type_ids (T1 : T) (T2 : T) (T3 : T) (T4 : T) : sig
    val type_id : (T1.t, T2.t, T3.t, T4.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 't2, 't3, 't4, 'a) field := ('t1, 't2, 't3, 't4, 'a) t

    type ('t1, 't2, 't3, 't4) t' =
      | T : ('t1, 't2, 't3, 't4, 'a) field -> ('t1, 't2, 't3, 't4) t'

    type t = { f : 't1 't2 't3 't4. ('t1, 't2, 't3, 't4) t' }
    [@@deriving compare, equal] [@@unboxed]

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val all : t list
    val pack : ('t1, 't2, 't3, 't4, 'a) field -> t
  end
end

module type S5 = sig
  type ('t1, 't2, 't3, 't4, 't5, 'a) t
  type ('t1, 't2, 't3, 't4, 't5) derived_on

  val names : string list
  val name : _ t -> string
  val path : _ t -> string list
  val __ord : _ t -> int list

  (** The name of the field, e.g. "rgb" from the example at the top of the page. *)
  module Type_ids (T1 : T) (T2 : T) (T3 : T) (T4 : T) (T5 : T) : sig
    val type_id : (T1.t, T2.t, T3.t, T4.t, T5.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 't2, 't3, 't4, 't5, 'a) field := ('t1, 't2, 't3, 't4, 't5, 'a) t

    type ('t1, 't2, 't3, 't4, 't5) t' =
      | T : ('t1, 't2, 't3, 't4, 't5, 'a) field -> ('t1, 't2, 't3, 't4, 't5) t'

    type t = { f : 't1 't2 't3 't4 't5. ('t1, 't2, 't3, 't4, 't5) t' }
    [@@deriving compare, equal] [@@unboxed]

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val all : t list
    val pack : ('t1, 't2, 't3, 't4, 't5, 'a) field -> t
  end
end

module type Typed_variants_lib = sig
  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3
  module type S4 = S4
  module type S5 = S5
end
