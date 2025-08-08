(** [@@deriving typed_fields] and [@@deriving typed_variants] will derive a module that
    satisfies Typed_fields_lib.S (or S1, S2, etc) or Typed_variants_lib.S (or S1, S2, etc)
    respectively.

    Each of them have many signature fields in common. In order to facilitate operations
    on both of them generically, this file contains the signature with the intersections
    of typed fields and typed variants. *)

(*$
  open Base
  open Stdio
  open Typed_fields_lib_cinaps
$*)

open Base

(*$
  for n = 0 to 5 do
    [%string
      {|

module type %{this n "S"} = sig
  type (%{each n "'t%i,"} 'a) t [@@deriving globalize]
  type %{params n "'t%i"} derived_on

  val names : string list

  (** The name of the field, e.g. "rgb" from the example above. *)
  val name : _ t @ local -> string

  (** The path of a field, e.g. ["rgb"] from the example above.
      The list will have multiple elements if the field is a subproduct.  *)
  val path : _ t @ local -> string list

  (** Globalize without extra parameters. *)
  val globalize0 : (%{each n "'t%i,"} 'a) t @ local -> (%{each n "'t%i,"} 'a) t

  val __ord : _ t @ local -> int list

  module Type_ids %{each n "(T%i : T)"} : sig
    val type_id : (%{each n "T%i.t,"} 'a) t @ local -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type (%{each n "'t%i,"} 'a) field := (%{each n "'t%i,"} 'a) t
    type %{params n "'t%i"} t' = T : (%{each n "'t%i,"} 'a) field -> %{params n "'t%i"} t'

    type t = { f : %{poly n "'t%i"} %{params n "'t%i"} t' }
    [@@deriving compare ~localize, enumerate, equal ~localize, globalize, sexp ~stackify]
    [@@unboxed]

    include Comparator.S with type t := t

    val%template pack : (%{each n "'t%i,"} 'a) field @ m -> t @ m
    [@@mode m = (local, global)]
  end
end

    |}]
    |> print_endline
  done
*)

module type S = sig
  type 'a t [@@deriving globalize]
  type derived_on

  val names : string list

  (** The name of the field, e.g. "rgb" from the example above. *)
  val name : _ t -> string

  (** The path of a field, e.g. ["rgb"] from the example above. The list will have
      multiple elements if the field is a subproduct. *)
  val path : _ t -> string list

  (** Globalize without extra parameters. *)
  val globalize0 : 'a t -> 'a t

  val __ord : _ t -> int list

  module Type_ids : sig
    val type_id : 'a t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type 'a field := 'a t
    type t' = T : 'a field -> t'

    type t = { f : t' }
    [@@deriving compare ~localize, enumerate, equal ~localize, globalize, sexp ~stackify]
    [@@unboxed]

    include Comparator.S with type t := t

    val%template pack : 'a field -> t [@@mode m = (local, global)]
  end
end

module type S1 = sig
  type ('t1, 'a) t [@@deriving globalize]
  type 't1 derived_on

  val names : string list

  (** The name of the field, e.g. "rgb" from the example above. *)
  val name : _ t -> string

  (** The path of a field, e.g. ["rgb"] from the example above. The list will have
      multiple elements if the field is a subproduct. *)
  val path : _ t -> string list

  (** Globalize without extra parameters. *)
  val globalize0 : ('t1, 'a) t -> ('t1, 'a) t

  val __ord : _ t -> int list

  module Type_ids (T1 : T) : sig
    val type_id : (T1.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 'a) field := ('t1, 'a) t
    type 't1 t' = T : ('t1, 'a) field -> 't1 t'

    type t = { f : 't1. 't1 t' }
    [@@deriving compare ~localize, enumerate, equal ~localize, globalize, sexp ~stackify]
    [@@unboxed]

    include Comparator.S with type t := t

    val%template pack : ('t1, 'a) field -> t [@@mode m = (local, global)]
  end
end

module type S2 = sig
  type ('t1, 't2, 'a) t [@@deriving globalize]
  type ('t1, 't2) derived_on

  val names : string list

  (** The name of the field, e.g. "rgb" from the example above. *)
  val name : _ t -> string

  (** The path of a field, e.g. ["rgb"] from the example above. The list will have
      multiple elements if the field is a subproduct. *)
  val path : _ t -> string list

  (** Globalize without extra parameters. *)
  val globalize0 : ('t1, 't2, 'a) t -> ('t1, 't2, 'a) t

  val __ord : _ t -> int list

  module Type_ids (T1 : T) (T2 : T) : sig
    val type_id : (T1.t, T2.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 't2, 'a) field := ('t1, 't2, 'a) t
    type ('t1, 't2) t' = T : ('t1, 't2, 'a) field -> ('t1, 't2) t'

    type t = { f : 't1 't2. ('t1, 't2) t' }
    [@@deriving compare ~localize, enumerate, equal ~localize, globalize, sexp ~stackify]
    [@@unboxed]

    include Comparator.S with type t := t

    val%template pack : ('t1, 't2, 'a) field -> t [@@mode m = (local, global)]
  end
end

module type S3 = sig
  type ('t1, 't2, 't3, 'a) t [@@deriving globalize]
  type ('t1, 't2, 't3) derived_on

  val names : string list

  (** The name of the field, e.g. "rgb" from the example above. *)
  val name : _ t -> string

  (** The path of a field, e.g. ["rgb"] from the example above. The list will have
      multiple elements if the field is a subproduct. *)
  val path : _ t -> string list

  (** Globalize without extra parameters. *)
  val globalize0 : ('t1, 't2, 't3, 'a) t -> ('t1, 't2, 't3, 'a) t

  val __ord : _ t -> int list

  module Type_ids (T1 : T) (T2 : T) (T3 : T) : sig
    val type_id : (T1.t, T2.t, T3.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 't2, 't3, 'a) field := ('t1, 't2, 't3, 'a) t
    type ('t1, 't2, 't3) t' = T : ('t1, 't2, 't3, 'a) field -> ('t1, 't2, 't3) t'

    type t = { f : 't1 't2 't3. ('t1, 't2, 't3) t' }
    [@@deriving compare ~localize, enumerate, equal ~localize, globalize, sexp ~stackify]
    [@@unboxed]

    include Comparator.S with type t := t

    val%template pack : ('t1, 't2, 't3, 'a) field -> t [@@mode m = (local, global)]
  end
end

module type S4 = sig
  type ('t1, 't2, 't3, 't4, 'a) t [@@deriving globalize]
  type ('t1, 't2, 't3, 't4) derived_on

  val names : string list

  (** The name of the field, e.g. "rgb" from the example above. *)
  val name : _ t -> string

  (** The path of a field, e.g. ["rgb"] from the example above. The list will have
      multiple elements if the field is a subproduct. *)
  val path : _ t -> string list

  (** Globalize without extra parameters. *)
  val globalize0 : ('t1, 't2, 't3, 't4, 'a) t -> ('t1, 't2, 't3, 't4, 'a) t

  val __ord : _ t -> int list

  module Type_ids (T1 : T) (T2 : T) (T3 : T) (T4 : T) : sig
    val type_id : (T1.t, T2.t, T3.t, T4.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 't2, 't3, 't4, 'a) field := ('t1, 't2, 't3, 't4, 'a) t

    type ('t1, 't2, 't3, 't4) t' =
      | T : ('t1, 't2, 't3, 't4, 'a) field -> ('t1, 't2, 't3, 't4) t'

    type t = { f : 't1 't2 't3 't4. ('t1, 't2, 't3, 't4) t' }
    [@@deriving compare ~localize, enumerate, equal ~localize, globalize, sexp ~stackify]
    [@@unboxed]

    include Comparator.S with type t := t

    val%template pack : ('t1, 't2, 't3, 't4, 'a) field -> t [@@mode m = (local, global)]
  end
end

module type S5 = sig
  type ('t1, 't2, 't3, 't4, 't5, 'a) t [@@deriving globalize]
  type ('t1, 't2, 't3, 't4, 't5) derived_on

  val names : string list

  (** The name of the field, e.g. "rgb" from the example above. *)
  val name : _ t -> string

  (** The path of a field, e.g. ["rgb"] from the example above. The list will have
      multiple elements if the field is a subproduct. *)
  val path : _ t -> string list

  (** Globalize without extra parameters. *)
  val globalize0 : ('t1, 't2, 't3, 't4, 't5, 'a) t -> ('t1, 't2, 't3, 't4, 't5, 'a) t

  val __ord : _ t -> int list

  module Type_ids (T1 : T) (T2 : T) (T3 : T) (T4 : T) (T5 : T) : sig
    val type_id : (T1.t, T2.t, T3.t, T4.t, T5.t, 'a) t -> 'a Type_equal.Id.t
  end

  (** Packed is useful for making collections of 'a t's with different 'a's. *)
  module Packed : sig
    type ('t1, 't2, 't3, 't4, 't5, 'a) field := ('t1, 't2, 't3, 't4, 't5, 'a) t

    type ('t1, 't2, 't3, 't4, 't5) t' =
      | T : ('t1, 't2, 't3, 't4, 't5, 'a) field -> ('t1, 't2, 't3, 't4, 't5) t'

    type t = { f : 't1 't2 't3 't4 't5. ('t1, 't2, 't3, 't4, 't5) t' }
    [@@deriving compare ~localize, enumerate, equal ~localize, globalize, sexp ~stackify]
    [@@unboxed]

    include Comparator.S with type t := t

    val%template pack : ('t1, 't2, 't3, 't4, 't5, 'a) field -> t
    [@@mode m = (local, global)]
  end
end

(*$*)

module type Typed_variants_lib = sig
  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3
  module type S4 = S4
  module type S5 = S5
end
