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
    `Typed_fields` module. *)

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
  include Typed_common_lib_intf.%{this n "S"}

  type %{params n "'t%i"} creator = { f : 'a. (%{each n "'t%i,"} 'a) t -> 'a }

  val get : (%{each n "'t%i,"} 'a) t -> %{params n "'t%i"} derived_on -> 'a

  val set
    :  (%{each n "'t%i,"} 'a) t
    -> %{params n "'t%i"} derived_on
    -> 'a
    -> %{params n "'t%i"} derived_on

  val create : %{params n "'t%i"} creator -> %{params n "'t%i"} derived_on

  val create_local
    :  local_ %{params n "'t%i"} creator
    -> local_ %{params n "'t%i"} derived_on
end

    |}]
    |> print_endline
  done
*)

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

(*$*)

module type Typed_fields_lib = sig
  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3
  module type S4 = S4
  module type S5 = S5

  (*$
    for n = 1 to 5 do
      [%string
        {|

  module %{this n "S_of_S"} (M : %{this n "S"}) %{each n "(T%i : T)"} : S
    with type 'a t = (%{each n "T%i.t,"} 'a) M.t
     and type derived_on = %{params n "T%i.t"} M.derived_on

      |}]
      |> print_endline
    done
  *)

  module S_of_S1 (M : S1) (T1 : T) :
    S with type 'a t = (T1.t, 'a) M.t and type derived_on = T1.t M.derived_on

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

  (*$
    for n = 0 to 5 do
      [%string
        {|

  module %{this n "Singleton"} (%{this n "T"} : sig
      type %{params n "'t%i"} t
    end) :
  sig
    type (%{each n "'t%i,"} 'r) t =
      | T : (%{each n "'t%i,"} %{params n "'t%i"} %{this n "T"}.t) t

    include %{this n "S"}
      with type %{params n "'t%i"} derived_on = %{params n "'t%i"} %{this n "T"}.t
       and type (%{each n "'t%i,"} 'r) t := (%{each n "'t%i,"} 'r) t
  end

      |}]
      |> print_endline
    done
  *)

  module Singleton (T : sig
      type t
    end) : sig
    type 'r t = T : T.t t

    include S with type derived_on = T.t and type 'r t := 'r t
  end

  module Singleton1 (T1 : sig
      type 't1 t
    end) : sig
    type ('t1, 'r) t = T : ('t1, 't1 T1.t) t

    include S1 with type 't1 derived_on = 't1 T1.t and type ('t1, 'r) t := ('t1, 'r) t
  end

  module Singleton2 (T2 : sig
      type ('t1, 't2) t
    end) : sig
    type ('t1, 't2, 'r) t = T : ('t1, 't2, ('t1, 't2) T2.t) t

    include
      S2
      with type ('t1, 't2) derived_on = ('t1, 't2) T2.t
       and type ('t1, 't2, 'r) t := ('t1, 't2, 'r) t
  end

  module Singleton3 (T3 : sig
      type ('t1, 't2, 't3) t
    end) : sig
    type ('t1, 't2, 't3, 'r) t = T : ('t1, 't2, 't3, ('t1, 't2, 't3) T3.t) t

    include
      S3
      with type ('t1, 't2, 't3) derived_on = ('t1, 't2, 't3) T3.t
       and type ('t1, 't2, 't3, 'r) t := ('t1, 't2, 't3, 'r) t
  end

  module Singleton4 (T4 : sig
      type ('t1, 't2, 't3, 't4) t
    end) : sig
    type ('t1, 't2, 't3, 't4, 'r) t =
      | T : ('t1, 't2, 't3, 't4, ('t1, 't2, 't3, 't4) T4.t) t

    include
      S4
      with type ('t1, 't2, 't3, 't4) derived_on = ('t1, 't2, 't3, 't4) T4.t
       and type ('t1, 't2, 't3, 't4, 'r) t := ('t1, 't2, 't3, 't4, 'r) t
  end

  module Singleton5 (T5 : sig
      type ('t1, 't2, 't3, 't4, 't5) t
    end) : sig
    type ('t1, 't2, 't3, 't4, 't5, 'r) t =
      | T : ('t1, 't2, 't3, 't4, 't5, ('t1, 't2, 't3, 't4, 't5) T5.t) t

    include
      S5
      with type ('t1, 't2, 't3, 't4, 't5) derived_on = ('t1, 't2, 't3, 't4, 't5) T5.t
       and type ('t1, 't2, 't3, 't4, 't5, 'r) t := ('t1, 't2, 't3, 't4, 't5, 'r) t
  end

  (*$*)

  (** This is a convenient module for deriving typed_fields on unit, which you can
      conceptually think of as a record with no fields. OCaml does not support actual
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
