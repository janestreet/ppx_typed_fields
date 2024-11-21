open! Base

(* Creates sexper functions for a typed_field_map. *)
module Make (Map : The_map_intf.S_plain) : sig
  type to_sexper = { f : 'a. 'a Map.Key.t -> 'a Map.Data.t -> Sexp.t }
  type of_sexper = { f : 'a. 'a Map.Key.t -> Sexp.t -> 'a Map.Data.t }
  type defaulter = { f : 'a. 'a Map.Key.t -> 'a Map.Data.t option }

  val sexp_of_t : to_sexper -> Map.t -> Sexp.t
  val t_of_sexp : ?default:defaulter -> of_sexper -> Sexp.t -> Map.t
end
