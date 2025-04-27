open! Base
open Ppxlib

val fields : Deriving.t

module For_testing : sig
  val expand_struct
    :  ?super:expression
    -> loc:location
    -> rec_flag * type_declaration list
    -> structure_item list

  val expand_sig
    :  ?super:expression
    -> loc:location
    -> rec_flag * type_declaration list
    -> signature_item list

  val expand_anonymous_struct
    :  loc:location
    -> rec_flag
    -> type_declaration list
    -> module_expr

  val expand_variant_struct
    :  loc:location
    -> rec_flag * type_declaration list
    -> structure_item list

  val expand_variant_sig
    :  loc:location
    -> rec_flag * type_declaration list
    -> signature_item list

  val expand_variant_anonymous_struct
    :  loc:location
    -> rec_flag
    -> type_declaration list
    -> module_expr
end
