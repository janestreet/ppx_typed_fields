open Ppxlib

val variants : Deriving.t

module For_testing : sig
  val expand_struct
    :  loc:location
    -> rec_flag * type_declaration list
    -> structure_item list

  val expand_sig : loc:location -> rec_flag * type_declaration list -> signature_item list

  val expand_anonymous_struct
    :  loc:location
    -> rec_flag
    -> type_declaration list
    -> module_expr
end
