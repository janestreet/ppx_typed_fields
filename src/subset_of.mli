open Ppxlib
open Type_kind_intf

val generate_str
  :  loc:location
  -> typ_name:label
  -> fields:(label_declaration * granularity) list
  -> params:(core_type * 'a) list
  -> super:longident
  -> structure_item

val generate_sig
  :  loc:location
  -> typ_name:label
  -> params:(core_type * 'a) list
  -> super:longident
  -> signature_item
