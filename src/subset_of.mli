open! Base
open Ppxlib

val generate_str
  :  loc:location
  -> typ_name:label
  -> fields:(label_declaration * Type_kind.granularity) list
  -> params:(core_type * (variance * injectivity)) list
  -> super:longident
  -> structure_item

val generate_sig
  :  loc:location
  -> typ_name:label
  -> params:(core_type * (variance * injectivity)) list
  -> super:longident
  -> signature_item
