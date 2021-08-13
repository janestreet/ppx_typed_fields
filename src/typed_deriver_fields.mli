open Base
open Ppxlib

(* Generate a Typed_fields(_t | of_x) structure item given a specific implementation
   module for how to handle the specific conversions like how the names for the
   typed_fields constructors are determined and how setter/getter functions work.*)
val gen_str
  :  (module Type_kind_intf.S with type t = 'a)
  -> original_type:core_type option
  -> original_kind:type_kind
  -> loc:location
  -> elements_to_convert:('a * Type_kind_intf.granularity) list
  -> params:(core_type * (variance * injectivity)) list
  -> structure_item list

include Typed_deriver_intf.S
