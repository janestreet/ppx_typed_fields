open Base
open Ppxlib

type common_items =
  { upper : structure_item
  ; upper_rename : structure_item
  ; t_type_declaration : structure_item
  ; names : structure_item
  ; name : structure_item
  ; path : structure_item
  ; ord : structure_item
  ; type_ids : structure_item
  ; packed : structure_item
  }

(*Creates the common top level items between typed variants and types fields. *)
val common
  :  loc:location
  -> minimum_needed_parameters:(core_type * (variance * injectivity)) list
  -> core_type_params:core_type list
  -> ctype:core_type
  -> unique_id:label
  -> common_items
