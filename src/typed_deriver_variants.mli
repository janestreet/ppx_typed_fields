open! Base
open Ppxlib

(** Generates the anonymous records and gives them a concrete name. e.g. (Also attaches
    [@@deriving typed_fields] if needed.)

    {[
      type rgb =
        { r : int
        ; g : int
        ; b : int
        }

      type 'x rgbx =
        { r : int
        ; g : int
        ; b : int
        ; x : 'x
        }
      [@@deriving typed_fields]
    ]} *)
val generate_anonymous_records_sig
  :  loc:location
  -> elements_to_convert:Variant_kind_generator.supported_constructor_declaration list
  -> signature_item list

(** Generates the anonymous records and gives them a concrete name. e.g. (Also attaches
    [@@deriving typed_fields] if needed.)

    {[
      type rgb =
        { r : int
        ; g : int
        ; b : int
        }

      type 'x rgbx =
        { r : int
        ; g : int
        ; b : int
        ; x : 'x
        }
    ]} *)
val generate_anonymous_records_str
  :  loc:location
  -> elements_to_convert:Variant_kind_generator.supported_constructor_declaration list
  -> structure_item list

(** Generates the tuples module and gives them a concrete name. e.g. (Also attaches
    [@@deriving typed_fields] if needed.)

    {[
      type rgb = int * int * string
      type 'x rgbx = 'x * float * 'x [@@deriving typed_fields]
    ]} *)
val generate_tuples_sig
  :  loc:location
  -> elements_to_convert:Variant_kind_generator.supported_constructor_declaration list
  -> signature_item list

(** Generates the tuples module and gives them a concrete name. e.g. (Also attaches
    [@@deriving typed_fields] if needed.)

    {[
      type rgb = int * int * string
      type 'x rgbx = 'x * float * 'x [@@deriving typed_fields]
    ]} *)
val generate_tuples_str
  :  loc:location
  -> elements_to_convert:Variant_kind_generator.supported_constructor_declaration list
  -> structure_item list

(* Generate a Typed_fields(_t | of_x) structure item given a specific implementation
   module for how to handle the specific conversions like how the names for the
   typed_fields constructors are determined and how setter/getter functions work.*)
val gen_str
  :  (module Variant_kind_generator.S)
  -> original_type:core_type option
  -> original_kind:type_kind
  -> loc:location
  -> elements_to_convert:Variant_kind_generator.supported_constructor_declaration list
  -> expand_typed_variants:
       (loc:location -> rec_flag -> type_declaration list -> module_expr)
  -> params:(core_type * (variance * injectivity)) list
  -> td_case:Variant_kind_generator.type_case
  -> structure_item list

(* Generates packed with value type, e.g.

   type ('a, 'b, 'c, 'd) packed_with_value =
   | T : ('a, 'b, 'c, 'd, 'r) t * 'r -> ('a, 'b, 'c, 'd) packed_with_value
*)
val generate_packed_with_value_type
  :  loc:location
  -> params:(core_type * (variance * injectivity)) list
  -> core_type_params:core_type list
  -> unique_parameter_id:label
  -> type_declaration

include Typed_deriver.S
