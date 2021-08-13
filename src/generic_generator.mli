open Base
open Ppxlib

(* Generates top level type definitions
   `type record = t` and
   `type _ t = A : a |  B : b ...`  *)
val gen_t
  :  loc:location
  -> original_type:core_type option
  -> original_kind:type_kind
  -> elements_to_convert:('a * Type_kind_intf.granularity) list
  -> generate_constructors:
       (loc:location
        -> elements_to_convert:('a * Type_kind_intf.granularity) list
        -> core_type_params:core_type list
        -> (('a * Type_kind_intf.granularity) * constructor_declaration) list)
  -> params:(core_type * (variance * injectivity)) list
  -> upper_name:label
  -> 'a Type_kind_intf.gen_t_result

(* Generates type ('t1, 't2, 't3, ..., 'result) t *)
val gen_sig_t
  :  loc:location
  -> params:(core_type * (variance * injectivity)) list
  -> signature_item

(* Generates a signature for an opaque type. (e.g. type ('a, 'b, 'c) inner_weird)
   The parameter name is the name of the type (e.g. inner weird), and the
   params are the type parameters of the type (e.g. ('a, 'b, 'c))
*)
val opaque_signature
  :  (module Typed_deriver_intf.S)
  -> loc:location
  -> manifest_type:core_type option
  -> original_kind:type_kind
  -> params:(core_type * (variance * injectivity)) list
  -> module_type
