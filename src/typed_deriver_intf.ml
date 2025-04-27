open! Base
open Ppxlib

module Definitions = struct
  module type S = sig
    (** Either generates either [include Typed_fields_lib.SN with type record := record]
        or the fully generated partial signature if the number of parameter is above 5. *)
    val generate_include_signature_for_opaque
      :  loc:location
      -> params:(core_type * (variance * injectivity)) list
      -> signature_item list

    (** Either generates either
        [include Typed_fields_lib.SN with type record := record and type t := t] or the
        fully generated partial signature if the number of parameter is above 5. *)
    val generate_include_signature
      :  loc:location
      -> params:(core_type * (variance * injectivity)) list
      -> signature_item list
  end
end

module type Typed_deriver = sig
  include module type of struct
    include Definitions
  end

  val generate_packed_field_type_declaration
    :  loc:Location.t
    -> params:(core_type * (variance * injectivity)) list
    -> unique_parameter_id:string
    -> t_type_constr:core_type
    -> type_declaration

  val generate_packed_t_prime_type_declaration
    :  loc:Location.t
    -> params:(core_type * (variance * injectivity)) list
    -> core_type_params:core_type list
    -> field_type:core_type
    -> type_declaration

  val generate_packed_t_type_declaration
    :  loc:Location.t
    -> core_type_params:core_type list
    -> type_declaration

  val disable_warning_37 : loc:Location.t -> attribute

  (** Generates

      {[
        let <function_name> :
          type <unique_parameter_id>. <core_type_params @ [unique_parameter_id]> t
                -> <arrow_type> = <function_body>
      ]}

      e.g.

      {[
        let name : type a_. ('a, a_) t -> string = fun x -> match x with ...
      ]} *)
  val generate_new_typed_function
    :  loc:Location.t
    -> function_name:string
    -> core_type_params:core_type list
    -> unique_parameter_id:string
    -> var_arrow_type:core_type
    -> constr_arrow_type:core_type
    -> function_body:expression
    -> name_of_first_parameter:Longident.t
    -> structure_item

  val at_least_one_subproduct : ('a * Type_kind.granularity) list -> bool
end
