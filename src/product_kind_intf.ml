open Base
open Ppxlib

module type S = sig
  type t

  (* Generates the name with which an individual element is
     identified. This name is used to generate the constructor for each element. *)
  val name : int -> t -> label

  (* Retrieves the type of an element. This is the type that defines the type of
     each constructor in the t GADT.*)
  val to_type : t -> core_type

  (*
     Generates the expression which sets an element in the original
     tuple/record.

     The element to be changed is identified by the index which is position
     based in the order that the fields/elements where defined.
  *)
  val set_rhs_expression
    :  loc:location
    -> index:int
    -> element:t
    -> number_of_elements:int
    -> expression_to_set:expression
    -> expression

  (*
     Generates the expression which gets an element from the original
     tuple/record.

     The element to be retrieved is identified by the index which is position
     based in the order that the fields/elements where defined.
  *)
  val get_rhs_expression
    :  loc:location
    -> index:int
    -> element:t
    -> number_of_elements:int
    -> expression

  (*
     Generates an expression that creates a tuple/record from a creator function.
  *)
  val create_expression
    :  loc:location
    -> constructor_declarations:
         ((t * Type_kind_intf.granularity) * constructor_declaration) list
    -> local:bool
    -> expression
end
