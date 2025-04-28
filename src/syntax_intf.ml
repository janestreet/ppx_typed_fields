open! Base

module Definitions = struct
  module type S = sig
    include Ppxlib.Ast_builder.S
    include Ppxlib_jane.Ast_builder.S_with_implicit_loc
  end
end

module type Syntax = sig
  include module type of struct
    include Definitions
  end

  val builder : Ppxlib.Location.t -> (module S)
end
