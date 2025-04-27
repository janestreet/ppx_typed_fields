open! Base
include Syntax_intf.Definitions

let builder loc : (module S) =
  (module struct
    include (val Ppxlib.Ast_builder.make loc)
    include (val Ppxlib_jane.Ast_builder.make loc)
  end)
;;
