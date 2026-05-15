open Ppxlib_jane

let ptyp_var_any ~loc name =
  Ast_builder.Default.Latest.ptyp_var
    ~loc
    name
    (Some
       { pjka_loc = loc; pjka_desc = Pjk_abbreviation ({ txt = Lident "any"; loc }, []) })
;;
