open Ppx_type_conv.Std

let str_type_decl =
  Type_conv.Generator.make_noarg Ppx_compare_expander.str_type_decl

let sig_type_decl =
  Type_conv.Generator.make_noarg Ppx_compare_expander.sig_type_decl

let () =
  Type_conv.add
    "compare"
    ~str_type_decl
    ~sig_type_decl
    ~extension:(fun ~loc:_ ~path:_ ty -> Ppx_compare_expander.compare_core_type ty)
  |> Type_conv.ignore;
  Type_conv.add
    "compare.equal"
    ~extension:(fun ~loc:_ ~path:_ ty -> Ppx_compare_expander.equal_core_type ty)
  |> Type_conv.ignore;
;;
