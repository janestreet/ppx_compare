open Ppx_type_conv.Std
open Ppx_core.Std

let str_type_decl =
  Type_conv.Generator.make_noarg Ppx_compare_expander.str_type_decl

let sig_type_decl =
  Type_conv.Generator.make_noarg Ppx_compare_expander.sig_type_decl

let replace_underscores_by_variables =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type_desc = function
      | Ptyp_any -> Ptyp_var (gen_symbol ~prefix:"a" ())
      | t -> super#core_type_desc t
  end in
  map#core_type

let () =
  let name = "compare" in
  Type_conv.add name
    ~str_type_decl
    ~sig_type_decl
    ~extension:(fun ~loc:_ ~path:_ ty -> Ppx_compare_expander.compare_core_type ty)
  |> Type_conv.ignore;
  Ppx_driver.register_transformation name
    ~rules:[ Context_free.Rule.extension
               (Extension.declare name
                  Core_type Ast_pattern.(ptyp __)
                  (fun ~loc ~path:_ ty ->
                     Ppx_compare_expander.compare_type ~loc
                       (replace_underscores_by_variables ty))) ]
;;

let () =
  let name = "compare.equal" in
  Type_conv.add name
    ~str_type_decl
    ~sig_type_decl
    ~extension:(fun ~loc:_ ~path:_ ty -> Ppx_compare_expander.equal_core_type ty)
  |> Type_conv.ignore;
  Ppx_driver.register_transformation name
    ~rules:[ Context_free.Rule.extension
               (Extension.declare name
                  Core_type Ast_pattern.(ptyp __)
                  (fun ~loc ~path:_ ty ->
                     Ppx_compare_expander.equal_type ~loc
                       (replace_underscores_by_variables ty))) ]
;;
