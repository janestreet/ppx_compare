open Stdppx
open Ppxlib
open Ppx_compare_expander

let add_deriver ~always_localize name (module E : Ppx_compare_expander.S) =
  let str_type_decl, sig_type_decl =
    match always_localize with
    | false ->
      let localize_and_portable_arg () =
        Deriving.Args.(empty +> flag "localize" +> flag "portable")
      in
      ( Deriving.Generator.V2.make
          (localize_and_portable_arg ())
          (fun ~ctxt tds localize portable ->
            E.str_type_decl ~ctxt tds ~localize ~portable)
          ~attributes:E.str_attributes
      , Deriving.Generator.V2.make
          (localize_and_portable_arg ())
          (fun ~ctxt tds localize portable ->
             E.sig_type_decl ~ctxt tds ~localize ~portable) )
    | true ->
      let portable_arg () = Deriving.Args.(empty +> flag "portable") in
      ( Deriving.Generator.V2.make
          (portable_arg ())
          (fun ~ctxt tds portable ->
            E.str_type_decl ~ctxt tds ~localize:always_localize ~portable)
          ~attributes:E.str_attributes
      , Deriving.Generator.V2.make (portable_arg ()) (fun ~ctxt tds portable ->
          E.sig_type_decl ~ctxt tds ~localize:always_localize ~portable) )
  in
  Deriving.add name ~str_type_decl ~sig_type_decl
;;

let compare = add_deriver ~always_localize:false "compare" (module Compare)
let equal = add_deriver ~always_localize:false "equal" (module Equal)

let () =
  add_deriver ~always_localize:true "compare__local" (module Compare) |> Deriving.ignore
;;

let () =
  add_deriver ~always_localize:true "equal__local" (module Equal) |> Deriving.ignore
;;

let replace_underscores_by_variables =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type_desc t =
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree t with
        | Ptyp_any jkind ->
          Ppxlib_jane.Shim.Core_type_desc.to_parsetree
            (Ptyp_var (gen_symbol ~prefix:"a" (), jkind))
        | _ -> super#core_type_desc t
    end
  in
  map#core_type
;;

let () =
  [ "compare", "compare_local", Compare.type_, Compare.core_type
  ; "equal", "equal_local", Equal.type_, Equal.core_type
  ; "@compare.equal", "@compare_local.equal", Equal.type_, Compare.equal_core_type
  ]
  |> List.concat_map ~f:(fun (name, local_name, type_, core_type) ->
    [ name, type_ ~with_local:false, core_type ~with_local:false
    ; local_name, type_ ~with_local:true, core_type ~with_local:true
    ])
  |> List.iter ~f:(fun (name, type_, core_type) ->
    Driver.register_transformation
      (if String.is_prefix name ~prefix:"@" then String.drop_prefix name 1 else name)
      ~rules:
        [ Context_free.Rule.extension
            (Extension.declare
               name
               Core_type
               Ast_pattern.(ptyp __)
               (fun ~loc ~path:_ ty ->
                 type_ ~hide:true ~loc (replace_underscores_by_variables ty)))
        ; Context_free.Rule.extension
            (Extension.declare
               name
               Expression
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty -> core_type ty))
        ])
;;
