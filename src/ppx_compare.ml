open Stdppx
open Ppxlib
open Ppx_compare_expander

let generator f ~explicit_localize =
  match explicit_localize with
  | None ->
    Deriving.Generator.V2.make
      Deriving.Args.(empty +> flag "localize" +> flag "portable")
      (fun ~ctxt ast localize portable -> f ~ctxt ast ~localize ~portable)
  | Some localize ->
    Deriving.Generator.V2.make
      Deriving.Args.(empty +> flag "portable")
      (fun ~ctxt ast portable -> f ~ctxt ast ~localize ~portable)
;;

let deriver name (module M : S) ~explicit_localize =
  Deriving.add
    name
    ~str_type_decl:(generator M.str_type_decl ~explicit_localize)
    ~sig_type_decl:(generator M.sig_type_decl ~explicit_localize)
;;

let compare = deriver "compare" (module Compare) ~explicit_localize:None
let equal = deriver "equal" (module Equal) ~explicit_localize:None

let () =
  deriver "compare__local" (module Compare) ~explicit_localize:(Some true)
  |> Deriving.ignore
;;

let () =
  deriver "equal__local" (module Equal) ~explicit_localize:(Some true) |> Deriving.ignore
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
