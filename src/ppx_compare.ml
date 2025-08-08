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
  deriver "compare__global" (module Compare) ~explicit_localize:(Some false)
  |> Deriving.ignore
;;

let () =
  deriver "equal__global" (module Equal) ~explicit_localize:(Some false)
  |> Deriving.ignore
;;

let compare_local =
  deriver "compare__local" (module Compare) ~explicit_localize:(Some true)
;;

let equal_local = deriver "equal__local" (module Equal) ~explicit_localize:(Some true)

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
  [ ( "compare"
    , "compare_local"
    , "compare__local"
    , Compare.type_
    , Compare.core_type
    , Some Compare.pattern )
  ; ( "equal"
    , "equal_local"
    , "equal__local"
    , Equal.type_
    , Equal.core_type
    , Some Equal.pattern )
  ; ( "@compare.equal"
    , "@compare_local.equal"
    , "@compare.equal__local"
    , Equal.type_
    , Compare.equal_core_type
    , None )
  ]
  |> List.concat_map
       ~f:
         (fun
           ( name
           , local_name
           , local_name_with_two_underscores_for_ppx_template
           , type_
           , core_type
           , pattern )
         ->
         let local_extension_types name =
           ( name
           , type_ ~with_local:true
           , core_type ~with_local:true
           , Option.map pattern ~f:(fun pattern -> pattern ~with_local:true) )
         in
         [ ( name
           , type_ ~with_local:false
           , core_type ~with_local:false
           , Option.map pattern ~f:(fun pattern -> pattern ~with_local:false) )
         ; local_extension_types local_name
         ; local_extension_types local_name_with_two_underscores_for_ppx_template
         ])
  |> List.iter ~f:(fun (name, type_, core_type, pattern) ->
    Driver.register_transformation
      (if String.is_prefix name ~prefix:"@" then String.drop_prefix name 1 else name)
      ~rules:
        (Context_free.Rule.extension
           (Extension.declare
              name
              Core_type
              Ast_pattern.(ptyp __)
              (fun ~loc ~path:_ ty ->
                type_ ~hide:true ~loc (replace_underscores_by_variables ty)))
         :: Context_free.Rule.extension
              (Extension.declare
                 name
                 Expression
                 Ast_pattern.(ptyp __)
                 (fun ~loc:_ ~path:_ ty -> core_type ty))
         ::
         (match pattern with
          | None -> []
          | Some pattern ->
            [ Context_free.Rule.extension
                (Extension.declare
                   name
                   Pattern
                   Ast_pattern.(ptyp __)
                   (fun ~loc:_ ~path:_ ty ->
                     match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ty.ptyp_desc with
                     | Ptyp_constr (id, _) -> pattern id
                     | Ptyp_var _ ->
                       Ast_builder.Default.ppat_extension
                         ~loc:ty.ptyp_loc
                         (Location.error_extensionf
                            ~loc:ty.ptyp_loc
                            "Type variables are disallowed here to emphasize that type \
                             constraints don't work as expected. Instead, consider using \
                             a locally abstract type.")
                     | _ ->
                       Ast_builder.Default.ppat_extension
                         ~loc:ty.ptyp_loc
                         (Location.error_extensionf
                            ~loc:ty.ptyp_loc
                            "Only type constructors are allowed here (e.g. [t], ['a t], \
                             or [M(X).t]).")))
            ])))
;;
