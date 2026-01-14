open Stdppx
open Ppxlib
open Ppx_compare_expander

let require_explicit_locality = ref false

let () =
  Driver.add_arg
    "-compare-require-explicit-locality"
    (Set require_explicit_locality)
    ~doc:
      "If this flag is passed, [ppx_compare] will require locality to be stated \
       explicitly. This means either using [[@@deriving compare__global]], [[@@deriving \
       compare__local]], or the equivalent [[@@deriving compare [@mode m]]] with \
       [ppx_template]."
;;

let generator f ~explicit_localize ~name =
  let args () = Deriving.Args.(empty +> flag "unboxed" +> flag "portable") in
  let f ~ctxt (rf, tds) ~localize ~unboxed ~portable =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let tds = Ppx_helpers.with_implicit_unboxed_records ~loc ~unboxed tds in
    f ~ctxt (rf, tds) ~localize ~portable
  in
  match explicit_localize with
  | None ->
    Deriving.Generator.V2.make
      Deriving.Args.(args () +> flag "localize")
      (fun ~ctxt (rf, tds) unboxed portable localize ->
        if !require_explicit_locality && not localize
        then
          Location.raise_errorf
            ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
            "deriving %s: must specify global/local"
            name;
        f ~ctxt (rf, tds) ~localize ~unboxed ~portable)
  | Some localize ->
    Deriving.Generator.V2.make (args ()) (fun ~ctxt (rf, tds) unboxed portable ->
      f ~ctxt (rf, tds) ~localize ~unboxed ~portable)
;;

let deriver name (module M : S) ~explicit_localize =
  Deriving.add
    name
    ~str_type_decl:(generator M.str_type_decl ~explicit_localize ~name)
    ~sig_type_decl:(generator M.sig_type_decl ~explicit_localize ~name)
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

let declare_maybe_raise_if_not_explicit name context pattern f ~explicit =
  Extension.declare name context pattern (fun ~loc ~path a ->
    if !require_explicit_locality && not explicit
    then Location.raise_errorf ~loc "deriving %s: must specify global/local" name;
    f ~loc ~path a)
;;

let () =
  [ ( "compare"
    , "compare__local"
    , "compare__global"
    , Compare.type_
    , Compare.core_type
    , Some Compare.pattern )
  ; ( "equal"
    , "equal__local"
    , "equal__global"
    , Equal.type_
    , Equal.core_type
    , Some Equal.pattern )
  ; ( "@compare.equal"
    , "@compare.equal__local"
    , "@compare.equal__global"
    , Equal.type_
    , Compare.equal_core_type
    , None )
  ]
  |> List.concat_map
       ~f:
         (fun
           (name, explicit_local_name, explicit_global_name, type_, core_type, pattern) ->
         let global_extension_types name ~explicit =
           ( name
           , type_ ~with_local:false
           , core_type ~with_local:false
           , Option.map pattern ~f:(fun pattern -> pattern ~with_local:false)
           , explicit )
         in
         let local_extension_types name ~explicit =
           ( name
           , type_ ~with_local:true
           , core_type ~with_local:true
           , Option.map pattern ~f:(fun pattern -> pattern ~with_local:true)
           , explicit )
         in
         [ global_extension_types name ~explicit:false
         ; global_extension_types explicit_global_name ~explicit:true
         ; local_extension_types explicit_local_name ~explicit:true
         ])
  |> List.iter ~f:(fun (name, type_, core_type, pattern, explicit) ->
    Driver.register_transformation
      (if String.is_prefix name ~prefix:"@" then String.drop_prefix name 1 else name)
      ~rules:
        (Context_free.Rule.extension
           (declare_maybe_raise_if_not_explicit
              name
              Core_type
              Ast_pattern.(ptyp __)
              (fun ~loc ~path:_ ty ->
                type_ ~hide:true ~loc (replace_underscores_by_variables ty))
              ~explicit)
         :: Context_free.Rule.extension
              (declare_maybe_raise_if_not_explicit
                 name
                 Expression
                 Ast_pattern.(ptyp __)
                 (fun ~loc:_ ~path:_ ty -> core_type ty)
                 ~explicit)
         ::
         (match pattern with
          | None -> []
          | Some pattern ->
            [ Context_free.Rule.extension
                (declare_maybe_raise_if_not_explicit
                   name
                   Pattern
                   Ast_pattern.(ptyp __)
                   (fun ~loc:_ ~path:_ ty -> pattern ty)
                   ~explicit)
            ])))
;;
