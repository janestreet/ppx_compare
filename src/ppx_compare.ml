open Base
open Ppxlib
open Ppx_compare_expander

let add_deriver name (module E : Ppx_compare_expander.S) =
  let flags () = Deriving.Args.(empty +> flag "localize") in
  let str_type_decl =
    Deriving.Generator.V2.make (flags ()) E.str_type_decl ~attributes:E.str_attributes
  in
  let sig_type_decl = Deriving.Generator.V2.make (flags ()) E.sig_type_decl in
  Deriving.add name ~str_type_decl ~sig_type_decl
;;

let compare = add_deriver "compare" (module Compare)
let equal = add_deriver "equal" (module Equal)

let replace_underscores_by_variables =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type_desc =
        function
        | Ptyp_any -> Ptyp_var (gen_symbol ~prefix:"a" ())
        | t -> super#core_type_desc t
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
         (String.strip name ~drop:(Char.equal '@'))
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
