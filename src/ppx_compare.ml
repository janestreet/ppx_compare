(* Generated code should depend on the environment in scope as little as possible.
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=].  It
   is especially important to not use polymorphic comparisons, since we are moving more
   and more to code that doesn't have them in scope. *)


(* Note: I am introducing a few unnecessary explicit closures, (not all of them some are
   unnecessary due to the value restriction).
*)

open StdLabels
open Ppx_core.Std
open Asttypes
open Parsetree
open Ast_builder.Default
open Ppx_type_conv.Std
open Ppx_compare_expander.Internal

[@@@metaloc loc]

module Pa_tools : sig
  val with_record : Location.t -> value:expression -> lds:label_declaration list ->
    ((expression * core_type) list -> expression) -> expression
end = struct
  let eval_expr_once loc e f =
    match e.pexp_desc with
    | Pexp_ident _ -> f e
    | _ ->
      let n = gen_symbol () in
      pexp_let ~loc Nonrecursive
        [value_binding ~loc ~pat:(pvar ~loc n) ~expr:e]
        (f (evar ~loc n))

  let with_record loc ~value ~lds f =
    (* generate
        let r = value in expr where expr is the result of
        f [ r.fieldname1, fieldtype1; ... ]
     *)
    eval_expr_once loc value (fun record ->
      f (List.map lds ~f:(fun ld ->
        (pexp_field ~loc record (Located.map lident ld.pld_name), ld.pld_type))))
end

let tds_contains_t tds = List.exists tds ~f:(fun td -> td.ptype_name.txt = "t")

module Gen_struct = struct

  let branches_of_sum cds =
    (* process right->left, so gen_symbol call order matches camlp4, thus avoiding
       spurious diffs *)
    let cds = List.rev cds in
    let rightmost_index = 0 in
    List.mapi cds ~f:(fun i cd ->
      let rightmost = i = rightmost_index in
      let loc = cd.pcd_loc in
      if cd.pcd_res <> None then
        Location.raise_errorf ~loc "GADTs are not supported by comparelib";
      match cd.pcd_args with
      | [] ->
        let pcnstr = pconstruct cd None in
        let pany = ppat_any ~loc in
        let case l r n =
          case ~guard:None ~lhs:(ppat_tuple ~loc [l; r]) ~rhs:(eint ~loc n)
        in
        if rightmost then
          [ case pcnstr pcnstr 0 ]
        else
          [ case pcnstr pcnstr 0
          ; case pcnstr pany   (-1)
          ; case pany pcnstr   1
          ]
      | tps ->
        let ids_ty =
          List.map tps
            ~f:(fun ty ->
              (gen_symbol ~prefix:"_a" (),
               gen_symbol ~prefix:"_b" (),
               ty))
        in
        let lpatt = List.map ids_ty ~f:(fun (l,_r,_ty) -> pvar ~loc l) |> ppat_tuple ~loc
        and rpatt = List.map ids_ty ~f:(fun (_l,r,_ty) -> pvar ~loc r) |> ppat_tuple ~loc
        and body =
          List.map ids_ty ~f:(fun (l,r,ty) ->
            compare_of_ty ty (evar ~loc l) (evar ~loc r))
          |> chain_if
        in
        let res =
          case ~guard:None
            ~lhs:(ppat_tuple ~loc [ pconstruct cd (Some lpatt)
                                  ; pconstruct cd (Some rpatt)
                                  ])
            ~rhs:body
        in
        if rightmost then
          [ res ]
        else
          let pany = ppat_any ~loc in
          let pcnstr = pconstruct cd (Some pany) in
          let case l r n =
            case ~guard:None ~lhs:(ppat_tuple ~loc [l; r]) ~rhs:(eint ~loc n)
          in
          [ res
          ; case pcnstr pany   (-1)
          ; case pany   pcnstr 1
          ])
    |> List.map ~f:List.rev
    |> List.concat
    |> List.rev

  let compare_sum loc cds value1 value2 =
    let mcs = branches_of_sum cds in
    let e = pexp_match ~loc (pexp_tuple ~loc [value1; value2]) mcs in
    phys_equal_first value1 value2 e

  let compare_of_record loc lds value1 value2 =
    let expr =
      Pa_tools.with_record loc ~value:value1 ~lds (fun fields1 ->
        Pa_tools.with_record loc ~value:value2 ~lds (fun fields2 ->
          let exprs = List.map2 fields1 fields2 ~f:(fun (v1,t) (v2,_) ->
            compare_of_ty t v1 v2)
          in
          chain_if exprs))
    in
    phys_equal_first value1 value2 expr

  let compare_of_nil loc type_name v_a v_b =
    let str =
      Printf.sprintf
        "Compare called on the type %s, which is abtract in an implementation."
        type_name
    in
    [%expr
      let _ = [%e v_a] in
      let _ = [%e v_b] in
      failwith [%e estring ~loc str]
    ]

  let scheme_of_td td =
    let loc = td.ptype_loc in
    let type_ =
      combinator_type_of_type_declaration td
        ~f:(fun ~loc ty -> [%type: [%t ty] -> [%t ty] -> int])
    in
    match td.ptype_params with
    | [] -> type_
    | l ->
      let vars = List.map l ~f:(fun x -> (get_type_param_name x).txt) in
      ptyp_poly ~loc vars type_

  let compare_of_td td =
    let loc = td.ptype_loc in
    let a = gen_symbol ~prefix:"a" () in
    let b = gen_symbol ~prefix:"b" () in
    let v_a = evar ~loc a in
    let v_b = evar ~loc b in
    let body =
      match td.ptype_kind with
      | Ptype_variant cds -> compare_sum       loc cds v_a v_b
      | Ptype_record  lds -> compare_of_record loc lds v_a v_b
      | Ptype_open ->
        Location.raise_errorf ~loc
          "ppx_compare: open types are not yet supported"
      | Ptype_abstract ->
        match td.ptype_manifest with
        | None -> compare_of_nil loc td.ptype_name.txt v_a v_b
        | Some ty ->
          match ty.ptyp_desc with
          | Ptyp_variant (_, Open, _) | Ptyp_variant (_, Closed, Some (_ :: _)) ->
            Location.raise_errorf ~loc:ty.ptyp_loc
              "ppx_compare: cannot compare open polymorphic variant types"
          | Ptyp_variant (row_fields, _, _) ->
            compare_variant loc row_fields v_a v_b
          | _ ->
            compare_of_ty ty v_a v_b
    in
    let extra_names =
      List.map td.ptype_params
        ~f:(fun p -> tp_name (get_type_param_name p).txt)
    in
    let patts = List.map (extra_names @ [a; b]) ~f:(pvar ~loc)
    and bnd =
      let tn = td.ptype_name.txt in
      pvar ~loc (if tn = "t" then
                   "compare"
                 else
                   "compare_" ^ tn)
    in
    let poly_scheme = (match extra_names with [] -> false | _::_ -> true) in
    if poly_scheme
    then value_binding ~loc ~pat:(ppat_constraint ~loc bnd (scheme_of_td td))
           ~expr:(eabstract ~loc patts body)
    else value_binding ~loc ~pat:bnd
           ~expr:(pexp_constraint ~loc (eabstract ~loc patts body) (scheme_of_td td))

  let compare_of ~loc ~path:_ (rec_flag, tds) =
    let rec_flag = really_recursive rec_flag tds in
    let bindings =
      tds
      |> List.rev
      |> List.map ~f:compare_of_td
      |> List.rev
    in
    let body = pstr_value ~loc rec_flag bindings in
    let st =
      if tds_contains_t tds then
        [ body
        ; [%stri let compare_t = compare ]
        ]
    else
      [ body ]
    in
    Type_conv.Generator_result.make_just_after st

  let gen = Type_conv.Generator.make Type_conv.Args.empty compare_of
end

module Gen_sig = struct
  let sig_of_td td =
    let compare_of =
      combinator_type_of_type_declaration td ~f:(fun ~loc ty ->
        [%type: [%t ty] -> [%t ty] -> int])
    in
    let name =
      match td.ptype_name.txt with
      | "t" -> "compare"
      | tn  -> "compare_" ^ tn
    in
    let loc = td.ptype_loc in
    psig_value ~loc (value_description ~loc ~name:{ td.ptype_name with txt = name }
                       ~type_:compare_of ~prim:[])

  let sig_of_tds ~loc:_ ~path:_ (_rec_flag, tds) =
    List.map tds ~f:sig_of_td
    |> Type_conv.Generator_result.make_at_the_end

  let gen = Type_conv.Generator.make Type_conv.Args.empty sig_of_tds
end

let () =
  Type_conv.add
    "compare"
    ~str_type_decl:Gen_struct.gen
    ~sig_type_decl:Gen_sig.gen
    ~extension:(fun ~loc:_ ~path:_ ty -> Ppx_compare_expander.compare ty)
  |> Type_conv.ignore
;;
