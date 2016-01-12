(* Generated code should depend on the environment in scope as little as possible.
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=].  It
   is especially important to not use polymorphic comparisons, since we are moving more
   and more to code that doesn't have them in scope. *)


(* Note: I am introducing a few unnecessary explicit closures, (not all of them some are
   unnecessary due to the value restriction).
*)

open StdLabels
open Ppx_core.Std
open Parsetree
open Ast_builder.Default

[@@@metaloc loc]

let with_tuple loc ~value ~tys f =
  (* generate
     let id_1, id_2, id_3, ... id_n = value in expr
     where expr is the result of (f [id_1, ty_1 ; id_2, ty_2; ...])
  *)
  let names_types = List.map tys
                      ~f:(fun t -> gen_symbol ~prefix:"t" (), t) in
  let pattern =
    let l = List.map names_types ~f:(fun (n, _) -> pvar ~loc n) in
    ppat_tuple ~loc l
  in
  let e = f (List.map names_types ~f:(fun (n,t) -> (evar ~loc n, t))) in
  let binding  = value_binding ~loc ~pat:pattern ~expr:value in
  pexp_let ~loc Nonrecursive [binding] e

let phys_equal_first a b cmp =
  let loc = cmp.pexp_loc in
  [%expr
    if Pervasives.(==) [%e a] [%e b] then 0 else [%e cmp]
  ]

let rec chain_if  = function
  | [] -> assert false
  | [x] -> x
  | x :: xs ->
    let loc = x.pexp_loc in
    [%expr
      let ret = [%e x] in if Pervasives.(<>) ret 0 then ret else [%e chain_if xs]
    ]

let base_types =
  [ "nativeint"; "int64"; "int32"; "char"; "int"; "bool"; "string"; "float" ]

let compare_named (name : Longident.t Located.t) =
  let loc = name.loc in
  let path, id =
    match name.txt with
    | Ldot (path, id) -> (Some path, id)
    | Lident id       -> (None     , id)
    | Lapply _        -> assert false
  in
  let ldot path id : Longident.t Located.t =
    match path with
    | Some p -> { name with txt = Ldot (p, id) }
    | None   -> { name with txt = Lident id    }
  in
  match path, id with
  | None, v when List.mem ~set:base_types v ->
    let t = ptyp_constr ~loc name [] in
    [%expr (Pervasives.compare : [%t t] -> [%t t] -> int)]
  | None, "unit" -> [%expr  fun _ _ -> 0 ]
  | path, "t" ->
    pexp_ident ~loc (ldot path "compare")
  | path, tn ->
    pexp_ident ~loc (ldot path @@ "compare_" ^ tn)

let tp_name n = Printf.sprintf "_cmp__%s" n

let rec compare_applied ty value1 value2 =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "ref"; _ }, [t]) ->
    let e1 = [%expr [%e value1].Pervasives.contents ] in
    let e2 = [%expr [%e value2].Pervasives.contents ] in
    compare_of_ty t e1 e2
  | Ptyp_constr ({ txt = Lident "option"; _ }, [t]) ->
    let a = gen_symbol () in
    let b = gen_symbol () in
    [%expr
      match ([%e value1], [%e value2]) with
      | (None, None)   -> 0
      | (None, Some _) -> -1
      | (Some _, None) -> 1
      | (Some [%p pvar ~loc a], Some [%p pvar ~loc b]) ->
        [%e compare_of_ty t (evar ~loc a) (evar ~loc b)]
    ]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [t]) ->
    compare_array t value1 value2
  | Ptyp_constr ({ txt = Lident "list"; _ }, [t]) ->
    compare_list t value1 value2
  | Ptyp_constr (name, ta) ->
    let args = List.map ta ~f:(compare_of_ty_fun ~type_constraint:false) in
    let cmp = eapply ~loc (compare_named name) args in
    eapply ~loc cmp [value1; value2]
  | _ -> assert false

and compare_list t value1 value2 =
  let loc = t.ptyp_loc in
  [%expr
    let rec loop a b =
      match (a, b) with
      | ([], []) -> 0
      | ([], _) -> (-1)
      | (_, []) -> 1
      | (x :: xs, y :: ys) ->
        let n = [%e compare_of_ty t [%expr x] [%expr y] ]
        in if Pervasives.(=) n 0 then loop xs ys else n
    in loop [%e value1] [%e value2]
  ]

and compare_array t value1 value2 =
  let loc = t.ptyp_loc in
  [%expr
    if Pervasives.(==) [%e value1] [%e value2] then
      0
    else
      let len_a = Array.length [%e value1] in
      let len_b = Array.length [%e value2] in
      let ret = Pervasives.compare len_a len_b in
      if Pervasives.(<>) ret 0 then ret
      else
        let rec loop i =
          if Pervasives.(=) i len_a then
            0
          else
            let l = Array.unsafe_get [%e value1] i
            and r = Array.unsafe_get [%e value2] i in
            let res = [%e compare_of_ty t [%expr  l ] [%expr  r ]] in
            if Pervasives.(<>) res 0 then res
            else loop (i+1)
        in
        loop 0
  ]

and compare_of_tuple loc tys value1 value2 =
  with_tuple loc ~value:value1 ~tys (fun elems1 ->
    with_tuple loc ~value:value2 ~tys (fun elems2 ->
      let exprs = List.map2 elems1 elems2 ~f:(fun (v1, t) (v2, _) ->
        compare_of_ty t v1 v2)
      in
      chain_if exprs))

and compare_variant loc row_fields value1 value2 =
  let map = function
    | Rtag (cnstr, _attrs, true, _) | Rtag (cnstr, _attrs, _, []) ->
      case ~guard:None
        ~lhs:(ppat_tuple ~loc
                [ppat_variant ~loc cnstr None; ppat_variant ~loc cnstr None])
        ~rhs:(eint ~loc 0)
    | Rtag (cnstr, _attrs, false, tp :: _) ->
      let v1 = gen_symbol ~prefix:"_left" ()
      and v2 = gen_symbol ~prefix:"_right" () in
      let body = compare_of_ty tp (evar ~loc v1) (evar ~loc v2) in
      case ~guard:None
        ~lhs:(ppat_tuple ~loc [ ppat_variant ~loc cnstr (Some (pvar ~loc v1))
                              ; ppat_variant ~loc cnstr (Some (pvar ~loc v2))
                              ])
        ~rhs:body
    | Rinherit ({ ptyp_desc = Ptyp_constr (id, [_]); _ } as ty) ->
      (* quite sadly, this code doesn't handle:
         type 'a id = 'a with compare
         type t = [ `a | [ `b ] id ] with compare
         because it will generate a pattern #id, when id is not even a polymorphic
         variant in the first place.
         The culprit is caml though, since it only allows #id but not #([`b] id) *)
      let v1 = gen_symbol ~prefix:"_left" ()
      and v2 = gen_symbol ~prefix:"_right" () in
      let call = compare_applied ty (evar ~loc v1) (evar ~loc v2) in
      case ~guard:None
        ~lhs:(ppat_tuple ~loc [ ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v1)
                              ; ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v2)
                              ])
        ~rhs:call
    | Rinherit { ptyp_desc = Ptyp_constr (id, []); _ } ->
      let call = compare_named id in
      let v1 = gen_symbol ~prefix:"_left" ()
      and v2 = gen_symbol ~prefix:"_right" () in
      case ~guard:None
        ~lhs:(ppat_tuple ~loc [ ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v1)
                              ; ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v2)
                              ])
        ~rhs:(eapply ~loc call [evar ~loc v1; evar ~loc v2])
    | Rinherit ty ->
      Location.raise_errorf ~loc:ty.ptyp_loc "Ppx_compare.compare_variant: unknown type"
  in
  let e =
    let matched = pexp_tuple ~loc [value1; value2] in
    match
      row_fields
      |> List.rev
      |> List.map ~f:map
      |> List.rev
    with
    | [v] -> pexp_match ~loc matched [v]
    | l     ->
      pexp_match ~loc matched
        (l @
         (* Providing we didn't screw up badly we now know that the tags of the variants
            are different. We let pervasive do its magic. *)
         [ case ~guard:None ~lhs:[%pat? (x, y)] ~rhs:[%expr Pervasives.compare x y] ])
  in
  phys_equal_first value1 value2 e

and branches_of_sum cds =
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

and compare_sum loc cds value1 value2 =
  let mcs = branches_of_sum cds in
  let e = pexp_match ~loc (pexp_tuple ~loc [value1; value2]) mcs in
  phys_equal_first value1 value2 e

and compare_of_ty ty value1 value2 =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_constr (id, []) ->
    eapply ~loc (compare_named id) [value1; value2]
  | Ptyp_constr _ -> compare_applied ty value1 value2
  | Ptyp_tuple tys -> compare_of_tuple loc tys value1 value2
  | Ptyp_var name -> eapply ~loc (evar ~loc @@ tp_name name) [value1; value2]
  | Ptyp_arrow _ ->
    Location.raise_errorf ~loc
      "ppx_compare: Functions can not be compared."
  | Ptyp_variant (row_fields, Closed, None) ->
    compare_variant loc row_fields value1 value2
  | _ ->
    Location.raise_errorf ~loc "ppx_compare: unknown type"

and compare_of_ty_fun ~type_constraint ty =
  let loc = ty.ptyp_loc in
  let a = gen_symbol ~prefix:"a" () in
  let b = gen_symbol ~prefix:"b" () in
  let mk_pat x =
    if type_constraint then
      ppat_constraint ~loc (pvar ~loc x) ty
    else
      pvar ~loc x
  in
  [%expr
    fun [%p mk_pat a] [%p mk_pat b] ->
      [%e compare_of_ty ty (evar ~loc a) (evar ~loc b) ]
  ]

let compare_of_record _loc lds value1 value2 =
  let is_evar = function
    | { pexp_desc = Pexp_ident _; _ } -> true
    | _                               -> false
  in
  assert (is_evar value1);
  assert (is_evar value2);
  List.map lds ~f:(fun ld ->
    let loc = ld.pld_loc in
    let label = Located.map lident ld.pld_name in
    compare_of_ty ld.pld_type
      (pexp_field ~loc value1 label)
      (pexp_field ~loc value2 label))
  |> chain_if
  |> phys_equal_first value1 value2

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

let tds_contains_t tds = List.exists tds ~f:(fun td -> td.ptype_name.txt = "t")

let str_type_decl ~loc ~path:_ (rec_flag, tds) =
  let rec_flag = really_recursive rec_flag tds in
  let bindings =
    tds
    |> List.rev
    |> List.map ~f:compare_of_td
    |> List.rev
  in
  let body = pstr_value ~loc rec_flag bindings in
  if tds_contains_t tds then
    [ body
    ; [%stri let compare_t = compare ]
    ]
  else
    [ body ]

let sig_type_decl ~loc:_ ~path:_ (_rec_flag, tds) =
  List.map tds ~f:(fun td ->
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
                       ~type_:compare_of ~prim:[]))

let compare_core_type ty = compare_of_ty_fun ~type_constraint:true ty

let equal_core_type ty =
  let loc = ty.ptyp_loc in
  let arg1 = gen_symbol () in
  let arg2 = gen_symbol () in
  [%expr
    (fun [%p pvar ~loc arg1] [%p pvar ~loc arg2] ->
       match [%e compare_core_type ty] [%e evar ~loc arg1] [%e evar ~loc arg2] with
       | 0 -> true
       | _ -> false
    )
  ]
