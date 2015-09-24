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

let compare ty = compare_of_ty_fun ~type_constraint:true ty

module Internal = struct
  let chain_if         = chain_if
  let compare_of_ty    = compare_of_ty
  let phys_equal_first = phys_equal_first
  let compare_variant  = compare_variant
  let tp_name          = tp_name
end

