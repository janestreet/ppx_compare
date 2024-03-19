(* Generated code should depend on the environment in scope as little as possible.
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=].  It
   is especially important to not use polymorphic comparisons, since we are moving more
   and more to code that doesn't have them in scope. *)

(* Note: I am introducing a few unnecessary explicit closures, (not all of them some are
   unnecessary due to the value restriction).
*)

open Base
open Ppxlib
open Ast_builder.Default
include Ppx_compare_expander_intf

(* Two-argument function, possibly with [local_] arguments *)
let ptyp_arrow2 ~loc ~local_args arg1 arg2 res =
  if local_args
  then [%type: [%t arg1] -> [%t arg2] -> [%t res]]
  else [%type: [%t arg1] -> [%t arg2] -> [%t res]]
;;

type kind =
  | Compare
  | Equal

module type Params = sig
  val name : string
  val kind : kind
  val chain : expression -> expression -> expression
  val const : loc:Location.t -> Ordering.t -> expression
  val result_type : loc:Location.t -> core_type
  val poly : loc:Location.t -> expression -> expression -> expression

  val abstract
    :  loc:Location.t
    -> type_name:string
    -> expression
    -> expression
    -> expression

  module Attrs : Attrs
end

module Make_attrs (Name : sig
  val name : string
end) : Attrs = struct
  let ignore_label_declaration =
    Attribute.declare
      (Name.name ^ ".ignore")
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()
  ;;

  let ignore_core_type =
    Attribute.declare
      (Name.name ^ ".ignore")
      Attribute.Context.core_type
      Ast_pattern.(pstr nil)
      ()
  ;;
end

module Compare_params : Params = struct
  let name = "compare"
  let kind = Compare

  let chain a b =
    let loc = a.pexp_loc in
    [%expr
      match [%e a] with
      | 0 -> [%e b]
      | n -> n]
  ;;

  let const ~loc (ord : Ordering.t) =
    eint
      ~loc
      (match ord with
       | Less -> -1
       | Equal -> 0
       | Greater -> 1)
  ;;

  let result_type ~loc = [%type: int]
  let poly ~loc a b = [%expr Stdlib.compare [%e a] [%e b]]

  let abstract ~loc ~type_name a b =
    [%expr
      Ppx_compare_lib.compare_abstract
        ~type_name:[%e estring ~loc type_name]
        [%e a]
        [%e b]]
  ;;

  module Attrs = Make_attrs (struct
    let name = name
  end)
end

module Equal_params : Params = struct
  let name = "equal"
  let kind = Equal

  let chain a b =
    let loc = a.pexp_loc in
    [%expr Stdlib.( && ) [%e a] [%e b]]
  ;;

  let const ~loc (ord : Ordering.t) =
    match ord with
    | Equal -> [%expr true]
    | Less | Greater -> [%expr false]
  ;;

  let result_type ~loc = [%type: bool]
  let poly ~loc a b = [%expr Stdlib.( = ) [%e a] [%e b]]

  let abstract ~loc ~type_name a b =
    [%expr
      Ppx_compare_lib.equal_abstract ~type_name:[%e estring ~loc type_name] [%e a] [%e b]]
  ;;

  module Attrs = Make_attrs (struct
    let name = name
  end)
end

module Make (Params : Params) = struct
  open Params
  module Attrs = Attrs

  let str_attributes =
    [ Attribute.T Attrs.ignore_label_declaration; Attribute.T Attrs.ignore_core_type ]
  ;;

  let is_ignored_gen ~loc ~compare_attr ~equal_attr ast =
    match kind, Attribute.get compare_attr ast, Attribute.get equal_attr ast with
    | _, Some (), Some () | Compare, Some (), None | Equal, None, Some () -> true
    | _, None, None -> false
    | Compare, None, Some () ->
      Location.raise_errorf
        ~loc
        "Cannot use [@@equal.ignore] with [@@@@deriving compare]."
    | Equal, Some (), None ->
      Location.raise_errorf ~loc "Cannot use [@@compare.ignore] with [@@@@deriving equal]"
  ;;

  let core_type_is_ignored ty =
    is_ignored_gen
      ~loc:ty.ptyp_loc
      ~compare_attr:Compare_params.Attrs.ignore_core_type
      ~equal_attr:Equal_params.Attrs.ignore_core_type
      ty
  ;;

  let label_is_ignored ld =
    is_ignored_gen
      ~loc:ld.pld_loc
      ~compare_attr:Compare_params.Attrs.ignore_label_declaration
      ~equal_attr:Equal_params.Attrs.ignore_label_declaration
      ld
  ;;

  let with_tuple loc ~value ~ltys f =
    (* generate
       let id_1, id_2, id_3, ... id_n = value in expr
       where expr is the result of (f [id_1, ty_1 ; id_2, ty_2; ...])
    *)
    let names_types =
      List.map ltys ~f:(fun (lbl, t) -> gen_symbol ~prefix:"t" (), lbl, t)
    in
    let pattern =
      let l = List.map names_types ~f:(fun (n, lbl, _) -> lbl, pvar ~loc n) in
      Ppxlib_jane.Jane_syntax.Pattern.pat_of ~loc ~attrs:[] (Jpat_tuple (l, Closed))
    in
    let e = f (List.map names_types ~f:(fun (n, _, t) -> evar ~loc n, t)) in
    let binding = value_binding ~loc ~pat:pattern ~expr:value in
    pexp_let ~loc Nonrecursive [ binding ] e
  ;;

  let phys_equal_first a b cmp =
    let loc = cmp.pexp_loc in
    [%expr if Stdlib.( == ) [%e a] [%e b] then [%e const ~loc Equal] else [%e cmp]]
  ;;

  let rec chain_if ~loc = function
    | [] -> const ~loc Equal
    | [ x ] -> x
    | x :: xs -> chain x (chain_if ~loc:x.pexp_loc xs)
  ;;

  let tp_name n = Printf.sprintf "_cmp__%s" n

  let type_ ~with_local ~hide ~loc ty =
    let loc = { loc with loc_ghost = true } in
    let ptyp_attributes =
      if hide
      then Merlin_helpers.hide_attribute :: ty.ptyp_attributes
      else ty.ptyp_attributes
    in
    let hty = { ty with ptyp_attributes } in
    ptyp_arrow2 ~loc ~local_args:with_local ty hty (result_type ~loc)
  ;;

  let function_name ~with_local typename =
    let name =
      match typename with
      | "t" -> name
      | s -> name ^ "_" ^ s
    in
    if with_local then name ^ "__local" else name
  ;;

  let compare_ignore ~loc value1 value2 =
    [%expr
      let (_ : _) = [%e value1]
      and (_ : _) = [%e value2] in
      [%e const ~loc Equal]]
  ;;

  let rec compare_applied ~hide ~with_local ~constructor ~args value1 value2 =
    let args =
      List.map args ~f:(compare_of_ty_fun ~hide ~with_local ~type_constraint:false)
      @ [ value1; value2 ]
    in
    type_constr_conv
      ~loc:(Located.loc constructor)
      constructor
      args
      ~f:(function_name ~with_local)

  and compare_of_tuple ~hide ~with_local loc ltys value1 value2 =
    with_tuple loc ~value:value1 ~ltys (fun elems1 ->
      with_tuple loc ~value:value2 ~ltys (fun elems2 ->
        let exprs =
          List.map2_exn elems1 elems2 ~f:(fun (v1, t) (v2, _) ->
            compare_of_ty ~hide ~with_local t v1 v2)
        in
        chain_if ~loc exprs))

  and compare_variant ~hide ~with_local loc row_fields value1 value2 =
    let map row =
      match row.prf_desc with
      | Rtag ({ txt = cnstr; _ }, true, _) | Rtag ({ txt = cnstr; _ }, _, []) ->
        case
          ~guard:None
          ~lhs:
            (ppat_tuple
               ~loc
               [ ppat_variant ~loc cnstr None; ppat_variant ~loc cnstr None ])
          ~rhs:(const ~loc Equal)
      | Rtag ({ txt = cnstr; _ }, false, tp :: _) ->
        let v1 = gen_symbol ~prefix:"_left" ()
        and v2 = gen_symbol ~prefix:"_right" () in
        let body = compare_of_ty ~hide ~with_local tp (evar ~loc v1) (evar ~loc v2) in
        case
          ~guard:None
          ~lhs:
            (ppat_tuple
               ~loc
               [ ppat_variant ~loc cnstr (Some (pvar ~loc v1))
               ; ppat_variant ~loc cnstr (Some (pvar ~loc v2))
               ])
          ~rhs:body
      | Rinherit { ptyp_desc = Ptyp_constr (id, args); _ } ->
        (* quite sadly, this code doesn't handle:
           type 'a id = 'a with compare
           type t = [ `a | [ `b ] id ] with compare
           because it will generate a pattern #id, when id is not even a polymorphic
           variant in the first place.
           The culprit is caml though, since it only allows #id but not #([`b] id)
        *)
        let v1 = gen_symbol ~prefix:"_left" ()
        and v2 = gen_symbol ~prefix:"_right" () in
        case
          ~guard:None
          ~lhs:
            (ppat_tuple
               ~loc
               [ ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v1)
               ; ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v2)
               ])
          ~rhs:
            (compare_applied
               ~hide
               ~with_local
               ~constructor:id
               ~args
               (evar ~loc v1)
               (evar ~loc v2))
      | Rinherit ty ->
        Location.raise_errorf ~loc:ty.ptyp_loc "Ppx_compare.compare_variant: unknown type"
    in
    let e =
      let matched = pexp_tuple ~loc [ value1; value2 ] in
      match List.map ~f:map row_fields with
      | [ v ] -> pexp_match ~loc matched [ v ]
      | l ->
        pexp_match
          ~loc
          matched
          (l
           @ (* Providing we didn't screw up badly we now know that the tags of the variants
                are different. We let pervasive do its magic. *)
           [ case ~guard:None ~lhs:[%pat? x, y] ~rhs:(poly ~loc [%expr x] [%expr y]) ])
    in
    phys_equal_first value1 value2 e

  and branches_of_sum ~hide ~with_local cds =
    let rightmost_index = List.length cds - 1 in
    List.concat
      (List.mapi cds ~f:(fun i cd ->
         let rightmost = i = rightmost_index in
         let loc = cd.pcd_loc in
         if Option.is_some cd.pcd_res
         then
           (* If we get GADTs support, fix the constant sum type optimization for them *)
           Location.raise_errorf ~loc "GADTs are not supported by comparelib";
         match cd.pcd_args with
         | Pcstr_record lds ->
           let value1 = gen_symbol ~prefix:"_a" () in
           let value2 = gen_symbol ~prefix:"_b" () in
           let res =
             case
               ~guard:None
               ~lhs:
                 (ppat_tuple
                    ~loc
                    [ pconstruct cd (Some (pvar ~loc value1))
                    ; pconstruct cd (Some (pvar ~loc value2))
                    ])
               ~rhs:
                 (compare_of_record_no_phys_equal
                    ~hide
                    ~with_local
                    loc
                    lds
                    (evar ~loc value1)
                    (evar ~loc value2))
           in
           if rightmost
           then [ res ]
           else (
             let pany = ppat_any ~loc in
             let pcnstr = pconstruct cd (Some pany) in
             let case l r ord =
               case ~guard:None ~lhs:(ppat_tuple ~loc [ l; r ]) ~rhs:(const ~loc ord)
             in
             [ res; case pcnstr pany Less; case pany pcnstr Greater ])
         | Pcstr_tuple pcd_args ->
           (match pcd_args with
            | [] ->
              let pcnstr = pconstruct cd None in
              let pany = ppat_any ~loc in
              let case l r ord =
                case ~guard:None ~lhs:(ppat_tuple ~loc [ l; r ]) ~rhs:(const ~loc ord)
              in
              if rightmost
              then [ case pcnstr pcnstr Equal ]
              else
                [ case pcnstr pcnstr Equal
                ; case pcnstr pany Less
                ; case pany pcnstr Greater
                ]
            | tps ->
              let ids_ty =
                List.map tps ~f:(fun ty ->
                  let a = gen_symbol ~prefix:"_a" () in
                  let b = gen_symbol ~prefix:"_b" () in
                  a, b, ty)
              in
              let lpatt =
                List.map ids_ty ~f:(fun (l, _r, _ty) -> pvar ~loc l) |> ppat_tuple ~loc
              and rpatt =
                List.map ids_ty ~f:(fun (_l, r, _ty) -> pvar ~loc r) |> ppat_tuple ~loc
              and body =
                List.map ids_ty ~f:(fun (l, r, ty) ->
                  compare_of_ty ~hide ~with_local ty (evar ~loc l) (evar ~loc r))
                |> chain_if ~loc
              in
              let res =
                case
                  ~guard:None
                  ~lhs:
                    (ppat_tuple
                       ~loc
                       [ pconstruct cd (Some lpatt); pconstruct cd (Some rpatt) ])
                  ~rhs:body
              in
              if rightmost
              then [ res ]
              else (
                let pany = ppat_any ~loc in
                let pcnstr = pconstruct cd (Some pany) in
                let case l r ord =
                  case ~guard:None ~lhs:(ppat_tuple ~loc [ l; r ]) ~rhs:(const ~loc ord)
                in
                [ res; case pcnstr pany Less; case pany pcnstr Greater ]))))

  and compare_sum ~hide ~with_local loc cds value1 value2 =
    let is_sum_type_with_all_constant_constructors =
      List.for_all cds ~f:(fun cd ->
        Option.is_none cd.pcd_res
        &&
        (* we could support GADTs, but the general case
           doesn't, so let's hold off *)
        match cd.pcd_args with
        | Pcstr_tuple l -> List.is_empty l
        | Pcstr_record l -> List.is_empty l)
    in
    if is_sum_type_with_all_constant_constructors
    then
      (* the compiler will optimize the polymorphic comparison to an integer one *)
      poly ~loc value1 value2
    else (
      let mcs = branches_of_sum ~hide ~with_local cds in
      let e = pexp_match ~loc (pexp_tuple ~loc [ value1; value2 ]) mcs in
      phys_equal_first value1 value2 e)

  and compare_of_ty ~hide ~with_local ty value1 value2 =
    let loc = ty.ptyp_loc in
    if core_type_is_ignored ty
    then compare_ignore ~loc value1 value2
    else (
      match Ppxlib_jane.Jane_syntax.Core_type.of_ast ty with
      | Some (Jtyp_tuple ltps, _attrs) ->
        compare_of_tuple ~hide ~with_local loc ltps value1 value2
      | Some (Jtyp_layout _, _) ->
        Location.raise_errorf
          ~loc
          "Layout annotations are not currently supported with [ppx_compare]."
      | None ->
        (match ty.ptyp_desc with
         | Ptyp_constr (constructor, args) ->
           compare_applied ~hide ~with_local ~constructor ~args value1 value2
         | Ptyp_tuple tys ->
           compare_of_tuple
             ~hide
             ~with_local
             loc
             (List.map ~f:(fun ty -> None, ty) tys)
             value1
             value2
         | Ptyp_var name -> eapply ~loc (evar ~loc (tp_name name)) [ value1; value2 ]
         | Ptyp_arrow _ ->
           Location.raise_errorf ~loc "ppx_compare: Functions can not be compared."
         | Ptyp_variant (row_fields, Closed, None) ->
           compare_variant ~hide ~with_local loc row_fields value1 value2
         | Ptyp_any -> compare_ignore ~loc value1 value2
         | _ -> Location.raise_errorf ~loc "ppx_compare: unknown type"))

  and compare_of_ty_fun ~hide ~with_local ~type_constraint ty =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    let do_hide hide_fun x = if hide then hide_fun x else x in
    let a = gen_symbol ~prefix:"a" () in
    let b = gen_symbol ~prefix:"b" () in
    let e_a = evar ~loc a in
    let e_b = evar ~loc b in
    let mk_pat x =
      if type_constraint then ppat_constraint ~loc (pvar ~loc x) ty else pvar ~loc x
    in
    let body =
      do_hide Merlin_helpers.hide_expression (compare_of_ty ~hide ~with_local ty e_a e_b)
    in
    eta_reduce_if_possible
      [%expr
        fun [%p mk_pat a] [%p do_hide Merlin_helpers.hide_pattern (mk_pat b)] -> [%e body]]

  and compare_of_record_no_phys_equal ~hide ~with_local loc lds value1 value2 =
    let is_evar = function
      | { pexp_desc = Pexp_ident _; _ } -> true
      | _ -> false
    in
    assert (is_evar value1);
    assert (is_evar value2);
    List.filter lds ~f:(fun ld -> not (label_is_ignored ld))
    |> List.map ~f:(fun ld ->
         let loc = ld.pld_loc in
         let label = Located.map lident ld.pld_name in
         compare_of_ty
           ~hide
           ~with_local
           ld.pld_type
           (pexp_field ~loc value1 label)
           (pexp_field ~loc value2 label))
    |> chain_if ~loc
  ;;

  let compare_of_record ~hide ~with_local loc lds value1 value2 =
    compare_of_record_no_phys_equal ~hide ~with_local loc lds value1 value2
    |> phys_equal_first value1 value2
  ;;

  let compare_abstract loc type_name v_a v_b = abstract ~loc ~type_name v_a v_b

  let scheme_of_td ~hide ~with_local td =
    let loc = td.ptype_loc in
    let type_ = combinator_type_of_type_declaration td ~f:(type_ ~hide ~with_local) in
    match td.ptype_params with
    | [] -> type_
    | l ->
      let vars = List.map l ~f:get_type_param_name in
      ptyp_poly ~loc vars type_
  ;;

  let compare_of_td ~hide ~with_local td ~rec_flag =
    let loc = td.ptype_loc in
    let a = gen_symbol ~prefix:"a" () in
    let b = gen_symbol ~prefix:"b" () in
    let v_a = evar ~loc a in
    let v_b = evar ~loc b in
    let function_body =
      match td.ptype_kind with
      | Ptype_variant cds -> compare_sum ~hide ~with_local loc cds v_a v_b
      | Ptype_record lds -> compare_of_record ~hide ~with_local loc lds v_a v_b
      | Ptype_open ->
        Location.raise_errorf ~loc "ppx_compare: open types are not yet supported"
      | Ptype_abstract ->
        (match td.ptype_manifest with
         | None -> compare_abstract loc td.ptype_name.txt v_a v_b
         | Some ty ->
           (match ty.ptyp_desc with
            | Ptyp_variant (_, Open, _) | Ptyp_variant (_, Closed, Some (_ :: _)) ->
              Location.raise_errorf
                ~loc:ty.ptyp_loc
                "ppx_compare: cannot compare open polymorphic variant types"
            | Ptyp_variant (row_fields, _, _) ->
              compare_variant ~hide ~with_local loc row_fields v_a v_b
            | _ -> compare_of_ty ~hide ~with_local ty v_a v_b))
    in
    let extra_names =
      List.map td.ptype_params ~f:(fun p -> tp_name (get_type_param_name p).txt)
    in
    let patts = List.map (extra_names @ [ a; b ]) ~f:(pvar ~loc)
    and bnd = pvar ~loc (function_name ~with_local td.ptype_name.txt) in
    let poly_scheme =
      match extra_names with
      | [] -> false
      | _ :: _ -> true
    in
    let body =
      eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc patts function_body)
    in
    if poly_scheme
    then
      value_binding
        ~loc
        ~pat:(ppat_constraint ~loc bnd (scheme_of_td ~hide ~with_local td))
        ~expr:body
    else
      value_binding
        ~loc
        ~pat:bnd
        ~expr:(pexp_constraint ~loc body (scheme_of_td ~hide ~with_local td))
  ;;

  let bindings_of_tds tds ~hide ~with_local ~rec_flag =
    List.map tds ~f:(fun td -> compare_of_td td ~hide ~with_local ~rec_flag)
  ;;

  let eta_expand2 ~loc f =
    eabstract
      ~loc
      [ pvar ~loc "a"; pvar ~loc "b" ]
      (eapply ~loc f [ evar ~loc "a"; evar ~loc "b" ])
  ;;

  let aliases_of_tds tds ~hide =
    (* So that ~localize doesn't double the size of the generated code, we define the non
       local_ function as an alias to the local_ function. This only works for ground
       types, as [('a -> 'a -> int) -> 'a list -> 'a list -> int] is a type that is
       neither stronger nor weaker than the same type with local_ on the 'a and 'a
       list. If the compiler supports polymorphism over locality one day, we may be able
       to only generate one version of the code, the local version. *)
    if List.for_all tds ~f:(fun td -> List.is_empty td.ptype_params)
    then
      Some
        (List.map tds ~f:(fun td ->
           let loc = td.ptype_name.loc in
           value_binding
             ~loc
             ~pat:(pvar ~loc (function_name ~with_local:false td.ptype_name.txt))
             ~expr:
               (pexp_constraint
                  ~loc
                  (eta_expand2
                     ~loc
                     (evar ~loc (function_name ~with_local:true td.ptype_name.txt)))
                  (scheme_of_td ~hide ~with_local:false td))))
    else None
  ;;

  let str_type_decl ~ctxt (rec_flag, tds) localize =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let hide = not (Expansion_context.Deriver.inline ctxt) in
    let tds = List.map tds ~f:name_type_params_in_td in
    let rec_flag =
      (object
         inherit type_is_recursive rec_flag tds as super

         method! label_declaration ld =
           if not (label_is_ignored ld) then super#label_declaration ld

         method! core_type ty = if not (core_type_is_ignored ty) then super#core_type ty
      end)
        #go
        ()
    in
    if localize
    then
      [ pstr_value ~loc rec_flag (bindings_of_tds tds ~hide ~with_local:true ~rec_flag)
      ; (match aliases_of_tds tds ~hide with
         | Some values -> pstr_value ~loc Nonrecursive values
         | None ->
           pstr_value
             ~loc
             rec_flag
             (bindings_of_tds tds ~hide ~with_local:false ~rec_flag))
      ]
    else
      [ pstr_value ~loc rec_flag (bindings_of_tds tds ~hide ~with_local:false ~rec_flag) ]
  ;;

  let mk_sig ~ctxt ~localize (_rec_flag, tds) =
    let hide = not (Expansion_context.Deriver.inline ctxt) in
    let tds = List.map tds ~f:name_type_params_in_td in
    List.concat_map tds ~f:(fun td ->
      let generate ~with_local =
        let compare_of =
          combinator_type_of_type_declaration td ~f:(type_ ~hide ~with_local)
        in
        let name = function_name ~with_local td.ptype_name.txt in
        let loc = td.ptype_loc in
        psig_value
          ~loc
          (value_description
             ~loc
             ~name:{ td.ptype_name with txt = name }
             ~type_:compare_of
             ~prim:[])
      in
      if localize
      then [ generate ~with_local:false; generate ~with_local:true ]
      else [ generate ~with_local:false ])
  ;;

  let sig_type_decl ~ctxt (rec_flag, tds) localize =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let module_name =
      match kind with
      | Compare -> "Comparable"
      | Equal -> "Equal"
    in
    let mk_named_sig ~with_local =
      let module_type_name = if with_local then "S_local" else "S" in
      let sg_name = Printf.sprintf "Ppx_compare_lib.%s.%s" module_name module_type_name in
      mk_named_sig ~loc ~sg_name ~handle_polymorphic_variant:false tds
    in
    match mk_named_sig ~with_local:false, mk_named_sig ~with_local:true with
    | Some include_infos, _ when not localize -> [ psig_include ~loc include_infos ]
    | Some include_infos, Some include_infos_local when localize ->
      [ psig_include ~loc include_infos; psig_include ~loc include_infos_local ]
    | _ -> mk_sig ~ctxt ~localize (rec_flag, tds)
  ;;

  let compare_core_type ~with_local ty =
    compare_of_ty_fun ~hide:true ~with_local ~type_constraint:true ty
  ;;

  let core_type ~with_local = compare_core_type ~with_local
end

module Compare = struct
  include Make (Compare_params)

  let equal_core_type ~with_local ty =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    let arg1 = gen_symbol () in
    let arg2 = gen_symbol () in
    let body =
      Merlin_helpers.hide_expression
        [%expr
          match
            [%e compare_core_type ~with_local ty] [%e evar ~loc arg1] [%e evar ~loc arg2]
          with
          | 0 -> true
          | _ -> false]
    in
    [%expr fun ([%p pvar ~loc arg1] : [%t ty]) [%p pvar ~loc arg2] -> [%e body]]
  ;;
end

module Equal = Make (Equal_params)
