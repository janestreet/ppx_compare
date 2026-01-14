include Ppx_compare_lib_intf.Definitions

[@@@warning "-incompatible-with-upstream"]

external ( = ) : (int[@local_opt]) -> (int[@local_opt]) -> bool @@ portable = "%equal"
external ( <> ) : (int[@local_opt]) -> (int[@local_opt]) -> bool @@ portable = "%notequal"
external compare : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%compare"
external equal : (int[@local_opt]) -> (int[@local_opt]) -> bool @@ portable = "%equal"

module Poly = struct
  external compare : ('a[@local_opt]) -> ('a[@local_opt]) -> int @@ portable = "%compare"
  external equal : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%equal"
end

module Array = struct
  external length
    : ('a : any mod separable).
    'a array @ local -> int
    @@ portable
    = "%array_length"
  [@@layout_poly]

  external unsafe_get
    : ('a : any mod separable).
    'a array @ local -> int -> 'a
    @@ portable
    = "%array_unsafe_get"
  [@@layout_poly]
end

let compare_abstract ~type_name _ _ =
  Printf.ksprintf
    failwith
    "Compare called on the type %s, which is abstract in an implementation."
    type_name
;;

let equal_abstract ~type_name _ _ =
  Printf.ksprintf
    failwith
    "Equal called on the type %s, which is abstract in an implementation."
    type_name
;;

module Builtin = struct
  [%%template
  [@@@mode.default l = (global, local)]

  let compare_bool : (bool compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_char : (char compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_float : (float compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_int : (int compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_int32 : (int32 compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_int64 : (int64 compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_nativeint : (nativeint compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_string : (string compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_bytes : (bytes compare[@mode l]) = fun x y -> Poly.compare x y
  let compare_unit : (unit compare[@mode l]) = fun x y -> Poly.compare x y

  let rec compare_list compare_elt (a @ l) (b @ l) =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let res = compare_elt x y in
      if res <> 0 then res else (compare_list [@mode l]) compare_elt xs ys
  ;;

  let compare_option compare_elt (a @ l) (b @ l) =
    match a, b with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> compare_elt a b
  ;;]

  [%%template
  [@@@kind.default k = base_or_null]

  let[@mode local] compare_array
    (type a : k mod separable)
    compare_elt
    (a : a array @ local)
    (b : a array @ local)
    =
    if a == b
    then 0
    else (
      let len_a = Array.length a in
      let len_b = Array.length b in
      let ret = compare len_a len_b in
      if ret <> 0
      then ret
      else (
        let rec loop i =
          if i = len_a
          then 0
          else (
            let l = Array.unsafe_get a i
            and r = Array.unsafe_get b i in
            let res = compare_elt l r in
            if res <> 0 then res else loop (i + 1))
        in
        loop 0 [@nontail]))
  ;;

  let compare_array = [%eta3 compare_array [@kind k] [@mode local]]]

  [%%template
  let[@mode local] compare_ref compare_elt (a @ local) (b @ local) = compare_elt !a !b
  let compare_ref = [%eta3 compare_ref [@mode local]]]

  [%%template
  [@@@mode.default l = (global, local)]

  let equal_bool : (bool equal[@mode l]) = fun x y -> Poly.equal x y
  let equal_char : (char equal[@mode l]) = fun x y -> Poly.equal x y
  let equal_int : (int equal[@mode l]) = fun x y -> Poly.equal x y
  let equal_int32 : (int32 equal[@mode l]) = fun x y -> Poly.equal x y
  let equal_int64 : (int64 equal[@mode l]) = fun x y -> Poly.equal x y
  let equal_nativeint : (nativeint equal[@mode l]) = fun x y -> Poly.equal x y
  let equal_string : (string equal[@mode l]) = fun x y -> Poly.equal x y
  let equal_bytes : (bytes equal[@mode l]) = fun x y -> Poly.equal x y
  let equal_unit : (unit equal[@mode l]) = fun x y -> Poly.equal x y

  (* [Poly.equal] is IEEE compliant, which is not what we want here. *)
  let equal_float x y = equal_int ((compare_float [@mode l]) x y) 0

  let rec equal_list equal_elt (a @ l) (b @ l) =
    match a, b with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x :: xs, y :: ys -> equal_elt x y && (equal_list [@mode l]) equal_elt xs ys
  ;;

  let equal_option equal_elt (a @ l) (b @ l) =
    match a, b with
    | None, None -> true
    | None, Some _ | Some _, None -> false
    | Some a, Some b -> equal_elt a b
  ;;]

  [%%template
  [@@@kind.default k = base_or_null]

  let[@mode local] equal_array
    (type a : k mod separable)
    equal_elt
    (a : a array @ local)
    (b : a array @ local)
    =
    a == b
    ||
    let len_a = Array.length a in
    let len_b = Array.length b in
    equal len_a len_b
    &&
    let rec loop i =
      i = len_a
      ||
      let l = Array.unsafe_get a i
      and r = Array.unsafe_get b i in
      equal_elt l r && loop (i + 1)
    in
    loop 0 [@nontail]
  ;;

  let equal_array = [%eta3 equal_array [@kind k] [@mode local]]]

  [%%template
  let[@mode local] equal_ref equal_elt (a @ local) (b @ local) = equal_elt !a !b
  let equal_ref = [%eta3 equal_ref [@mode local]]]
end
