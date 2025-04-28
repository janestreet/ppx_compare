include Ppx_compare_lib_intf.Definitions

external ( = ) : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%equal"
external ( <> ) : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%notequal"
external compare : (int[@local_opt]) -> (int[@local_opt]) -> int = "%compare"
external equal : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%equal"

module Poly = struct
  external compare : ('a[@local_opt]) -> ('a[@local_opt]) -> int = "%compare"
  external equal : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%equal"
end

module Array = struct
  external length : 'a array -> int = "%array_length"
  external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
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
  let compare_bool : bool compare = fun x y -> Poly.compare x y
  let compare_bool__local : bool compare__local = fun x y -> Poly.compare x y
  let compare_char : char compare = fun x y -> Poly.compare x y
  let compare_char__local : char compare__local = fun x y -> Poly.compare x y
  let compare_float : float compare = fun x y -> Poly.compare x y
  let compare_float__local : float compare__local = fun x y -> Poly.compare x y
  let compare_int : int compare = fun x y -> Poly.compare x y
  let compare_int__local : int compare__local = fun x y -> Poly.compare x y
  let compare_int32 : int32 compare = fun x y -> Poly.compare x y
  let compare_int32__local : int32 compare__local = fun x y -> Poly.compare x y
  let compare_int64 : int64 compare = fun x y -> Poly.compare x y
  let compare_int64__local : int64 compare__local = fun x y -> Poly.compare x y
  let compare_nativeint : nativeint compare = fun x y -> Poly.compare x y
  let compare_nativeint__local : nativeint compare__local = fun x y -> Poly.compare x y
  let compare_string : string compare = fun x y -> Poly.compare x y
  let compare_string__local : string compare__local = fun x y -> Poly.compare x y
  let compare_bytes : bytes compare = fun x y -> Poly.compare x y
  let compare_bytes__local : bytes compare__local = fun x y -> Poly.compare x y
  let compare_unit : unit compare = fun x y -> Poly.compare x y
  let compare_unit__local : unit compare__local = fun x y -> Poly.compare x y

  let compare_array__local compare_elt a b =
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

  let compare_array compare_elt a b = compare_array__local compare_elt a b

  let rec compare_list compare_elt a b =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let res = compare_elt x y in
      if res <> 0 then res else compare_list compare_elt xs ys
  ;;

  let rec compare_list__local compare_elt__local a b =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let res = compare_elt__local x y in
      if res <> 0 then res else compare_list__local compare_elt__local xs ys
  ;;

  let compare_option compare_elt a b =
    match a, b with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> compare_elt a b
  ;;

  let compare_option__local compare_elt__local a b =
    match a, b with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> compare_elt__local a b
  ;;

  let compare_ref compare_elt a b = compare_elt !a !b
  let compare_ref__local compare_elt a b = compare_elt !a !b
  let equal_bool : bool equal = fun x y -> Poly.equal x y
  let equal_bool__local : bool equal__local = fun x y -> Poly.equal x y
  let equal_char : char equal = fun x y -> Poly.equal x y
  let equal_char__local : char equal__local = fun x y -> Poly.equal x y
  let equal_int : int equal = fun x y -> Poly.equal x y
  let equal_int__local : int equal__local = fun x y -> Poly.equal x y
  let equal_int32 : int32 equal = fun x y -> Poly.equal x y
  let equal_int32__local : int32 equal__local = fun x y -> Poly.equal x y
  let equal_int64 : int64 equal = fun x y -> Poly.equal x y
  let equal_int64__local : int64 equal__local = fun x y -> Poly.equal x y
  let equal_nativeint : nativeint equal = fun x y -> Poly.equal x y
  let equal_nativeint__local : nativeint equal__local = fun x y -> Poly.equal x y
  let equal_string : string equal = fun x y -> Poly.equal x y
  let equal_string__local : string equal__local = fun x y -> Poly.equal x y
  let equal_bytes : bytes equal = fun x y -> Poly.equal x y
  let equal_bytes__local : bytes equal__local = fun x y -> Poly.equal x y
  let equal_unit : unit equal = fun x y -> Poly.equal x y
  let equal_unit__local : unit equal__local = fun x y -> Poly.equal x y

  (* [Poly.equal] is IEEE compliant, which is not what we want here. *)
  let equal_float x y = equal_int (compare_float x y) 0
  let equal_float__local x y = equal_int (compare_float__local x y) 0

  let equal_array__local equal_elt a b =
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

  let equal_array equal_elt a b = equal_array__local equal_elt a b

  let rec equal_list equal_elt a b =
    match a, b with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x :: xs, y :: ys -> equal_elt x y && equal_list equal_elt xs ys
  ;;

  let rec equal_list__local equal_elt__local a b =
    match a, b with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x :: xs, y :: ys -> equal_elt__local x y && equal_list__local equal_elt__local xs ys
  ;;

  let equal_option equal_elt a b =
    match a, b with
    | None, None -> true
    | None, Some _ | Some _, None -> false
    | Some a, Some b -> equal_elt a b
  ;;

  let equal_option__local equal_elt__local a b =
    match a, b with
    | None, None -> true
    | None, Some _ | Some _, None -> false
    | Some a, Some b -> equal_elt__local a b
  ;;

  let equal_ref equal_elt a b = equal_elt !a !b
  let equal_ref__local equal_elt a b = equal_elt !a !b
end
