open Ppx_compare_lib.Builtin

let failwith = `Should_refer_to_runtime_lib
let ignore = `Should_refer_to_runtime_lib
let ( = ) = `Should_refer_to_runtime_lib
let ( <> ) = `Should_refer_to_runtime_lib
let ( == ) = `Should_refer_to_runtime_lib
let ( != ) = `Should_refer_to_runtime_lib
let ( > ) = `Should_refer_to_runtime_lib
let ( < ) = `Should_refer_to_runtime_lib
let ( >= ) = `Should_refer_to_runtime_lib
let ( <= ) = `Should_refer_to_runtime_lib
let max = `Should_refer_to_runtime_lib
let min = `Should_refer_to_runtime_lib
let equal = `Should_refer_to_runtime_lib
let compare = `Should_refer_to_runtime_lib

module type M1_sig = sig
  type t [@@deriving_inline compare, equal, compare ~localize, equal ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S with type t := t
    include Ppx_compare_lib.Equal.S with type t := t
    include Ppx_compare_lib.Comparable.S with type t := t
    include Ppx_compare_lib.Comparable.S__local with type t := t
    include Ppx_compare_lib.Equal.S with type t := t
    include Ppx_compare_lib.Equal.S__local with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type M1_sig_wrong_name = sig
  type t1 [@@deriving_inline compare, equal, compare ~localize, equal ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    val compare_t1 : t1 -> t1 -> int
    val equal_t1 : t1 -> t1 -> bool
    val compare_t1 : t1 -> t1 -> int
    val compare_t1__local : t1 -> t1 -> int
    val equal_t1 : t1 -> t1 -> bool
    val equal_t1__local : t1 -> t1 -> bool
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type M2_sig = sig
  type 'a t [@@deriving_inline compare, equal, compare ~localize, equal ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Comparable.S1__local with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S1__local with type 'a t := 'a t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type M2_sig_wrong_name = sig
  type 'a t1 [@@deriving_inline compare, equal, compare ~localize, equal ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    val compare_t1 : 'a. ('a -> 'a -> int) -> 'a t1 -> 'a t1 -> int
    val equal_t1 : 'a. ('a -> 'a -> bool) -> 'a t1 -> 'a t1 -> bool
    val compare_t1 : 'a. ('a -> 'a -> int) -> 'a t1 -> 'a t1 -> int
    val compare_t1__local : 'a. ('a -> 'a -> int) -> 'a t1 -> 'a t1 -> int
    val equal_t1 : 'a. ('a -> 'a -> bool) -> 'a t1 -> 'a t1 -> bool
    val equal_t1__local : 'a. ('a -> 'a -> bool) -> 'a t1 -> 'a t1 -> bool
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module M1 = struct
  type t = unit [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M2 = struct
  type t = int [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M3 = struct
  type t = bool [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M4 = struct
  type t = int32 [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M5 = struct
  type t = nativeint [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M6 = struct
  type t = int64 [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M7 = struct
  type t = float [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M8 = struct
  type t = bool * float [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M9 = struct
  type t = bool * float * int
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M10 = struct
  type t = bool * float * int * string
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M11 = struct
  type t = int ref [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M12 = struct
  type t = (float * float) option
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M13 = struct
  type t = float array [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M14 = struct
  type t = (int * int) array
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M15 = struct
  type t = float array array
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M16 = struct
  type t = int list [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M17 = struct
  type t =
    { s : string
    ; b : float array list
    ; mutable c : int * int64 option
    }
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M18 = struct
  type t =
    { a : float
    ; b : float
    ; c : float
    }
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M19 = struct
  type t = Foo [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M20 = struct
  type t = Foo of int [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M21 = struct
  type t = Foo of int * float
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M22 = struct
  type t =
    | Foo
    | Bar of int
    | Baz of string option
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M23 = struct
  type t =
    [ `Foo
    | `Bar of string * string
    ]
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M24 = struct
  type t = int * string * [ `Foo | `Bar ]
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M25 = struct
  (* no local comparison for String.t, only for string, so we don't test that *)
  type t = String.t [@@deriving compare, equal]
end

module type M26_sig = sig
  type 'a t [@@deriving_inline compare, equal, compare ~localize, equal ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Comparable.S1__local with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S1__local with type 'a t := 'a t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module M26 = struct
  type 'a t = 'a array [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module MyList = struct
  type 'a t =
    | Nil
    | Node of 'a * 'a t
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M27 = struct
  type t = int [@@deriving compare, equal, compare ~localize, equal ~localize]

  module Inner = struct
    type nonrec t = t list [@@deriving compare, equal, compare ~localize, equal ~localize]

    let _ = ((compare : int list -> int list -> int) : t -> t -> int)
  end
end

module M28 = struct
  (* making sure that nobody is reversing the type parameters *)
  type ('a, 'b) t = ('a * 'b) list
  [@@deriving compare, equal, compare ~localize, equal ~localize]

  let (_ : (int, float) t -> int) = [%compare: (int, float) t] [ 1, nan ]
end

module M29 = struct
  type t =
    | A of
        { a : float
        ; b : float
        ; c : float
        }
    | B of float * float * float
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module M30 = struct
  type ('a, 'b) t =
    | A of
        { a : 'a
        ; b : 'b
        ; c : float
        }
    | B of 'a * 'b
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module type Polyrec_sig = sig
  type ('a, 'b) t = T of ('a option, 'b) t [@@deriving_inline compare, equal]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S2 with type ('a, 'b) t := ('a, 'b) t
    include Ppx_compare_lib.Equal.S2 with type ('a, 'b) t := ('a, 'b) t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  type ('a, 'b) t1 = T of ('a option, 'b) t2

  and ('a, 'b) t2 =
    | T1 of ('a list, 'b) t1
    | T2 of ('a, 'b list) t2
  [@@deriving_inline compare, equal]

  include sig
    [@@@ocaml.warning "-32"]

    val compare_t1
      : 'a 'b.
      ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t1 -> ('a, 'b) t1 -> int

    val compare_t2
      : 'a 'b.
      ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t2 -> ('a, 'b) t2 -> int

    val equal_t1
      : 'a 'b.
      ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t1 -> ('a, 'b) t1 -> bool

    val equal_t2
      : 'a 'b.
      ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t2 -> ('a, 'b) t2 -> bool
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Polyrec = struct
  type ('a, 'b) t = T of ('a option, 'b) t
  [@@deriving compare, equal, compare ~localize, equal ~localize]

  type ('a, 'b) t1 = T of ('a option, 'b) t2

  and ('a, 'b) t2 =
    | T1 of ('a list, 'b) t1
    | T2 of ('a, 'b list) t2
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module type Variance_sig = sig
  type +'a t [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module Variance = struct
  type -'a t [@@deriving compare, equal, compare ~localize, equal ~localize]

  type (-'a, +'b) u = 'a t * 'b
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module Test = struct
  let ( = ) : int -> int -> bool = Base.Poly.( = )

  (* checking that for the types mentioned in the readme, we compare structurally  *)
  let%test _ = [%compare: unit option] None (Some ()) = Base.Poly.compare None (Some ())
  let%test _ = [%compare: unit list] [] [ () ] = Base.Poly.compare [] [ () ]

  let%test _ =
    [%compare: int array] [| 0; 1 |] [| 1 |] = Base.Poly.compare [| 0; 1 |] [| 1 |]
  ;;

  let%test _ =
    Base.Poly.( = )
      (List.sort [%compare: int option] [ Some 3; None; Some 2; Some 1 ])
      [ None; Some 1; Some 2; Some 3 ]
  ;;
end

module Variant_inclusion = struct
  type 'a type1 = [ `T1 of 'a ]
  [@@deriving compare, equal, compare ~localize, equal ~localize]

  type 'a type2 =
    [ 'a type1
    | `T2
    ]
  [@@deriving compare, equal, compare ~localize, equal ~localize]

  type 'a type3 =
    [ `T3
    | 'a type1
    ]
  [@@deriving compare, equal, compare ~localize, equal ~localize]

  type 'a type4 =
    [ 'a type2
    | `T4
    | 'a type3
    ]
  [@@deriving compare, equal, compare ~localize, equal ~localize]

  type 'a id = 'a [@@deriving compare, equal, compare ~localize, equal ~localize]

  type ('a, 'b) u = [ `u of 'a * 'b ]
  [@@deriving compare, equal, compare ~localize, equal ~localize]

  type t = [ | (int, int) u ]
  [@@deriving compare, equal, compare ~localize, equal ~localize]
end

module Equal = struct
  let%test _ = [%compare.equal: int list] [ 7; 8; 9 ] [ 7; 8; 9 ]
  let%test _ = not ([%compare.equal: int list] [ 7; 8 ] [ 7; 8; 9 ])

  let%test _ =
    match [%compare: int * int] (1, 2) (1, 3) with
    | -1 -> true
    | _ -> false
  ;;

  let%test _ =
    match [%compare: int * int] (1, 3) (1, 2) with
    | 1 -> true
    | _ -> false
  ;;

  let%test _ = [%compare.equal: string option] None None
  let%test _ = not ([%compare.equal: string option] (Some "foo") None)
  let%test _ = [%compare.equal: string] "hello" "hello"
  let%test _ = not ([%compare.equal: string] "hello" "goodbye")
end

module Equal_local = struct
  let%test _ = [%compare_local.equal: int list] [ 7; 8; 9 ] [ 7; 8; 9 ]
  let%test _ = not ([%compare_local.equal: int list] [ 7; 8 ] [ 7; 8; 9 ])

  let%test _ =
    match [%compare_local: int * int] (1, 2) (1, 3) with
    | -1 -> true
    | _ -> false
  ;;

  let%test _ =
    match [%compare_local: int * int] (1, 3) (1, 2) with
    | 1 -> true
    | _ -> false
  ;;

  let%test _ = [%compare_local.equal: string option] None None
  let%test _ = not ([%compare_local.equal: string option] (Some "foo") None)
  let%test _ = [%compare_local.equal: string] "hello" "hello"
  let%test _ = not ([%compare_local.equal: string] "hello" "goodbye")
end

module Type_extensions : sig
  (* Making sure we don't generate [_ t -> _ t -> int], as
     that's too general. *)
  module type S = sig
    type 'a t

    val compare : [%compare: _ t]
    val equal : [%compare.equal: _ t]
    val compare__local : [%compare_local: _ t]
    val equal__local : [%compare_local.equal: _ t]
  end
end = struct
  module type S = sig
    type 'a t

    val compare : 'a t -> 'a t -> int
    val equal : 'a t -> 'a t -> bool
    val compare__local : 'a t -> 'a t -> int
    val equal__local : 'a t -> 'a t -> bool
  end
end

module Ignoring_field = struct
  type t =
    { a : int [@ignore]
    ; b : int
    ; c : int
    }
  [@@deriving_inline compare, equal]

  let _ = fun (_ : t) -> ()

  let compare =
    (fun a__1375_ b__1376_ ->
       if Stdlib.( == ) a__1375_ b__1376_
       then 0
       else (
         match compare_int a__1375_.b b__1376_.b with
         | 0 -> compare_int a__1375_.c b__1376_.c
         | n -> n)
     : t -> t -> int)
  ;;

  let _ = compare

  let equal =
    (fun a__1377_ b__1378_ ->
       if Stdlib.( == ) a__1377_ b__1378_
       then true
       else
         Stdlib.( && ) (equal_int a__1377_.b b__1378_.b) (equal_int a__1377_.c b__1378_.c)
     : t -> t -> bool)
  ;;

  let _ = equal

  [@@@end]

  let equal = [%compare.equal: t]
end

module Ignoring_inline = struct
  type t = int * int * int

  let compare = [%compare: _ * (int[@ignore]) * int]
  let _ = compare
  let equal = [%compare.equal: t]
  let%test _ = equal (0, 1, 2) (9, 1, 2)
  let%test _ = equal (0, 1, 2) (0, 9, 2)
  let%test _ = not (equal (0, 1, 2) (0, 1, 9))
end

module Ignoring = struct
  type t = { a : (int[@ignore]) * string } [@@deriving_inline compare, equal]

  let _ = fun (_ : t) -> ()

  let compare =
    (fun a__1395_ b__1396_ ->
       if Stdlib.( == ) a__1395_ b__1396_
       then 0
       else (
         let t__1397_, t__1398_ = a__1395_.a in
         let t__1399_, t__1400_ = b__1396_.a in
         match
           let (_ : _) = t__1397_
           and (_ : _) = t__1399_ in
           0
         with
         | 0 -> compare_string t__1398_ t__1400_
         | n -> n)
     : t -> t -> int)
  ;;

  let _ = compare

  let equal =
    (fun a__1401_ b__1402_ ->
       if Stdlib.( == ) a__1401_ b__1402_
       then true
       else (
         let t__1403_, t__1404_ = a__1401_.a in
         let t__1405_, t__1406_ = b__1402_.a in
         Stdlib.( && )
           (let (_ : _) = t__1403_
            and (_ : _) = t__1405_ in
            true)
           (equal_string t__1404_ t__1406_))
     : t -> t -> bool)
  ;;

  let _ = equal

  [@@@end]

  let%test _ = equal { a = 1, "hi" } { a = 2, "hi" }
  let%test _ = not (equal { a = 1, "hi" } { a = 1, "ho" })
end

module Ignoring_with_type = struct
  type t =
    { a : int
    ; b : (int[@compare.ignore])
    }
  [@@deriving_inline compare]

  let _ = fun (_ : t) -> ()

  let compare =
    (fun a__1407_ b__1408_ ->
       if Stdlib.( == ) a__1407_ b__1408_
       then 0
       else (
         match compare_int a__1407_.a b__1408_.a with
         | 0 ->
           let (_ : _) = a__1407_.b
           and (_ : _) = b__1408_.b in
           0
         | n -> n)
     : t -> t -> int)
  ;;

  let _ = compare

  [@@@end]
end

module Enum_optim = struct
  type t =
    | A
    | B
    | C
  [@@deriving_inline compare, equal]

  let _ = fun (_ : t) -> ()
  let compare = (Stdlib.compare : t -> t -> int)
  let _ = compare
  let equal = (Stdlib.( = ) : t -> t -> bool)
  let _ = equal

  [@@@end]
end

module Lazy_behavior = struct
  (* Test that the generated functions don't evaluate more than necessary *)
  type a = unit

  let equal_a () () = assert false
  let equal_a__local () () = assert false
  let compare_a () () = assert false
  let compare_a__local () () = assert false

  type b = int * a [@@deriving compare, equal, compare ~localize, equal ~localize]

  let%test _ = not (equal_b (0, ()) (1, ()))
  let%test _ = Base.Poly.( < ) (compare_b (0, ()) (1, ())) 0
end

module Not_ieee_compliant = struct
  type t = float [@@deriving compare, equal, compare ~localize, equal ~localize]

  let%test _ = [%equal: t] nan nan
  let%test _ = Base.Poly.( = ) ([%compare: t] nan nan) 0
end

module Wildcard : sig
  type _ transparent = int [@@deriving compare, equal, compare ~localize, equal ~localize]
  type _ opaque [@@deriving compare, equal, compare ~localize, equal ~localize]
end = struct
  type _ transparent = int [@@deriving compare, equal, compare ~localize, equal ~localize]

  let%test _ = [%equal: string transparent] 1 1
  let%test _ = not ([%equal: string transparent] 1 2)
  let%test _ = Base.Poly.( < ) ([%compare: string transparent] 1 2) 0
  let%test _ = Base.Poly.( = ) ([%compare: string transparent] 1 1) 0
  let%test _ = Base.Poly.( > ) ([%compare: string transparent] 2 1) 0

  type 'a opaque = 'a option
  [@@deriving compare, equal, compare ~localize, equal ~localize]

  let%test _ = [%equal: int opaque] (Some 1) (Some 1)
  let%test _ = not ([%equal: int opaque] None (Some 1))
  let%test _ = not ([%equal: int opaque] (Some 1) (Some 2))
  let%test _ = Base.Poly.( < ) ([%compare: int opaque] None (Some 1)) 0
  let%test _ = Base.Poly.( = ) ([%compare: int opaque] (Some 1) (Some 1)) 0
  let%test _ = Base.Poly.( > ) ([%compare: int opaque] (Some 2) (Some 1)) 0
end

module Local_with_aliased_comparisons : sig
  type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
  [@@deriving_inline compare ~localize, equal ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S with type t := t
    include Ppx_compare_lib.Comparable.S__local with type t := t
    include Ppx_compare_lib.Equal.S with type t := t
    include Ppx_compare_lib.Equal.S__local with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : t) -> ()

  let rec compare__local =
    (fun a__1541_ b__1542_ ->
       if Stdlib.( == ) a__1541_ b__1542_
       then 0
       else (
         match a__1541_, b__1542_ with
         | Int _a__1543_, Int _b__1544_ -> compare_int__local _a__1543_ _b__1544_
         | Int _, _ -> -1
         | _, Int _ -> 1
         | Add (_a__1545_, _a__1547_), Add (_b__1546_, _b__1548_) ->
           (match compare__local _a__1545_ _b__1546_ with
            | 0 -> compare__local _a__1547_ _b__1548_
            | n -> n)
         | Add _, _ -> -1
         | _, Add _ -> 1
         | Sub (_a__1549_, _a__1551_), Sub (_b__1550_, _b__1552_) ->
           (match compare__local _a__1549_ _b__1550_ with
            | 0 -> compare__local _a__1551_ _b__1552_
            | n -> n))
     : t -> t -> int)
  ;;

  let _ = compare__local
  let compare = (fun a b -> compare__local a b : t -> t -> int)
  let _ = compare

  let rec equal__local =
    (fun a__1553_ b__1554_ ->
       if Stdlib.( == ) a__1553_ b__1554_
       then true
       else (
         match a__1553_, b__1554_ with
         | Int _a__1555_, Int _b__1556_ -> equal_int__local _a__1555_ _b__1556_
         | Int _, _ -> false
         | _, Int _ -> false
         | Add (_a__1557_, _a__1559_), Add (_b__1558_, _b__1560_) ->
           Stdlib.( && )
             (equal__local _a__1557_ _b__1558_)
             (equal__local _a__1559_ _b__1560_)
         | Add _, _ -> false
         | _, Add _ -> false
         | Sub (_a__1561_, _a__1563_), Sub (_b__1562_, _b__1564_) ->
           Stdlib.( && )
             (equal__local _a__1561_ _b__1562_)
             (equal__local _a__1563_ _b__1564_))
     : t -> t -> bool)
  ;;

  let _ = equal__local
  let equal = (fun a b -> equal__local a b : t -> t -> bool)
  let _ = equal

  [@@@end]
end

module Polymorphic_variants_fully_poly_inputs : sig
  module Upper_bound_only : sig
    val compare : [< `A | `B | `C of int ] -> [< `A | `B | `C of int ] -> int
    val equal : [< `A | `B | `C of int ] -> [< `A | `B | `C of int ] -> bool
  end

  module Upper_and_lower_bounds : sig
    val compare : [< `A | `B | `C of int > `A ] -> [< `A | `B | `C of int > `A ] -> int
    val equal : [< `A | `B | `C of int > `A ] -> [< `A | `B | `C of int > `A ] -> bool
  end
end = struct
  module Upper_bound_only = struct
    let compare = [%compare: [< `A | `B | `C of int ]]
    let equal = [%compare.equal: [< `A | `B | `C of int ]]
  end

  module Upper_and_lower_bounds = struct
    let compare = [%compare: [< `A | `B | `C of int > `A ]]
    let equal = [%compare.equal: [< `A | `B | `C of int > `A ]]
  end
end
