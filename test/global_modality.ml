open! Base

module A : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
end = struct
  type t = int [@@deriving compare, equal]
end

module A1 : sig
  type 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end = struct
  type 'a t = 'a list [@@deriving compare, equal]
end

(* Tests without inline *)

module No_inline = struct
  type simple_rec =
    { x : int
    ; y : A.t @@ global
    }
  [@@deriving compare ~localize, equal ~localize]

  type simple_var =
    | A of int
    | B of A.t @@ global
  [@@deriving compare ~localize, equal ~localize]

  type simple_rec1 =
    { x : int
    ; y : string A1.t @@ global
    }
  [@@deriving compare ~localize, equal ~localize]

  type simple_var1 =
    | A of int
    | B of string A1.t @@ global
  [@@deriving compare ~localize, equal ~localize]

  type 'a might_need_eta_rec =
    { x : 'a
    ; y : 'a A1.t @@ global
    }
  [@@deriving compare ~localize, equal ~localize]

  type 'a might_need_eta_var =
    | A of 'a
    | B of 'a A1.t @@ global
  [@@deriving compare ~localize, equal ~localize]

  type 'a mut =
    { x : int
    ; mutable y : A.t
    }
  [@@deriving compare ~localize, equal ~localize]

  type recursive =
    { x : int
    ; y : recursive A1.t @@ global
    }
  [@@deriving compare ~localize, equal ~localize]

  type 'a recursive1 =
    { x : int
    ; y : 'a A1.t recursive1 @@ global
    }
  [@@deriving compare ~localize, equal ~localize]

  module Test_nonrec = struct
    type nonrec 'a recursive1 =
      { x : int
      ; y : 'a A1.t recursive1 @@ global
      }
    [@@deriving compare ~localize, equal ~localize]
  end
end

(* Tests with inline --- there was previously a bug in [deriving_inline] interaction that
   resulted in extra eta-reductions that caused type errors in the last two tests. *)

module Inline = struct
  type simple_rec =
    { x : int
    ; y : A.t @@ global
    }
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : simple_rec) -> ()

  let compare_simple_rec__local =
    (fun a__165_ b__166_ ->
       if Stdlib.( == ) a__165_ b__166_
       then 0
       else (
         match compare_int__local a__165_.x b__166_.x with
         | 0 -> A.compare a__165_.y b__166_.y
         | n -> n)
     : local_ simple_rec -> local_ simple_rec -> int)
  ;;

  let _ = compare_simple_rec__local

  let compare_simple_rec =
    (fun a b -> compare_simple_rec__local a b : simple_rec -> simple_rec -> int)
  ;;

  let _ = compare_simple_rec

  let equal_simple_rec__local =
    (fun a__167_ b__168_ ->
       if Stdlib.( == ) a__167_ b__168_
       then true
       else
         Stdlib.( && )
           (equal_int__local a__167_.x b__168_.x)
           (A.equal a__167_.y b__168_.y)
     : local_ simple_rec -> local_ simple_rec -> bool)
  ;;

  let _ = equal_simple_rec__local

  let equal_simple_rec =
    (fun a b -> equal_simple_rec__local a b : simple_rec -> simple_rec -> bool)
  ;;

  let _ = equal_simple_rec

  [@@@end]

  type simple_var =
    | A of int
    | B of A.t @@ global
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : simple_var) -> ()

  let compare_simple_var__local =
    (fun a__169_ b__170_ ->
       if Stdlib.( == ) a__169_ b__170_
       then 0
       else (
         match a__169_, b__170_ with
         | A _a__171_, A _b__172_ -> compare_int__local _a__171_ _b__172_
         | A _, _ -> -1
         | _, A _ -> 1
         | B _a__173_, B _b__174_ -> A.compare _a__173_ _b__174_)
     : local_ simple_var -> local_ simple_var -> int)
  ;;

  let _ = compare_simple_var__local

  let compare_simple_var =
    (fun a b -> compare_simple_var__local a b : simple_var -> simple_var -> int)
  ;;

  let _ = compare_simple_var

  let equal_simple_var__local =
    (fun a__175_ b__176_ ->
       if Stdlib.( == ) a__175_ b__176_
       then true
       else (
         match a__175_, b__176_ with
         | A _a__177_, A _b__178_ -> equal_int__local _a__177_ _b__178_
         | A _, _ -> false
         | _, A _ -> false
         | B _a__179_, B _b__180_ -> A.equal _a__179_ _b__180_)
     : local_ simple_var -> local_ simple_var -> bool)
  ;;

  let _ = equal_simple_var__local

  let equal_simple_var =
    (fun a b -> equal_simple_var__local a b : simple_var -> simple_var -> bool)
  ;;

  let _ = equal_simple_var

  [@@@end]

  type simple_rec1 =
    { x : int
    ; y : string A1.t @@ global
    }
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : simple_rec1) -> ()

  let compare_simple_rec1__local =
    (fun a__181_ b__182_ ->
       if Stdlib.( == ) a__181_ b__182_
       then 0
       else (
         match compare_int__local a__181_.x b__182_.x with
         | 0 ->
           A1.compare
             (fun a__183_ b__184_ -> compare_string a__183_ b__184_)
             a__181_.y
             b__182_.y
         | n -> n)
     : local_ simple_rec1 -> local_ simple_rec1 -> int)
  ;;

  let _ = compare_simple_rec1__local

  let compare_simple_rec1 =
    (fun a b -> compare_simple_rec1__local a b : simple_rec1 -> simple_rec1 -> int)
  ;;

  let _ = compare_simple_rec1

  let equal_simple_rec1__local =
    (fun a__185_ b__186_ ->
       if Stdlib.( == ) a__185_ b__186_
       then true
       else
         Stdlib.( && )
           (equal_int__local a__185_.x b__186_.x)
           (A1.equal
              (fun a__187_ b__188_ -> equal_string a__187_ b__188_)
              a__185_.y
              b__186_.y)
     : local_ simple_rec1 -> local_ simple_rec1 -> bool)
  ;;

  let _ = equal_simple_rec1__local

  let equal_simple_rec1 =
    (fun a b -> equal_simple_rec1__local a b : simple_rec1 -> simple_rec1 -> bool)
  ;;

  let _ = equal_simple_rec1

  [@@@end]

  type simple_var1 =
    | A of int
    | B of string A1.t @@ global
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : simple_var1) -> ()

  let compare_simple_var1__local =
    (fun a__189_ b__190_ ->
       if Stdlib.( == ) a__189_ b__190_
       then 0
       else (
         match a__189_, b__190_ with
         | A _a__191_, A _b__192_ -> compare_int__local _a__191_ _b__192_
         | A _, _ -> -1
         | _, A _ -> 1
         | B _a__193_, B _b__194_ ->
           A1.compare
             (fun a__195_ b__196_ -> compare_string a__195_ b__196_)
             _a__193_
             _b__194_)
     : local_ simple_var1 -> local_ simple_var1 -> int)
  ;;

  let _ = compare_simple_var1__local

  let compare_simple_var1 =
    (fun a b -> compare_simple_var1__local a b : simple_var1 -> simple_var1 -> int)
  ;;

  let _ = compare_simple_var1

  let equal_simple_var1__local =
    (fun a__197_ b__198_ ->
       if Stdlib.( == ) a__197_ b__198_
       then true
       else (
         match a__197_, b__198_ with
         | A _a__199_, A _b__200_ -> equal_int__local _a__199_ _b__200_
         | A _, _ -> false
         | _, A _ -> false
         | B _a__201_, B _b__202_ ->
           A1.equal
             (fun a__203_ b__204_ -> equal_string a__203_ b__204_)
             _a__201_
             _b__202_)
     : local_ simple_var1 -> local_ simple_var1 -> bool)
  ;;

  let _ = equal_simple_var1__local

  let equal_simple_var1 =
    (fun a b -> equal_simple_var1__local a b : simple_var1 -> simple_var1 -> bool)
  ;;

  let _ = equal_simple_var1

  [@@@end]

  type 'a might_need_eta_rec =
    { x : 'a
    ; y : 'a A1.t @@ global
    }
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : 'a might_need_eta_rec) -> ()

  let compare_might_need_eta_rec__local
    : 'a.
    (local_ 'a -> local_ 'a -> int)
    -> local_ 'a might_need_eta_rec
    -> local_ 'a might_need_eta_rec
    -> int
    =
    fun _cmp__a a__205_ b__206_ ->
    if Stdlib.( == ) a__205_ b__206_
    then 0
    else (
      match _cmp__a a__205_.x b__206_.x with
      | 0 ->
        A1.compare (fun a__207_ b__208_ -> _cmp__a a__207_ b__208_) a__205_.y b__206_.y
      | n -> n)
  ;;

  let _ = compare_might_need_eta_rec__local

  let compare_might_need_eta_rec
    : 'a. ('a -> 'a -> int) -> 'a might_need_eta_rec -> 'a might_need_eta_rec -> int
    =
    fun _cmp__a a__209_ b__210_ ->
    if Stdlib.( == ) a__209_ b__210_
    then 0
    else (
      match _cmp__a a__209_.x b__210_.x with
      | 0 ->
        A1.compare (fun a__211_ b__212_ -> _cmp__a a__211_ b__212_) a__209_.y b__210_.y
      | n -> n)
  ;;

  let _ = compare_might_need_eta_rec

  let equal_might_need_eta_rec__local
    : 'a.
    (local_ 'a -> local_ 'a -> bool)
    -> local_ 'a might_need_eta_rec
    -> local_ 'a might_need_eta_rec
    -> bool
    =
    fun _cmp__a a__213_ b__214_ ->
    if Stdlib.( == ) a__213_ b__214_
    then true
    else
      Stdlib.( && )
        (_cmp__a a__213_.x b__214_.x)
        (A1.equal (fun a__215_ b__216_ -> _cmp__a a__215_ b__216_) a__213_.y b__214_.y)
  ;;

  let _ = equal_might_need_eta_rec__local

  let equal_might_need_eta_rec
    : 'a. ('a -> 'a -> bool) -> 'a might_need_eta_rec -> 'a might_need_eta_rec -> bool
    =
    fun _cmp__a a__217_ b__218_ ->
    if Stdlib.( == ) a__217_ b__218_
    then true
    else
      Stdlib.( && )
        (_cmp__a a__217_.x b__218_.x)
        (A1.equal (fun a__219_ b__220_ -> _cmp__a a__219_ b__220_) a__217_.y b__218_.y)
  ;;

  let _ = equal_might_need_eta_rec

  [@@@end]

  type 'a might_need_eta_var =
    | A of 'a
    | B of 'a A1.t @@ global
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : 'a might_need_eta_var) -> ()

  let compare_might_need_eta_var__local
    : 'a.
    (local_ 'a -> local_ 'a -> int)
    -> local_ 'a might_need_eta_var
    -> local_ 'a might_need_eta_var
    -> int
    =
    fun _cmp__a a__221_ b__222_ ->
    if Stdlib.( == ) a__221_ b__222_
    then 0
    else (
      match a__221_, b__222_ with
      | A _a__223_, A _b__224_ -> _cmp__a _a__223_ _b__224_
      | A _, _ -> -1
      | _, A _ -> 1
      | B _a__225_, B _b__226_ ->
        A1.compare (fun a__227_ b__228_ -> _cmp__a a__227_ b__228_) _a__225_ _b__226_)
  ;;

  let _ = compare_might_need_eta_var__local

  let compare_might_need_eta_var
    : 'a. ('a -> 'a -> int) -> 'a might_need_eta_var -> 'a might_need_eta_var -> int
    =
    fun _cmp__a a__229_ b__230_ ->
    if Stdlib.( == ) a__229_ b__230_
    then 0
    else (
      match a__229_, b__230_ with
      | A _a__231_, A _b__232_ -> _cmp__a _a__231_ _b__232_
      | A _, _ -> -1
      | _, A _ -> 1
      | B _a__233_, B _b__234_ ->
        A1.compare (fun a__235_ b__236_ -> _cmp__a a__235_ b__236_) _a__233_ _b__234_)
  ;;

  let _ = compare_might_need_eta_var

  let equal_might_need_eta_var__local
    : 'a.
    (local_ 'a -> local_ 'a -> bool)
    -> local_ 'a might_need_eta_var
    -> local_ 'a might_need_eta_var
    -> bool
    =
    fun _cmp__a a__237_ b__238_ ->
    if Stdlib.( == ) a__237_ b__238_
    then true
    else (
      match a__237_, b__238_ with
      | A _a__239_, A _b__240_ -> _cmp__a _a__239_ _b__240_
      | A _, _ -> false
      | _, A _ -> false
      | B _a__241_, B _b__242_ ->
        A1.equal (fun a__243_ b__244_ -> _cmp__a a__243_ b__244_) _a__241_ _b__242_)
  ;;

  let _ = equal_might_need_eta_var__local

  let equal_might_need_eta_var
    : 'a. ('a -> 'a -> bool) -> 'a might_need_eta_var -> 'a might_need_eta_var -> bool
    =
    fun _cmp__a a__245_ b__246_ ->
    if Stdlib.( == ) a__245_ b__246_
    then true
    else (
      match a__245_, b__246_ with
      | A _a__247_, A _b__248_ -> _cmp__a _a__247_ _b__248_
      | A _, _ -> false
      | _, A _ -> false
      | B _a__249_, B _b__250_ ->
        A1.equal (fun a__251_ b__252_ -> _cmp__a a__251_ b__252_) _a__249_ _b__250_)
  ;;

  let _ = equal_might_need_eta_var

  [@@@end]

  type 'a mut =
    { x : int
    ; mutable y : A.t
    }
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : 'a mut) -> ()

  let compare_mut__local
    : 'a. (local_ 'a -> local_ 'a -> int) -> local_ 'a mut -> local_ 'a mut -> int
    =
    fun _cmp__a a__253_ b__254_ ->
    if Stdlib.( == ) a__253_ b__254_
    then 0
    else (
      match compare_int__local a__253_.x b__254_.x with
      | 0 -> A.compare a__253_.y b__254_.y
      | n -> n)
  ;;

  let _ = compare_mut__local

  let compare_mut : 'a. ('a -> 'a -> int) -> 'a mut -> 'a mut -> int =
    fun _cmp__a a__255_ b__256_ ->
    if Stdlib.( == ) a__255_ b__256_
    then 0
    else (
      match compare_int a__255_.x b__256_.x with
      | 0 -> A.compare a__255_.y b__256_.y
      | n -> n)
  ;;

  let _ = compare_mut

  let equal_mut__local
    : 'a. (local_ 'a -> local_ 'a -> bool) -> local_ 'a mut -> local_ 'a mut -> bool
    =
    fun _cmp__a a__257_ b__258_ ->
    if Stdlib.( == ) a__257_ b__258_
    then true
    else
      Stdlib.( && ) (equal_int__local a__257_.x b__258_.x) (A.equal a__257_.y b__258_.y)
  ;;

  let _ = equal_mut__local

  let equal_mut : 'a. ('a -> 'a -> bool) -> 'a mut -> 'a mut -> bool =
    fun _cmp__a a__259_ b__260_ ->
    if Stdlib.( == ) a__259_ b__260_
    then true
    else Stdlib.( && ) (equal_int a__259_.x b__260_.x) (A.equal a__259_.y b__260_.y)
  ;;

  let _ = equal_mut

  [@@@end]

  type recursive =
    { x : int
    ; y : recursive A1.t @@ global
    }
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : recursive) -> ()

  let rec compare_recursive__local =
    (fun a__261_ b__262_ ->
       if Stdlib.( == ) a__261_ b__262_
       then 0
       else (
         match compare_int__local a__261_.x b__262_.x with
         | 0 ->
           A1.compare
             (fun a__263_ b__264_ -> compare_recursive a__263_ b__264_)
             a__261_.y
             b__262_.y
         | n -> n)
     : local_ recursive -> local_ recursive -> int)

  and compare_recursive =
    (fun a b -> compare_recursive__local a b : recursive -> recursive -> int)
  ;;

  let _ = compare_recursive__local
  and _ = compare_recursive

  let rec equal_recursive__local =
    (fun a__265_ b__266_ ->
       if Stdlib.( == ) a__265_ b__266_
       then true
       else
         Stdlib.( && )
           (equal_int__local a__265_.x b__266_.x)
           (A1.equal
              (fun a__267_ b__268_ -> equal_recursive a__267_ b__268_)
              a__265_.y
              b__266_.y)
     : local_ recursive -> local_ recursive -> bool)

  and equal_recursive =
    (fun a b -> equal_recursive__local a b : recursive -> recursive -> bool)
  ;;

  let _ = equal_recursive__local
  and _ = equal_recursive

  [@@@end]

  type 'a recursive1 =
    { x : int
    ; y : 'a A1.t recursive1 @@ global
    }
  [@@deriving_inline compare ~localize, equal ~localize]

  let _ = fun (_ : 'a recursive1) -> ()

  let rec compare_recursive1__local
    : 'a.
    (local_ 'a -> local_ 'a -> int) -> local_ 'a recursive1 -> local_ 'a recursive1 -> int
    =
    fun _cmp__a a__269_ b__270_ ->
    if Stdlib.( == ) a__269_ b__270_
    then 0
    else (
      match compare_int__local a__269_.x b__270_.x with
      | 0 ->
        compare_recursive1
          (fun a__271_ b__272_ ->
            A1.compare (fun a__273_ b__274_ -> _cmp__a a__273_ b__274_) a__271_ b__272_)
          a__269_.y
          b__270_.y
      | n -> n)

  and compare_recursive1 : 'a. ('a -> 'a -> int) -> 'a recursive1 -> 'a recursive1 -> int =
    fun _cmp__a a__275_ b__276_ ->
    if Stdlib.( == ) a__275_ b__276_
    then 0
    else (
      match compare_int a__275_.x b__276_.x with
      | 0 ->
        compare_recursive1
          (fun a__277_ b__278_ ->
            A1.compare (fun a__279_ b__280_ -> _cmp__a a__279_ b__280_) a__277_ b__278_)
          a__275_.y
          b__276_.y
      | n -> n)
  ;;

  let _ = compare_recursive1__local
  and _ = compare_recursive1

  let rec equal_recursive1__local
    : 'a.
    (local_ 'a -> local_ 'a -> bool)
    -> local_ 'a recursive1
    -> local_ 'a recursive1
    -> bool
    =
    fun _cmp__a a__281_ b__282_ ->
    if Stdlib.( == ) a__281_ b__282_
    then true
    else
      Stdlib.( && )
        (equal_int__local a__281_.x b__282_.x)
        (equal_recursive1
           (fun a__283_ b__284_ ->
             A1.equal (fun a__285_ b__286_ -> _cmp__a a__285_ b__286_) a__283_ b__284_)
           a__281_.y
           b__282_.y)

  and equal_recursive1 : 'a. ('a -> 'a -> bool) -> 'a recursive1 -> 'a recursive1 -> bool =
    fun _cmp__a a__287_ b__288_ ->
    if Stdlib.( == ) a__287_ b__288_
    then true
    else
      Stdlib.( && )
        (equal_int a__287_.x b__288_.x)
        (equal_recursive1
           (fun a__289_ b__290_ ->
             A1.equal (fun a__291_ b__292_ -> _cmp__a a__291_ b__292_) a__289_ b__290_)
           a__287_.y
           b__288_.y)
  ;;

  let _ = equal_recursive1__local
  and _ = equal_recursive1

  [@@@end]

  module Test_nonrec = struct
    type nonrec 'a recursive1 =
      { x : int
      ; y : 'a A1.t recursive1 @@ global
      }
    [@@deriving_inline compare ~localize, equal ~localize]

    let _ = fun (_ : 'a recursive1) -> ()

    let compare_recursive1__local
      : 'a.
      (local_ 'a -> local_ 'a -> int)
      -> local_ 'a recursive1
      -> local_ 'a recursive1
      -> int
      =
      fun _cmp__a a__293_ b__294_ ->
      if Stdlib.( == ) a__293_ b__294_
      then 0
      else (
        match compare_int__local a__293_.x b__294_.x with
        | 0 ->
          compare_recursive1
            (fun a__295_ b__296_ ->
              A1.compare (fun a__297_ b__298_ -> _cmp__a a__297_ b__298_) a__295_ b__296_)
            a__293_.y
            b__294_.y
        | n -> n)
    ;;

    let _ = compare_recursive1__local

    let compare_recursive1
      : 'a. ('a -> 'a -> int) -> 'a recursive1 -> 'a recursive1 -> int
      =
      fun _cmp__a a__299_ b__300_ ->
      if Stdlib.( == ) a__299_ b__300_
      then 0
      else (
        match compare_int a__299_.x b__300_.x with
        | 0 ->
          compare_recursive1
            (fun a__301_ b__302_ ->
              A1.compare (fun a__303_ b__304_ -> _cmp__a a__303_ b__304_) a__301_ b__302_)
            a__299_.y
            b__300_.y
        | n -> n)
    ;;

    let _ = compare_recursive1

    let equal_recursive1__local
      : 'a.
      (local_ 'a -> local_ 'a -> bool)
      -> local_ 'a recursive1
      -> local_ 'a recursive1
      -> bool
      =
      fun _cmp__a a__305_ b__306_ ->
      if Stdlib.( == ) a__305_ b__306_
      then true
      else
        Stdlib.( && )
          (equal_int__local a__305_.x b__306_.x)
          (equal_recursive1
             (fun a__307_ b__308_ ->
               A1.equal (fun a__309_ b__310_ -> _cmp__a a__309_ b__310_) a__307_ b__308_)
             a__305_.y
             b__306_.y)
    ;;

    let _ = equal_recursive1__local

    let equal_recursive1
      : 'a. ('a -> 'a -> bool) -> 'a recursive1 -> 'a recursive1 -> bool
      =
      fun _cmp__a a__311_ b__312_ ->
      if Stdlib.( == ) a__311_ b__312_
      then true
      else
        Stdlib.( && )
          (equal_int a__311_.x b__312_.x)
          (equal_recursive1
             (fun a__313_ b__314_ ->
               A1.equal (fun a__315_ b__316_ -> _cmp__a a__315_ b__316_) a__313_ b__314_)
             a__311_.y
             b__312_.y)
    ;;

    let _ = equal_recursive1

    [@@@end]
  end
end
