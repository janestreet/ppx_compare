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

(* Tests with [[@globalized]] attribute *)
module Via_public_release_attribute = struct
  (* The purpose of the [[@globalized]] attribute is to be used in public release, where
     modes are not checked. We simulate this by using a type [t] that crosses locality for
     the fields that are marked [[@globalized]] *)
  module A : sig
    type t : value mod global

    val compare : t -> t -> int
    val equal : t -> t -> bool
  end = struct
    type t = int [@@deriving compare, equal]
  end

  module A1 : sig
    type 'a t : value mod global

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end = struct
    type 'a t = int [@@deriving compare, equal]
  end

  type simple_rec =
    { x : int
    ; y : A.t [@globalized]
    }
  [@@deriving compare ~localize, equal ~localize]

  type simple_var =
    | A of int
    | B of (A.t[@globalized])
  [@@deriving compare ~localize, equal ~localize]

  type simple_rec1 =
    { x : int
    ; y : string A1.t [@globalized]
    }
  [@@deriving compare ~localize, equal ~localize]

  type simple_var1 =
    | A of int
    | B of (string A1.t[@globalized])
  [@@deriving compare ~localize, equal ~localize]

  type 'a might_need_eta_rec =
    { x : 'a
    ; y : 'a A1.t [@globalized]
    }
  [@@deriving compare ~localize, equal ~localize]

  type 'a might_need_eta_var =
    | A of 'a
    | B of ('a A1.t[@globalized])
  [@@deriving compare ~localize, equal ~localize]

  type 'a mut =
    { x : int
    ; mutable y : A.t
    }
  [@@deriving compare ~localize, equal ~localize]

  type recursive =
    { x : int
    ; y : recursive A1.t [@globalized]
    }
  [@@deriving compare ~localize, equal ~localize]
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
    (fun a__277_ b__278_ ->
       if Stdlib.( == ) a__277_ b__278_
       then 0
       else (
         match compare_int__local a__277_.x b__278_.x with
         | 0 -> A.compare a__277_.y b__278_.y
         | n -> n)
     : local_ simple_rec -> local_ simple_rec -> int)
  ;;

  let _ = compare_simple_rec__local

  let compare_simple_rec =
    (fun a b -> compare_simple_rec__local a b : simple_rec -> simple_rec -> int)
  ;;

  let _ = compare_simple_rec

  let equal_simple_rec__local =
    (fun a__279_ b__280_ ->
       if Stdlib.( == ) a__279_ b__280_
       then true
       else
         Stdlib.( && )
           (equal_int__local a__279_.x b__280_.x)
           (A.equal a__279_.y b__280_.y)
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
    (fun a__281_ b__282_ ->
       if Stdlib.( == ) a__281_ b__282_
       then 0
       else (
         match a__281_, b__282_ with
         | A _a__283_, A _b__284_ -> compare_int__local _a__283_ _b__284_
         | A _, _ -> -1
         | _, A _ -> 1
         | B _a__285_, B _b__286_ -> A.compare _a__285_ _b__286_)
     : local_ simple_var -> local_ simple_var -> int)
  ;;

  let _ = compare_simple_var__local

  let compare_simple_var =
    (fun a b -> compare_simple_var__local a b : simple_var -> simple_var -> int)
  ;;

  let _ = compare_simple_var

  let equal_simple_var__local =
    (fun a__287_ b__288_ ->
       if Stdlib.( == ) a__287_ b__288_
       then true
       else (
         match a__287_, b__288_ with
         | A _a__289_, A _b__290_ -> equal_int__local _a__289_ _b__290_
         | A _, _ -> false
         | _, A _ -> false
         | B _a__291_, B _b__292_ -> A.equal _a__291_ _b__292_)
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
    (fun a__293_ b__294_ ->
       if Stdlib.( == ) a__293_ b__294_
       then 0
       else (
         match compare_int__local a__293_.x b__294_.x with
         | 0 ->
           A1.compare
             (fun a__295_ b__296_ -> compare_string a__295_ b__296_)
             a__293_.y
             b__294_.y
         | n -> n)
     : local_ simple_rec1 -> local_ simple_rec1 -> int)
  ;;

  let _ = compare_simple_rec1__local

  let compare_simple_rec1 =
    (fun a b -> compare_simple_rec1__local a b : simple_rec1 -> simple_rec1 -> int)
  ;;

  let _ = compare_simple_rec1

  let equal_simple_rec1__local =
    (fun a__297_ b__298_ ->
       if Stdlib.( == ) a__297_ b__298_
       then true
       else
         Stdlib.( && )
           (equal_int__local a__297_.x b__298_.x)
           (A1.equal
              (fun a__299_ b__300_ -> equal_string a__299_ b__300_)
              a__297_.y
              b__298_.y)
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
    (fun a__301_ b__302_ ->
       if Stdlib.( == ) a__301_ b__302_
       then 0
       else (
         match a__301_, b__302_ with
         | A _a__303_, A _b__304_ -> compare_int__local _a__303_ _b__304_
         | A _, _ -> -1
         | _, A _ -> 1
         | B _a__305_, B _b__306_ ->
           A1.compare
             (fun a__307_ b__308_ -> compare_string a__307_ b__308_)
             _a__305_
             _b__306_)
     : local_ simple_var1 -> local_ simple_var1 -> int)
  ;;

  let _ = compare_simple_var1__local

  let compare_simple_var1 =
    (fun a b -> compare_simple_var1__local a b : simple_var1 -> simple_var1 -> int)
  ;;

  let _ = compare_simple_var1

  let equal_simple_var1__local =
    (fun a__309_ b__310_ ->
       if Stdlib.( == ) a__309_ b__310_
       then true
       else (
         match a__309_, b__310_ with
         | A _a__311_, A _b__312_ -> equal_int__local _a__311_ _b__312_
         | A _, _ -> false
         | _, A _ -> false
         | B _a__313_, B _b__314_ ->
           A1.equal
             (fun a__315_ b__316_ -> equal_string a__315_ b__316_)
             _a__313_
             _b__314_)
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
    fun _cmp__a a__317_ b__318_ ->
    if Stdlib.( == ) a__317_ b__318_
    then 0
    else (
      match _cmp__a a__317_.x b__318_.x with
      | 0 ->
        A1.compare (fun a__319_ b__320_ -> _cmp__a a__319_ b__320_) a__317_.y b__318_.y
      | n -> n)
  ;;

  let _ = compare_might_need_eta_rec__local

  let compare_might_need_eta_rec
    : 'a. ('a -> 'a -> int) -> 'a might_need_eta_rec -> 'a might_need_eta_rec -> int
    =
    fun _cmp__a a__321_ b__322_ ->
    if Stdlib.( == ) a__321_ b__322_
    then 0
    else (
      match _cmp__a a__321_.x b__322_.x with
      | 0 ->
        A1.compare (fun a__323_ b__324_ -> _cmp__a a__323_ b__324_) a__321_.y b__322_.y
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
    fun _cmp__a a__325_ b__326_ ->
    if Stdlib.( == ) a__325_ b__326_
    then true
    else
      Stdlib.( && )
        (_cmp__a a__325_.x b__326_.x)
        (A1.equal (fun a__327_ b__328_ -> _cmp__a a__327_ b__328_) a__325_.y b__326_.y)
  ;;

  let _ = equal_might_need_eta_rec__local

  let equal_might_need_eta_rec
    : 'a. ('a -> 'a -> bool) -> 'a might_need_eta_rec -> 'a might_need_eta_rec -> bool
    =
    fun _cmp__a a__329_ b__330_ ->
    if Stdlib.( == ) a__329_ b__330_
    then true
    else
      Stdlib.( && )
        (_cmp__a a__329_.x b__330_.x)
        (A1.equal (fun a__331_ b__332_ -> _cmp__a a__331_ b__332_) a__329_.y b__330_.y)
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
    fun _cmp__a a__333_ b__334_ ->
    if Stdlib.( == ) a__333_ b__334_
    then 0
    else (
      match a__333_, b__334_ with
      | A _a__335_, A _b__336_ -> _cmp__a _a__335_ _b__336_
      | A _, _ -> -1
      | _, A _ -> 1
      | B _a__337_, B _b__338_ ->
        A1.compare (fun a__339_ b__340_ -> _cmp__a a__339_ b__340_) _a__337_ _b__338_)
  ;;

  let _ = compare_might_need_eta_var__local

  let compare_might_need_eta_var
    : 'a. ('a -> 'a -> int) -> 'a might_need_eta_var -> 'a might_need_eta_var -> int
    =
    fun _cmp__a a__341_ b__342_ ->
    if Stdlib.( == ) a__341_ b__342_
    then 0
    else (
      match a__341_, b__342_ with
      | A _a__343_, A _b__344_ -> _cmp__a _a__343_ _b__344_
      | A _, _ -> -1
      | _, A _ -> 1
      | B _a__345_, B _b__346_ ->
        A1.compare (fun a__347_ b__348_ -> _cmp__a a__347_ b__348_) _a__345_ _b__346_)
  ;;

  let _ = compare_might_need_eta_var

  let equal_might_need_eta_var__local
    : 'a.
    (local_ 'a -> local_ 'a -> bool)
    -> local_ 'a might_need_eta_var
    -> local_ 'a might_need_eta_var
    -> bool
    =
    fun _cmp__a a__349_ b__350_ ->
    if Stdlib.( == ) a__349_ b__350_
    then true
    else (
      match a__349_, b__350_ with
      | A _a__351_, A _b__352_ -> _cmp__a _a__351_ _b__352_
      | A _, _ -> false
      | _, A _ -> false
      | B _a__353_, B _b__354_ ->
        A1.equal (fun a__355_ b__356_ -> _cmp__a a__355_ b__356_) _a__353_ _b__354_)
  ;;

  let _ = equal_might_need_eta_var__local

  let equal_might_need_eta_var
    : 'a. ('a -> 'a -> bool) -> 'a might_need_eta_var -> 'a might_need_eta_var -> bool
    =
    fun _cmp__a a__357_ b__358_ ->
    if Stdlib.( == ) a__357_ b__358_
    then true
    else (
      match a__357_, b__358_ with
      | A _a__359_, A _b__360_ -> _cmp__a _a__359_ _b__360_
      | A _, _ -> false
      | _, A _ -> false
      | B _a__361_, B _b__362_ ->
        A1.equal (fun a__363_ b__364_ -> _cmp__a a__363_ b__364_) _a__361_ _b__362_)
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
    fun _cmp__a a__365_ b__366_ ->
    if Stdlib.( == ) a__365_ b__366_
    then 0
    else (
      match compare_int__local a__365_.x b__366_.x with
      | 0 -> A.compare a__365_.y b__366_.y
      | n -> n)
  ;;

  let _ = compare_mut__local

  let compare_mut : 'a. ('a -> 'a -> int) -> 'a mut -> 'a mut -> int =
    fun _cmp__a a__367_ b__368_ ->
    if Stdlib.( == ) a__367_ b__368_
    then 0
    else (
      match compare_int a__367_.x b__368_.x with
      | 0 -> A.compare a__367_.y b__368_.y
      | n -> n)
  ;;

  let _ = compare_mut

  let equal_mut__local
    : 'a. (local_ 'a -> local_ 'a -> bool) -> local_ 'a mut -> local_ 'a mut -> bool
    =
    fun _cmp__a a__369_ b__370_ ->
    if Stdlib.( == ) a__369_ b__370_
    then true
    else
      Stdlib.( && ) (equal_int__local a__369_.x b__370_.x) (A.equal a__369_.y b__370_.y)
  ;;

  let _ = equal_mut__local

  let equal_mut : 'a. ('a -> 'a -> bool) -> 'a mut -> 'a mut -> bool =
    fun _cmp__a a__371_ b__372_ ->
    if Stdlib.( == ) a__371_ b__372_
    then true
    else Stdlib.( && ) (equal_int a__371_.x b__372_.x) (A.equal a__371_.y b__372_.y)
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
    (fun a__373_ b__374_ ->
       if Stdlib.( == ) a__373_ b__374_
       then 0
       else (
         match compare_int__local a__373_.x b__374_.x with
         | 0 ->
           A1.compare
             (fun a__375_ b__376_ -> compare_recursive a__375_ b__376_)
             a__373_.y
             b__374_.y
         | n -> n)
     : local_ recursive -> local_ recursive -> int)

  and compare_recursive =
    (fun a b -> compare_recursive__local a b : recursive -> recursive -> int)
  ;;

  let _ = compare_recursive__local
  and _ = compare_recursive

  let rec equal_recursive__local =
    (fun a__377_ b__378_ ->
       if Stdlib.( == ) a__377_ b__378_
       then true
       else
         Stdlib.( && )
           (equal_int__local a__377_.x b__378_.x)
           (A1.equal
              (fun a__379_ b__380_ -> equal_recursive a__379_ b__380_)
              a__377_.y
              b__378_.y)
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
    fun _cmp__a a__381_ b__382_ ->
    if Stdlib.( == ) a__381_ b__382_
    then 0
    else (
      match compare_int__local a__381_.x b__382_.x with
      | 0 ->
        compare_recursive1
          (fun a__383_ b__384_ ->
            A1.compare (fun a__385_ b__386_ -> _cmp__a a__385_ b__386_) a__383_ b__384_)
          a__381_.y
          b__382_.y
      | n -> n)

  and compare_recursive1 : 'a. ('a -> 'a -> int) -> 'a recursive1 -> 'a recursive1 -> int =
    fun _cmp__a a__387_ b__388_ ->
    if Stdlib.( == ) a__387_ b__388_
    then 0
    else (
      match compare_int a__387_.x b__388_.x with
      | 0 ->
        compare_recursive1
          (fun a__389_ b__390_ ->
            A1.compare (fun a__391_ b__392_ -> _cmp__a a__391_ b__392_) a__389_ b__390_)
          a__387_.y
          b__388_.y
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
    fun _cmp__a a__393_ b__394_ ->
    if Stdlib.( == ) a__393_ b__394_
    then true
    else
      Stdlib.( && )
        (equal_int__local a__393_.x b__394_.x)
        (equal_recursive1
           (fun a__395_ b__396_ ->
             A1.equal (fun a__397_ b__398_ -> _cmp__a a__397_ b__398_) a__395_ b__396_)
           a__393_.y
           b__394_.y)

  and equal_recursive1 : 'a. ('a -> 'a -> bool) -> 'a recursive1 -> 'a recursive1 -> bool =
    fun _cmp__a a__399_ b__400_ ->
    if Stdlib.( == ) a__399_ b__400_
    then true
    else
      Stdlib.( && )
        (equal_int a__399_.x b__400_.x)
        (equal_recursive1
           (fun a__401_ b__402_ ->
             A1.equal (fun a__403_ b__404_ -> _cmp__a a__403_ b__404_) a__401_ b__402_)
           a__399_.y
           b__400_.y)
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
      fun _cmp__a a__405_ b__406_ ->
      if Stdlib.( == ) a__405_ b__406_
      then 0
      else (
        match compare_int__local a__405_.x b__406_.x with
        | 0 ->
          compare_recursive1
            (fun a__407_ b__408_ ->
              A1.compare (fun a__409_ b__410_ -> _cmp__a a__409_ b__410_) a__407_ b__408_)
            a__405_.y
            b__406_.y
        | n -> n)
    ;;

    let _ = compare_recursive1__local

    let compare_recursive1
      : 'a. ('a -> 'a -> int) -> 'a recursive1 -> 'a recursive1 -> int
      =
      fun _cmp__a a__411_ b__412_ ->
      if Stdlib.( == ) a__411_ b__412_
      then 0
      else (
        match compare_int a__411_.x b__412_.x with
        | 0 ->
          compare_recursive1
            (fun a__413_ b__414_ ->
              A1.compare (fun a__415_ b__416_ -> _cmp__a a__415_ b__416_) a__413_ b__414_)
            a__411_.y
            b__412_.y
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
      fun _cmp__a a__417_ b__418_ ->
      if Stdlib.( == ) a__417_ b__418_
      then true
      else
        Stdlib.( && )
          (equal_int__local a__417_.x b__418_.x)
          (equal_recursive1
             (fun a__419_ b__420_ ->
               A1.equal (fun a__421_ b__422_ -> _cmp__a a__421_ b__422_) a__419_ b__420_)
             a__417_.y
             b__418_.y)
    ;;

    let _ = equal_recursive1__local

    let equal_recursive1
      : 'a. ('a -> 'a -> bool) -> 'a recursive1 -> 'a recursive1 -> bool
      =
      fun _cmp__a a__423_ b__424_ ->
      if Stdlib.( == ) a__423_ b__424_
      then true
      else
        Stdlib.( && )
          (equal_int a__423_.x b__424_.x)
          (equal_recursive1
             (fun a__425_ b__426_ ->
               A1.equal (fun a__427_ b__428_ -> _cmp__a a__427_ b__428_) a__425_ b__426_)
             a__423_.y
             b__424_.y)
    ;;

    let _ = equal_recursive1

    [@@@end]
  end
end
