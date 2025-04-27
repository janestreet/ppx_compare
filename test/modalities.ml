let compare_int : int -> int -> int = compare
let compare_int__local : int @ local -> int @ local -> int = compare
let equal_int : int -> int -> bool = ( = )
let equal_int__local : int @ local -> int @ local -> bool = ( = )

module Compare : sig
  type t [@@deriving_inline compare ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline compare ~portable]

  let _ = fun (_ : t) -> ()
  let compare @ portable = (compare_int : t -> t -> int)
  let _ = compare

  [@@@end]
end

module Compare_u : sig
  type u [@@deriving_inline compare ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val compare_u : u -> u -> int @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type u = int [@@deriving_inline compare ~portable]

  let _ = fun (_ : u) -> ()
  let compare_u @ portable = (compare_int : u -> u -> int)
  let _ = compare_u

  [@@@end]
end

module Compare_local : sig
  type t [@@deriving_inline compare ~localize ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S with type t := t @@ portable
    include Ppx_compare_lib.Comparable.S__local with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline compare ~localize ~portable]

  let _ = fun (_ : t) -> ()
  let compare__local @ portable = (compare_int__local : local_ t -> local_ t -> int)
  let _ = compare__local
  let compare = (fun a b -> compare__local a b : t -> t -> int)
  let _ = compare

  [@@@end]
end

module%template [@mode local] Compare : sig
  type t [@@deriving_inline (compare [@mode local]) ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S with type t := t @@ portable
    include Ppx_compare_lib.Comparable.S__local with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t =
    { a : int
    ; b : int
    }
  [@@deriving_inline (compare [@mode local]) ~portable]

  let _ = fun (_ : t) -> ()

  let compare__local @ portable =
    (fun a__007_ b__008_ ->
       if Stdlib.( == ) a__007_ b__008_
       then 0
       else (
         match compare_int__local a__007_.a b__008_.a with
         | 0 -> compare_int__local a__007_.b b__008_.b
         | n -> n)
     : local_ t -> local_ t -> int)
  ;;

  let _ = compare__local
  let compare = (fun a b -> compare__local a b : t -> t -> int)
  let _ = compare

  [@@@end]
end

module Equal : sig
  type t [@@deriving_inline equal ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Equal.S with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline equal ~portable]

  let _ = fun (_ : t) -> ()
  let equal @ portable = (equal_int : t -> t -> bool)
  let _ = equal

  [@@@end]
end

module Equal_u : sig
  type u [@@deriving_inline equal ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val equal_u : u -> u -> bool @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type u = int [@@deriving_inline equal ~portable]

  let _ = fun (_ : u) -> ()
  let equal_u @ portable = (equal_int : u -> u -> bool)
  let _ = equal_u

  [@@@end]
end

module Equal_local : sig
  type t [@@deriving_inline equal ~localize ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Equal.S with type t := t @@ portable
    include Ppx_compare_lib.Equal.S__local with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline equal ~localize ~portable]

  let _ = fun (_ : t) -> ()
  let equal__local @ portable = (equal_int__local : local_ t -> local_ t -> bool)
  let _ = equal__local
  let equal = (fun a b -> equal__local a b : t -> t -> bool)
  let _ = equal

  [@@@end]
end

module%template [@mode local] Equal : sig
  type t [@@deriving_inline (equal [@mode local]) ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Equal.S with type t := t @@ portable
    include Ppx_compare_lib.Equal.S__local with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline equal ~localize ~portable]

  let _ = fun (_ : t) -> ()
  let equal__local @ portable = (equal_int__local : local_ t -> local_ t -> bool)
  let _ = equal__local
  let equal = (fun a b -> equal__local a b : t -> t -> bool)
  let _ = equal

  [@@@end]
end

module Recursive : sig
  type t
  and 'a u
  and ('a, 'b) v [@@deriving_inline compare ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val compare : t -> t -> int @@ portable
    val compare_u : ('a -> 'a -> int) -> 'a u -> 'a u -> int @@ portable

    val compare_v
      :  ('a -> 'a -> int)
      -> ('b -> 'b -> int)
      -> ('a, 'b) v
      -> ('a, 'b) v
      -> int
      @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t =
    { a : int
    ; u : int u
    }

  and 'a u =
    { b : 'a
    ; v : (int, 'a) v
    }

  and ('a, 'b) v =
    { a : 'a
    ; b : 'b
    ; t : t
    }
  [@@deriving_inline compare ~portable]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : 'a u) -> ()
  let _ = fun (_ : ('a, 'b) v) -> ()

  let rec compare @ portable =
    (fun a__017_ b__018_ ->
       if Stdlib.( == ) a__017_ b__018_
       then 0
       else (
         match compare_int a__017_.a b__018_.a with
         | 0 -> compare_u compare_int a__017_.u b__018_.u
         | n -> n)
     : t -> t -> int)

  and compare_u : 'a. ('a -> 'a -> int) -> 'a u -> 'a u -> int @@ portable =
    fun _cmp__a a__021_ b__022_ ->
    if Stdlib.( == ) a__021_ b__022_
    then 0
    else (
      match _cmp__a a__021_.b b__022_.b with
      | 0 -> compare_v compare_int _cmp__a a__021_.v b__022_.v
      | n -> n)

  and compare_v
    : 'a 'b. ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) v -> ('a, 'b) v -> int
    @@ portable
    =
    fun _cmp__a _cmp__b a__027_ b__028_ ->
    if Stdlib.( == ) a__027_ b__028_
    then 0
    else (
      match _cmp__a a__027_.a b__028_.a with
      | 0 ->
        (match _cmp__b a__027_.b b__028_.b with
         | 0 -> compare a__027_.t b__028_.t
         | n -> n)
      | n -> n)
  ;;

  let _ = compare
  and _ = compare_u
  and _ = compare_v

  [@@@end]
end
