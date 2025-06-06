open Ppx_compare_lib.Builtin

(* Demonstrate that [@@deriving compare [@mode local]] is equivalent to
   [@@deriving compare ~localize]. *)
module%template With_compare : sig
  type t [@@deriving_inline compare [@mode local]]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Comparable.S with type t := t
    include Ppx_compare_lib.Comparable.S__local with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline compare [@mode local]]

  let _ = fun (_ : t) -> ()
  let compare__local = (compare_int__local : t -> t -> int)
  let _ = compare__local
  let compare = (fun a b -> compare__local a b : t -> t -> int)
  let _ = compare

  [@@@end]
end

(* Check that the signatures match. *)
module _ : sig
  type t [@@deriving compare ~localize]
end =
  With_compare

module _ : module type of With_compare = struct
  type t = int [@@deriving compare ~localize]
end

(* Demonstrate that [@@deriving equal [@mode local]] is equivalent to
   [@@deriving equal ~localize]. *)
module%template With_equal : sig
  type t [@@deriving_inline equal [@mode local]]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_compare_lib.Equal.S with type t := t
    include Ppx_compare_lib.Equal.S__local with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline equal [@mode local]]

  let _ = fun (_ : t) -> ()
  let equal__local = (equal_int__local : t -> t -> bool)
  let _ = equal__local
  let equal = (fun a b -> equal__local a b : t -> t -> bool)
  let _ = equal

  [@@@end]
end

(* Check that the signatures match. *)
module _ : sig
  type t [@@deriving equal ~localize]
end =
  With_equal

module _ : module type of With_equal = struct
  type t = int [@@deriving equal ~localize]
end

(* Demonstrate that the name mangling used by ppx_compare and ppx_template makes it easy
   to define implementations of signatures generated for templated types. *)

module type%template S1_with_compare_and_equal = sig
  type 'a t = { boxed : 'a }
  [@@deriving compare ~localize, equal ~localize] [@@kind k = (value, bits64, float64)]
end

module%template S1_manual : S1_with_compare_and_equal = struct
  [@@@kind.default k = (value, bits64, float64)]

  type 'a t = { boxed : 'a }

  [@@@mode.default m = (global, local)]

  let compare f t1 t2 = f t1.boxed t2.boxed
  let equal f t1 t2 = f t1.boxed t2.boxed
end

module%template S1_via_extension : S1_with_compare_and_equal = struct
  [@@@kind.default k = (value, bits64, float64)]

  type 'a t = ('a S1_manual.t[@kind k]) = { boxed : 'a }

  let compare (type a) compare_a = [%compare: (a S1_manual.t[@kind k])]
  let equal (type a) equal_a = [%equal: (a S1_manual.t[@kind k])]

  [@@@mode.default m = local]

  let compare (type a) compare_a__local = [%compare_local: (a S1_manual.t[@kind k])]
  let equal (type a) equal_a__local = [%equal_local: (a S1_manual.t[@kind k])]
end

module%template S1_derived : S1_with_compare_and_equal = struct
  type 'a t = { boxed : 'a }
  [@@deriving compare ~localize, equal ~localize] [@@kind k = (value, bits64, float64)]
end

module type%template S2_with_compare_and_equal = sig
  type ('a, 'b) t =
    { boxed_a : 'a
    ; boxed_b : 'b
    }
  [@@deriving compare ~localize, equal ~localize]
  [@@kind ka = (value, bits64, float64), kb = (value, bits64, float64)]
end

module%template S2_manual : S2_with_compare_and_equal = struct
  [@@@kind.default ka = (value, bits64, float64), kb = (value, bits64, float64)]

  type ('a, 'b) t =
    { boxed_a : 'a
    ; boxed_b : 'b
    }

  [@@@mode.default m = (global, local)]

  let compare f_a f_b t1 t2 =
    match f_a t1.boxed_a t2.boxed_a with
    | 0 -> f_b t1.boxed_b t2.boxed_b
    | res -> res
  ;;

  let equal f_a f_b t1 t2 = f_a t1.boxed_a t2.boxed_a && f_b t1.boxed_b t2.boxed_b
end

module%template S2_via_extension : S2_with_compare_and_equal = struct
  [@@@kind.default ka = (value, bits64, float64), kb = (value, bits64, float64)]

  type ('a, 'b) t = (('a, 'b) S2_manual.t[@kind ka kb]) =
    { boxed_a : 'a
    ; boxed_b : 'b
    }

  let compare (type a b) compare_a compare_b =
    [%compare: ((a, b) S2_manual.t[@kind ka kb])]
  ;;

  let equal (type a b) equal_a equal_b = [%equal: ((a, b) S2_manual.t[@kind ka kb])]

  [@@@mode.default m = local]

  let compare (type a b) compare_a__local compare_b__local =
    [%compare_local: ((a, b) S2_manual.t[@kind ka kb])]
  ;;

  let equal (type a b) equal_a__local equal_b__local =
    [%equal_local: ((a, b) S2_manual.t[@kind ka kb])]
  ;;
end

module%template S2_derived : S2_with_compare_and_equal = struct
  type ('a, 'b) t =
    { boxed_a : 'a
    ; boxed_b : 'b
    }
  [@@deriving compare ~localize, equal ~localize]
  [@@kind ka = (value, bits64, float64), kb = (value, bits64, float64)]
end
