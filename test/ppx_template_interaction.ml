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
  let compare__local = (compare_int__local : local_ t -> local_ t -> int)
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
  let equal__local = (equal_int__local : local_ t -> local_ t -> bool)
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
