let ignore = `Should_refer_to_pervasives_explicitely
let ( = ) = `Should_refer_to_pervasives_explicitely
let ( <> ) = `Should_refer_to_pervasives_explicitely
let ( == ) = `Should_refer_to_pervasives_explicitely
let ( != ) = `Should_refer_to_pervasives_explicitely
let ( > ) = `Should_refer_to_pervasives_explicitely
let ( < ) = `Should_refer_to_pervasives_explicitely
let ( >= ) = `Should_refer_to_pervasives_explicitely
let ( <= ) = `Should_refer_to_pervasives_explicitely
let ( max ) = `Should_refer_to_pervasives_explicitely
let ( min ) = `Should_refer_to_pervasives_explicitely
let ( equal ) = `Should_refer_to_pervasives_explicitely
let ( compare ) = `Should_refer_to_pervasives_explicitely

module M1 = struct type t = unit [@@deriving compare] end

module M2 = struct type t = int [@@deriving compare] end

module M3 = struct type t = bool [@@deriving compare] end

module M4 = struct type t = int32 [@@deriving compare] end

module M5 = struct type t = nativeint [@@deriving compare] end

module M6 = struct type t = int64 [@@deriving compare] end

module M7 = struct type t = float [@@deriving compare] end

module M8 = struct type t = bool * float [@@deriving compare] end

module M9 = struct type t = bool * float * int [@@deriving compare] end

module M10 = struct type t = bool * float * int * string [@@deriving compare]  end

module M11 = struct type t = int ref [@@deriving compare] end

module M12 = struct type t = (float * float) option [@@deriving compare] end

module M13 = struct type t = float array [@@deriving compare] end

module M14 = struct type t = (int * int) array [@@deriving compare] end

module M15 = struct type t = float array array [@@deriving compare] end

module M16 = struct type t = int list [@@deriving compare] end

module M17 = struct type t = {
  s : string;
  b : float array list;
  mutable c : (int * int64 option);
} [@@deriving compare]
end

module M18 = struct type t = {
  a : float;
  b : float;
  c : float;
} [@@deriving compare]
end

module M19 = struct type t = Foo [@@deriving compare] end

module M20 = struct type t = Foo of int [@@deriving compare] end

module M21 = struct type t = Foo of int * float [@@deriving compare]    end

module M22 = struct type t = Foo | Bar of int | Baz of string option [@@deriving compare] end

module M23 = struct type t = [`Foo | `Bar of string * string] [@@deriving compare] end

module M24 = struct type t = int * string * [`Foo | `Bar ] [@@deriving compare] end

module M25 = struct type t = String.t [@@deriving compare] end

module M26 = struct type 'a t = 'a array [@@deriving compare] end

module MyList = struct type 'a t = Nil | Node of 'a * 'a t [@@deriving compare] end

module M27 = struct
  type t = int [@@deriving compare]
  module Inner = struct
    type nonrec t = t list [@@deriving compare]
    let _ = ((compare : int list -> int list -> int) : t -> t -> int)
  end
end

module M28 = struct
  (* making sure that nobody is reversing the type parameters *)
  type ('a, 'b) t = ('a * 'b) list [@@deriving compare]
  let _ = [%compare: (int,float) t] [(1,nan)]
end

module Polyrec = struct
  type ('a, 'b) t = T of ('a option, 'b) t [@@deriving compare]

  type ('a, 'b) t1 = T of ('a option, 'b) t2
  and ('a, 'b) t2 = T1 of ('a list, 'b) t1 | T2 of ('a, 'b list) t2
  [@@deriving compare]
end

module type Variance_sig = sig
  type +'a t [@@deriving compare]
end

module Variance = struct
  type -'a t [@@deriving compare]
  type (-'a, +'b) u = 'a t * 'b [@@deriving compare]
end

module Test = struct
  let (=) : int -> int -> bool = Pervasives.(=)
  (* checking that for the types mentioned in the readme, we compare structurally  *)
  let%test _ = [%compare: unit option] None (Some ()) = Pervasives.compare None (Some ())
  let%test _ = [%compare: unit list] [] [()] = Pervasives.compare [] [()]
  let%test _ = [%compare: int array] [|0; 1|] [|1|] = Pervasives.compare [|0; 1|] [|1|]
  let%test _ =
    Pervasives.(=)
      (List.sort [%compare: int option] [Some 3; None; Some 2; Some 1])
      [None; Some 1; Some 2; Some 3]
end

module Variant_inclusion = struct
  type 'a type1 = [ `T1 of 'a ] [@@deriving compare]
  type 'a type2 = [ 'a type1 | `T2 ] [@@deriving compare]
  type 'a type3 = [ `T3 | 'a type1 ] [@@deriving compare]
  type 'a type4 = [ 'a type2 | `T4 | 'a type3 ] [@@deriving compare]
end

module Equal = struct
  let%test _ = [%compare.equal: int list] [7; 8; 9] [7; 8; 9]
  let%test _ = not ([%compare.equal: int list] [7; 8] [7; 8; 9])

  let%test _ = [%compare.equal: string option] None None
  let%test _ = not ([%compare.equal: string option] (Some "foo") None)

  let%test _ = [%compare.equal: string] "hello" "hello"
  let%test _ = not ([%compare.equal: string] "hello" "goodbye")
end
