(** Runtime support for auto-generated comparators. Users are not intended to use this
    module directly. *)

module Definitions = struct
  type 'a compare = 'a -> 'a -> int
  type 'a compare__local = 'a -> 'a -> int
  type 'a equal = 'a -> 'a -> bool
  type 'a equal__local = 'a -> 'a -> bool

  module Comparable = struct
    module type S = sig
      type t

      val compare : t compare
    end

    module type S1 = sig
      type 'a t

      val compare : 'a compare -> 'a t compare
    end

    module type S2 = sig
      type ('a, 'b) t

      val compare : 'a compare -> 'b compare -> ('a, 'b) t compare
    end

    module type S3 = sig
      type ('a, 'b, 'c) t

      val compare : 'a compare -> 'b compare -> 'c compare -> ('a, 'b, 'c) t compare
    end

    module type S__local = sig
      type t

      val compare__local : t compare__local
    end

    module type S1__local = sig
      type 'a t

      val compare__local : 'a compare__local -> 'a t compare__local
    end

    module type S2__local = sig
      type ('a, 'b) t

      val compare__local
        :  'a compare__local
        -> 'b compare__local
        -> ('a, 'b) t compare__local
    end

    module type S3__local = sig
      type ('a, 'b, 'c) t

      val compare__local
        :  'a compare__local
        -> 'b compare__local
        -> 'c compare__local
        -> ('a, 'b, 'c) t compare__local
    end
  end

  module Equal = struct
    module type S = sig
      type t

      val equal : t equal
    end

    module type S1 = sig
      type 'a t

      val equal : 'a equal -> 'a t equal
    end

    module type S2 = sig
      type ('a, 'b) t

      val equal : 'a equal -> 'b equal -> ('a, 'b) t equal
    end

    module type S3 = sig
      type ('a, 'b, 'c) t

      val equal : 'a equal -> 'b equal -> 'c equal -> ('a, 'b, 'c) t equal
    end

    module type S__local = sig
      type t

      val equal__local : t equal__local
    end

    module type S1__local = sig
      type 'a t

      val equal__local : 'a equal__local -> 'a t equal__local
    end

    module type S2__local = sig
      type ('a, 'b) t

      val equal__local : 'a equal__local -> 'b equal__local -> ('a, 'b) t equal__local
    end

    module type S3__local = sig
      type ('a, 'b, 'c) t

      val equal__local
        :  'a equal__local
        -> 'b equal__local
        -> 'c equal__local
        -> ('a, 'b, 'c) t equal__local
    end
  end
end

module type Ppx_compare_lib = sig
  include module type of struct
    include Definitions
  end

  (** Raise when fully applied *)
  val compare_abstract : type_name:string -> _ compare__local

  val equal_abstract : type_name:string -> _ equal__local

  module Builtin : sig
    val compare_bool : bool compare [@@zero_alloc arity 2]
    val compare_char : char compare [@@zero_alloc arity 2]
    val compare_float : float compare [@@zero_alloc arity 2]
    val compare_int : int compare [@@zero_alloc arity 2]
    val compare_int32 : int32 compare [@@zero_alloc arity 2]
    val compare_int64 : int64 compare [@@zero_alloc arity 2]
    val compare_nativeint : nativeint compare [@@zero_alloc arity 2]
    val compare_string : string compare [@@zero_alloc arity 2]
    val compare_bytes : bytes compare [@@zero_alloc arity 2]
    val compare_unit : unit compare [@@zero_alloc arity 2]
    val compare_array : 'a compare -> 'a array compare
    val compare_list : 'a compare -> 'a list compare
    val compare_option : 'a compare -> 'a option compare
    val compare_ref : 'a compare -> 'a ref compare
    val equal_bool : bool equal [@@zero_alloc arity 2]
    val equal_char : char equal [@@zero_alloc arity 2]
    val equal_float : float equal [@@zero_alloc arity 2]
    val equal_int : int equal [@@zero_alloc arity 2]
    val equal_int32 : int32 equal [@@zero_alloc arity 2]
    val equal_int64 : int64 equal [@@zero_alloc arity 2]
    val equal_nativeint : nativeint equal [@@zero_alloc arity 2]
    val equal_string : string equal [@@zero_alloc arity 2]
    val equal_bytes : bytes equal [@@zero_alloc arity 2]
    val equal_unit : unit equal [@@zero_alloc arity 2]
    val equal_array : 'a equal -> 'a array equal
    val equal_list : 'a equal -> 'a list equal
    val equal_option : 'a equal -> 'a option equal
    val equal_ref : 'a equal -> 'a ref equal
    val compare_bool__local : bool compare__local [@@zero_alloc arity 2]
    val compare_char__local : char compare__local [@@zero_alloc arity 2]
    val compare_float__local : float compare__local [@@zero_alloc arity 2]
    val compare_int__local : int compare__local [@@zero_alloc arity 2]
    val compare_int32__local : int32 compare__local [@@zero_alloc arity 2]
    val compare_int64__local : int64 compare__local [@@zero_alloc arity 2]
    val compare_nativeint__local : nativeint compare__local [@@zero_alloc arity 2]
    val compare_string__local : string compare__local [@@zero_alloc arity 2]
    val compare_bytes__local : bytes compare__local [@@zero_alloc arity 2]
    val compare_unit__local : unit compare__local [@@zero_alloc arity 2]
    val compare_array__local : 'a compare__local -> 'a array compare__local
    val compare_list__local : 'a compare__local -> 'a list compare__local
    val compare_option__local : 'a compare__local -> 'a option compare__local
    val compare_ref__local : 'a compare__local -> 'a ref compare__local
    val equal_bool__local : bool equal__local [@@zero_alloc arity 2]
    val equal_char__local : char equal__local [@@zero_alloc arity 2]
    val equal_float__local : float equal__local [@@zero_alloc arity 2]
    val equal_int__local : int equal__local [@@zero_alloc arity 2]
    val equal_int32__local : int32 equal__local [@@zero_alloc arity 2]
    val equal_int64__local : int64 equal__local [@@zero_alloc arity 2]
    val equal_nativeint__local : nativeint equal__local [@@zero_alloc arity 2]
    val equal_string__local : string equal__local [@@zero_alloc arity 2]
    val equal_bytes__local : bytes equal__local [@@zero_alloc arity 2]
    val equal_unit__local : unit equal__local [@@zero_alloc arity 2]
    val equal_array__local : 'a equal__local -> 'a array equal__local
    val equal_list__local : 'a equal__local -> 'a list equal__local
    val equal_option__local : 'a equal__local -> 'a option equal__local
    val equal_ref__local : 'a equal__local -> 'a ref equal__local
  end
end
