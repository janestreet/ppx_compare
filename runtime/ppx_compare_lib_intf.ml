(** Runtime support for auto-generated comparators. Users are not intended to use this
    module directly. *)

module Definitions = struct
  [%%template
  [@@@mode.default l = (global, local)]

  type 'a compare = 'a -> 'a -> int
  type 'a equal = 'a -> 'a -> bool]

  module Comparable = struct
    [%%template
    [@@@mode.default l = (global, local)]

    module type S = sig
      type t

      val compare : (t compare[@mode l]) [@@mode l]
    end

    module type S1 = sig
      type 'a t

      val compare : ('a compare[@mode l]) -> ('a t compare[@mode l]) [@@mode l]
    end

    module type S2 = sig
      type ('a, 'b) t

      val compare
        :  ('a compare[@mode l])
        -> ('b compare[@mode l])
        -> (('a, 'b) t compare[@mode l])
      [@@mode l]
    end

    module type S3 = sig
      type ('a, 'b, 'c) t

      val compare
        :  ('a compare[@mode l])
        -> ('b compare[@mode l])
        -> ('c compare[@mode l])
        -> (('a, 'b, 'c) t compare[@mode l])
      [@@mode l]
    end]
  end

  module Equal = struct
    [%%template
    [@@@mode.default l = (global, local)]

    module type S = sig
      type t

      val equal : (t equal[@mode l]) [@@mode l]
    end

    module type S1 = sig
      type 'a t

      val equal : ('a equal[@mode l]) -> ('a t equal[@mode l]) [@@mode l]
    end

    module type S2 = sig
      type ('a, 'b) t

      val equal
        :  ('a equal[@mode l])
        -> ('b equal[@mode l])
        -> (('a, 'b) t equal[@mode l])
      [@@mode l]
    end

    module type S3 = sig
      type ('a, 'b, 'c) t

      val equal
        :  ('a equal[@mode l])
        -> ('b equal[@mode l])
        -> ('c equal[@mode l])
        -> (('a, 'b, 'c) t equal[@mode l])
      [@@mode l]
    end]
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
    [%%template:
    [@@@mode.default l = (global, local)]

    val compare_bool : (bool compare[@mode l]) [@@zero_alloc arity 2]
    val compare_char : (char compare[@mode l]) [@@zero_alloc arity 2]
    val compare_float : (float compare[@mode l]) [@@zero_alloc arity 2]
    val compare_int : (int compare[@mode l]) [@@zero_alloc arity 2]
    val compare_int32 : (int32 compare[@mode l]) [@@zero_alloc arity 2]
    val compare_int64 : (int64 compare[@mode l]) [@@zero_alloc arity 2]
    val compare_nativeint : (nativeint compare[@mode l]) [@@zero_alloc arity 2]
    val compare_string : (string compare[@mode l]) [@@zero_alloc arity 2]
    val compare_bytes : (bytes compare[@mode l]) [@@zero_alloc arity 2]
    val compare_unit : (unit compare[@mode l]) [@@zero_alloc arity 2]

    val compare_array : 'a. ('a compare[@mode l]) -> ('a array compare[@mode l])
    [@@kind k = (float64, bits32, bits64, word, immediate, immediate64, value)]

    val compare_list : 'a. ('a compare[@mode l]) -> ('a list compare[@mode l])
    val compare_option : 'a. ('a compare[@mode l]) -> ('a option compare[@mode l])
    val compare_ref : 'a. ('a compare[@mode l]) -> ('a ref compare[@mode l])
    val equal_bool : (bool equal[@mode l]) [@@zero_alloc arity 2]
    val equal_char : (char equal[@mode l]) [@@zero_alloc arity 2]
    val equal_float : (float equal[@mode l]) [@@zero_alloc arity 2]
    val equal_int : (int equal[@mode l]) [@@zero_alloc arity 2]
    val equal_int32 : (int32 equal[@mode l]) [@@zero_alloc arity 2]
    val equal_int64 : (int64 equal[@mode l]) [@@zero_alloc arity 2]
    val equal_nativeint : (nativeint equal[@mode l]) [@@zero_alloc arity 2]
    val equal_string : (string equal[@mode l]) [@@zero_alloc arity 2]
    val equal_bytes : (bytes equal[@mode l]) [@@zero_alloc arity 2]
    val equal_unit : (unit equal[@mode l]) [@@zero_alloc arity 2]

    val equal_array : 'a. ('a equal[@mode l]) -> ('a array equal[@mode l])
    [@@kind k = (float64, bits32, bits64, word, immediate, immediate64, value)]

    val equal_list : 'a. ('a equal[@mode l]) -> ('a list equal[@mode l])
    val equal_option : 'a. ('a equal[@mode l]) -> ('a option equal[@mode l])
    val equal_ref : 'a. ('a equal[@mode l]) -> ('a ref equal[@mode l])]
  end
end
