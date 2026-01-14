(** Runtime support for auto-generated comparators. Users are not intended to use this
    module directly. *)

[@@@warning "-incompatible-with-upstream"]

module Definitions = struct
  [%%template
  [@@@mode.default l = (global, local)]

  type ('a : any) compare = 'a @ l -> 'a @ l -> int
  type ('a : any) equal = 'a @ l -> 'a @ l -> bool]

  module Comparable = struct
    [%%template
    [@@@mode.default l = (global, local)]

    module type S = sig
      type t : any

      val compare : (t compare[@mode l]) [@@mode l]
    end

    module type S1 = sig
      type 'a t : any

      val compare : ('a compare[@mode l]) -> ('a t compare[@mode l]) [@@mode l]
    end

    module type S2 = sig
      type ('a, 'b) t : any

      val compare
        :  ('a compare[@mode l])
        -> ('b compare[@mode l])
        -> (('a, 'b) t compare[@mode l])
      [@@mode l]
    end

    module type S3 = sig
      type ('a, 'b, 'c) t : any

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
      type t : any

      val equal : (t equal[@mode l]) [@@mode l]
    end

    module type S1 = sig
      type 'a t : any

      val equal : ('a equal[@mode l]) -> ('a t equal[@mode l]) [@@mode l]
    end

    module type S2 = sig
      type ('a, 'b) t : any

      val equal
        :  ('a equal[@mode l])
        -> ('b equal[@mode l])
        -> (('a, 'b) t equal[@mode l])
      [@@mode l]
    end

    module type S3 = sig
      type ('a, 'b, 'c) t : any

      val equal
        :  ('a equal[@mode l])
        -> ('b equal[@mode l])
        -> ('c equal[@mode l])
        -> (('a, 'b, 'c) t equal[@mode l])
      [@@mode l]
    end]
  end
end

module type Ppx_compare_lib = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** Raise when fully applied *)
  val compare_abstract : type_name:string -> _ compare__local

  val equal_abstract : type_name:string -> _ equal__local

  module Builtin : sig @@ portable
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

    val compare_array
      : ('a : k mod separable).
      ('a compare[@mode l]) -> ('a array compare[@mode l])
    [@@kind k = base_or_null]

    val compare_list
      : ('a : value_or_null).
      ('a compare[@mode l]) -> ('a list compare[@mode l])

    val compare_option
      : ('a : value_or_null).
      ('a compare[@mode l]) -> ('a option compare[@mode l])

    val compare_ref
      : ('a : value_or_null).
      ('a compare[@mode l]) -> ('a ref compare[@mode l])

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

    val equal_array
      : ('a : k mod separable).
      ('a equal[@mode l]) -> ('a array equal[@mode l])
    [@@kind k = base_or_null]

    val equal_list : ('a : value_or_null). ('a equal[@mode l]) -> ('a list equal[@mode l])

    val equal_option
      : ('a : value_or_null).
      ('a equal[@mode l]) -> ('a option equal[@mode l])

    val equal_ref : ('a : value_or_null). ('a equal[@mode l]) -> ('a ref equal[@mode l])]
  end
end
