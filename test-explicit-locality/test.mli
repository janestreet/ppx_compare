open! Base

(* NOTE: I tried to add the flag to an mlt but got the following error:

  {[
    Error: It is currently not possible to pass preprocessor flags to toplevel expect
    tests.
  ]}

  So I'm using this library to manually test that it works.
*)

module Localize_prevents_flag_from_raising : sig
  type t = int [@@deriving compare ~localize]
end

module Underscore_global_prevents_flag_from_raising : sig
  type t = int [@@deriving compare__global]
end

module Ppx_template_explicit_prevents_flag_from_raising : sig
  module Local : sig
    type%template t = int [@@deriving compare [@mode.explicit local]]
  end

  module Global : sig
    type%template t = int [@@deriving compare [@mode.explicit global]]
  end

  module Both : sig
    type%template t = int
    [@@mode m = (local, global)] [@@deriving compare [@mode.explicit m]]
  end
end
