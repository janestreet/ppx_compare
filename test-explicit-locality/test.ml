open! Base

module Localize_prevents_flag_from_raising = struct
  type t = int [@@deriving compare ~localize]
end

module Underscore_global_prevents_flag_from_raising = struct
  type t = int [@@deriving compare__global]
end

module Ppx_template_explicit_prevents_flag_from_raising = struct
  module Local = struct
    type%template t = int [@@deriving compare [@mode.explicit local]]
  end

  module Global = struct
    type%template t = int [@@deriving compare [@mode.explicit global]]
  end

  module Both = struct
    type%template t = int
    [@@mode m = (local, global)] [@@deriving compare [@mode.explicit m]]
  end
end
