open Ppx_compare_lib

type enum = A | B | C

let optim_bool      : bool      compare = polymorphic_compare
let optim_char      : char      compare = polymorphic_compare
let optim_float     : float     compare = polymorphic_compare
let optim_int       : int       compare = polymorphic_compare
let optim_int32     : int32     compare = polymorphic_compare
let optim_int64     : int64     compare = polymorphic_compare
let optim_nativeint : nativeint compare = polymorphic_compare
let optim_string    : string    compare = polymorphic_compare
let optim_unit      : unit      compare = polymorphic_compare
let optim_enum      : enum      compare = polymorphic_compare
