open Parsetree

(** [compare ty] is an expression of type [ty -> ty -> int] *)
val compare : core_type -> expression

(**/**)

module Internal : sig
  val compare_of_ty : core_type -> expression -> expression -> expression
  val chain_if : expression list -> expression
  val phys_equal_first : expression -> expression -> expression -> expression
  val compare_variant
    :  Location.t
    -> row_field list
    -> expression
    -> expression
    -> expression
  val tp_name : string -> string
end
