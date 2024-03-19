open Ppxlib

module type Attrs = sig
  val ignore_label_declaration : (label_declaration, unit) Attribute.t
  val ignore_core_type : (core_type, unit) Attribute.t
end

module type S = sig
  (** [type_ ~with_local ~hide ty] is [ty -> ty -> result_type] where [result_type]
      is [int] for [compare] and [bool] for [equal].

      [hide] controls whether some [[@merlin.hide]] attributes should be added.

      [with_local] adds local_ annotation around the input types.
  *)
  val type_ : with_local:bool -> hide:bool -> loc:Location.t -> core_type -> core_type

  (** [core_type ~with_local ty] is an expression of type [ty -> ty -> result_type]

      [~with_local:true] will make the arguments local *)
  val core_type : with_local:bool -> core_type -> expression

  (** In [str_type_decl] and [sig_type_decl], passing [true] for the third argument
      generates additional functions that take local arguments. We generate, e.g.
      [val compare__local : local_ t -> local_ t -> int] in addition to [compare]
      in order to incrementally grow the portion of the tree which supports local
      comparison.

      We need both [compare] and [compare__local] since neither has a stronger type than
      the other. In the case of polymorphic types, this is due to the fact that
      [compare__local] requires local compare functions for each of its type arguments,
      and in the case of monomorphic types, this is due to the possibility of partial
      application producing a local closure. *)

  val str_type_decl
    :  ctxt:Expansion_context.Deriver.t
    -> rec_flag * type_declaration list
    -> bool (** [true] means generate a definition with local arguments *)
    -> structure

  val sig_type_decl
    :  ctxt:Expansion_context.Deriver.t
    -> rec_flag * type_declaration list
    -> bool (** [true] means generate a signature with local arguments *)
    -> signature

  module Attrs : Attrs

  val str_attributes : Attribute.packed list
end
