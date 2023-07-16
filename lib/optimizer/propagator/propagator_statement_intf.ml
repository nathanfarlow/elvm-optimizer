module type S = sig
  type t
  type lhs
  type rhs
  type mapping = { from : lhs; to_ : rhs }

  val nop : t
  val from_mapping : mapping -> t
  val get_mapping_from_assignment : t -> mapping option
  val substitute_lhs_to_rhs : t -> from:lhs -> to_:rhs -> t * bool
end