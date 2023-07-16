module type S = sig
  type t
  type lhs
  type rhs
  type mapping = { from : lhs; to_ : rhs }

  val nop : t
  val from_mapping : mapping -> t
  val get_mapping_from_assignment : t -> mapping option
  val substitute_rhs_to_lhs : t -> from:rhs -> to_:lhs -> t * bool
end
