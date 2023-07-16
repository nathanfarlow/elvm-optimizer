module type S = sig
  type t
  type lhs
  type rhs
  type mapping = { from : lhs; to_ : rhs }

  val nop : t
  val from_mapping : mapping -> t
  val substitute_rev : t -> from:rhs -> to_:lhs -> t * bool
  val get_mapping_from_assignment : t -> mapping option
end