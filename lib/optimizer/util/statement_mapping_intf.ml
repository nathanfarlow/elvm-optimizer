module type S = sig
  type t
  type lhs
  type rhs

  val nop : t
  val from_assignment : lhs -> rhs -> t
  val substitute : t -> lhs -> rhs -> t * bool
  val get_assignment : t -> (lhs * rhs) option
end