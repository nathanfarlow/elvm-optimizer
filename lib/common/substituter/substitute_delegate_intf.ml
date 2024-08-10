open! Core

module type S = sig
  type stmt
  type lhs
  type rhs

  val substitute : stmt -> lhs:lhs -> rhs:rhs -> stmt * bool
end
