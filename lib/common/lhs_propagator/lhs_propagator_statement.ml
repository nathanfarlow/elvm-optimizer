open! Core

module type S = sig
  include Substituter_statement_intf.S

  val substitute_lhs_to_rhs : t -> from:lhs -> to_:rhs -> t * bool
end
