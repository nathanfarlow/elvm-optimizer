open! Core

module type S = sig
  include Substituter_statement_intf.S

  val substitute_rhs_to_lhs : t -> from:rhs -> to_:lhs -> t * bool
end
