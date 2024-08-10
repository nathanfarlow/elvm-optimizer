open! Core

module type S = sig
  include Assignable_statement_intf.S

  val nop : t
  val from_assignment : assignment -> t
end
