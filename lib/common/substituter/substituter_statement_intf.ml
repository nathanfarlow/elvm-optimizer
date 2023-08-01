module type S = sig
  include Simulator_statement_intf.S

  val nop : t
  val from_assignment : assignment -> t
end