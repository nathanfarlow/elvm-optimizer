open! Core

module Make
    (Statement : Statement_intf.S)
    (Optimizer : Optimizer_intf.S with type target := Statement.t) : sig
  type t

  include Inplace_optimizer_intf.S with type t := t and type target := Statement.t Graph.t

  val create : Optimizer.t -> t
end
