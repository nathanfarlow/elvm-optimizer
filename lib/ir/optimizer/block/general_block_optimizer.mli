open! Core

module Make
    (Statement_optimizer : Optimizer_intf.S with type target := Statement.t) : sig
  type t

  include Inplace_optimizer_intf.S with type t := t and type target := Block.M.t

  val create : Statement_optimizer.t -> t
end
