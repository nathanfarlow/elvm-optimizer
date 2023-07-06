module Make
    (Statement_optimizer : Optimizer_intf.S with type target := Statement.t) : sig
  type t

  include Optimizer_intf.S with type t := t and type target := Block.t

  val create : Statement_optimizer.t -> t
end
