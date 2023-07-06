module Make (Block_optimizer : Optimizer_intf.S with type target := Block.t) : sig
  type t

  include Optimizer_intf.S with type t := t and type target := Ir.t

  val create : Block_optimizer.t -> t
end
