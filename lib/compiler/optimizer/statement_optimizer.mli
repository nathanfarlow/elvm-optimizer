
module Make
    (Expression_optimizer : Optimizer_intf.S with type target := Expression.t) : sig
  type t

  include Optimizer_intf.S with type t := t and type target := Statement.t

  val create : Expression_optimizer.t -> t
end