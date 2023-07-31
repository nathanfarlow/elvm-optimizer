module Make
    (Statement : Eliminator_statement_intf.S)
    (Var : Propagator_var_intf.S with type t = Statement.var)
    (_ : Propagator_exp_intf.S with type t = Statement.exp and type var := Var.t) : sig
  type t

  val create : unit -> t

  include
    Inplace_optimizer_intf.S
      with type t := t
       and type target := Statement.t Graph.t
end
