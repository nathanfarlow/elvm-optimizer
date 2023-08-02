module Make
    (Statement : Assignable_statement_intf.S)
    (Lhs : Environment_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Environment_rhs_intf.S
             with type t = Statement.rhs
              and type lhs := Lhs.t)
    (_ : Substitute_delegate_intf.S
           with type stmt = Statement.t
            and type lhs = Lhs.t
            and type rhs = Rhs.t) : sig
  module Environment :
    Environment_intf.S with type lhs := Lhs.t and type rhs := Rhs.t

  type t

  val create : unit -> t
  val get_initial_env : t -> Statement.t Node.t -> Environment.t
  val get_final_env : t -> Statement.t Node.t -> Environment.t
end
