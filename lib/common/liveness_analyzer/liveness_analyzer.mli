module Make
    (Statement : Assignable_statement_intf.S)
    (Lhs : Liveness_analyzer_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Liveness_analyzer_rhs_intf.S
             with type t = Statement.rhs
              and type lhs := Lhs.t) : sig
  type t

  val create : unit -> t
  val get_living_after : t -> Statement.t Node.t -> Rhs.Lhs_set.t
end