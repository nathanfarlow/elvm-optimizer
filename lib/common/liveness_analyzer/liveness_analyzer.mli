module Make
    (Statement : Assignable_statement_intf.S)
    (Lhs : Liveness_analyzer_lhs_intf.S with type t = Statement.lhs)
    (_ : Liveness_analyzer_rhs_intf.S
           with type t = Statement.rhs
            and type lhs := Lhs.t) : sig
  module Lhs_set : module type of Set.Make (Lhs)

  type t

  val create : unit -> t
  val get_living_after : t -> Statement.t Node.t -> Lhs_set.t
end