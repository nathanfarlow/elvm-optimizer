open! Core

module Make
    (Statement : Assignable_statement_intf.S)
    (Lhs : Environment_lhs_intf.S with type t = Statement.lhs)
    (_ : Environment_rhs_intf.S
           with type t = Statement.rhs
            and type lhs := Lhs.t) : sig
  type t

  val create : unit -> t

  include
    Inplace_optimizer_intf.S
      with type t := t
       and type target := Statement.t Graph.t
end
