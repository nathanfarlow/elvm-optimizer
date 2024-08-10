open! Core

module Make
    (Statement : Substituter_statement_intf.S)
    (Lhs : Environment_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Environment_rhs_intf.S with type t = Statement.rhs and type lhs := Lhs.t)
    (_ : Substitute_delegate_intf.S
         with type stmt = Statement.t
          and type lhs = Lhs.t
          and type rhs = Rhs.t) : sig
  type t

  val create : unit -> t

  include Inplace_optimizer_intf.S with type t := t and type target := Statement.t Graph.t
end
