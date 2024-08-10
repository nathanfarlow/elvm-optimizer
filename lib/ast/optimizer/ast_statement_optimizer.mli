open! Core

module Make
    (Expression_optimizer : Optimizer_intf.S with type target := Ast.Expression.t) : sig
  type t

  include Optimizer_intf.S with type t := t and type target := Ast_statement.t

  val create : Expression_optimizer.t -> t
end
