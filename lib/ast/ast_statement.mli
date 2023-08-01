module Assignment : sig
  type t = { dst : Ast.Variable.t; src : Ast.Expression.t }
  [@@deriving sexp, equal, compare, hash]
end

module Jump : sig
  type t = { target : Ast.Expression.t; cond : Ast.Condition.t option }
  [@@deriving sexp, equal, compare, hash]
end

type t =
  | Assign of Assignment.t
  | Putc of Ast.Expression.t
  | Jump of Jump.t
  | Exit
  | Nop
[@@deriving sexp, equal, compare, hash]

include Statement_intf.S with type t := t

include
  Propagator_statement_intf.S
    with type t := t
     and type lhs = Ast.Variable.t
     and type rhs = Ast.Expression.t

include
  Eliminator_statement_intf.S
    with type t := t
     and type lhs := Ast.Variable.t
     and type rhs := Ast.Expression.t
