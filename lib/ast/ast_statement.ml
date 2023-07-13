module Assignment = struct
  type t = { dst : Ast_expression.Variable.t; src : Ast_expression.t }
  [@@deriving sexp, equal, compare, hash]
end

module Jump = struct
  type t = { target : Ast_expression.t; cond : Ast_expression.Condition.t option }
  [@@deriving sexp, equal, compare, hash]
end

type t =
  | Assign of Assignment.t
  | Putc of Ast_expression.t
  | Jump of Jump.t
  | Exit
  | Nop
[@@deriving sexp, equal, compare, hash]
