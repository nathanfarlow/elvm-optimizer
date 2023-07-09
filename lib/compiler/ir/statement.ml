module Assignment = struct
  type destination = Var of string | Memory of Expression.M.t
  [@@deriving sexp, equal]

  type t = { dst : destination; src : Expression.M.t } [@@deriving sexp, equal]
end

module Jump = struct
  type t = { target : Expression.M.t; cond : Expression.M.Condition.t }
  [@@deriving sexp, equal]
end

type t =
  | Assign of Assignment.t
  | Putc of Expression.M.t
  | Jump of Jump.t
  | Exit
  | Nop
[@@deriving sexp, equal]

let references _ = failwith "unimplemented"