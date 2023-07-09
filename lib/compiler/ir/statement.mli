module Assignment : sig
  type t = { dst : Expression.Variable.t; src : Expression.t }
  [@@deriving sexp, equal, hash]
end

module Jump : sig
  type t = { target : Expression.t; cond : Expression.Condition.t option }
  [@@deriving sexp, equal, hash]
end

type t =
  | Assign of Assignment.t
  | Putc of Expression.t
  | Jump of Jump.t
  | Exit
  | Nop
[@@deriving sexp, equal, hash]

val references : t -> string Hash_set.t
