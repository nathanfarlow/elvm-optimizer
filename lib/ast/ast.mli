module rec Expression : sig
  type t =
    | Const of int
    | Label of string
    | Var of Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of Condition.t
  [@@deriving sexp, equal, compare, hash]

  include Environment_rhs_intf.S with type t := t and type lhs := Variable.t

  include
    Liveness_analyzer_rhs_intf.S with type t := t and type lhs := Variable.t

  val substitute : t -> from:t -> to_:t -> t * bool
end

and Comparison : sig
  type t = Eq | Ne | Lt | Le [@@deriving sexp, equal, compare, hash]
end

and Condition : sig
  type t = { cmp : Comparison.t; left : Expression.t; right : Expression.t }
  [@@deriving sexp, equal, compare, hash]

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
end

and Variable : sig
  type t = Register of Eir.Register.t | Memory of Expression.t
  [@@deriving sexp, equal, compare, hash]

  include Environment_lhs_intf.S with type t := t
  include Liveness_analyzer_lhs_intf.S with type t := t

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
end
