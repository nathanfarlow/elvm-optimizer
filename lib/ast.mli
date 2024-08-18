open! Core

module rec Expression : sig
  type t =
    | Const of int
    | Label of string
    | Var of Variable.t
    | Add of t list
    | Sub of t * t
    | If of Condition.t
  [@@deriving sexp, equal, compare, hash]

  val substitute : t -> from:t -> to_:t -> t * bool
  val contains : t -> Variable.t -> bool
end

and Comparison : sig
  type t =
    | Eq
    | Ne
    | Lt
    | Le
  [@@deriving sexp, equal, compare, hash]
end

and Condition : sig
  type t =
    { cmp : Comparison.t
    ; left : Expression.t
    ; right : Expression.t
    }
  [@@deriving sexp, equal, compare, hash]

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
end

and Variable : sig
  type t =
    | Register of Eir.Register.t
    | Memory of Expression.t
  [@@deriving sexp, equal, compare, hash]

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
  val contains : t -> t -> bool

  include Comparator.S with type t := t
end

module Statement : sig
  module Assignment : sig
    type t =
      { dst : Variable.t
      ; src : Expression.t
      }
    [@@deriving sexp, equal, compare, hash]
  end

  module Jump : sig
    type t =
      { target : Expression.t
      ; cond : Condition.t option
      }
    [@@deriving sexp, equal, compare, hash]
  end

  type t =
    | Assign of Assignment.t
    | Putc of Expression.t
    | Getc of Variable.t
    | Jump of Jump.t
    | Exit
    | Nop
  [@@deriving sexp, equal, compare, hash]
end
