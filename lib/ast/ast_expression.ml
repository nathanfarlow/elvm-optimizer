module rec T : sig
  type t =
    | Const of int
    | Label of string
    | Var of T.Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of T.Condition.t
  [@@deriving sexp, equal, compare, hash]

  module Comparison : sig
    type t = Eq | Ne | Lt | Le [@@deriving sexp, equal, compare, hash]
  end

  module Condition : sig
    type t = { cmp : Comparison.t; left : T.t; right : T.t }
    [@@deriving sexp, equal, compare, hash]
  end

  module Variable : sig
    type t = Register of Eir.Register.t | Memory of T.t
    [@@deriving sexp, equal, compare, hash]
  end
end = T

include T