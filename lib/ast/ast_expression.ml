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

  include
    Forward_expression_intf.S with type t := t and type variable := Variable.t
end = struct
  type t =
    | Const of int
    | Label of string
    | Var of Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of Condition.t
  [@@deriving sexp, equal, compare, hash]

  let contains_var _t _var = failwith "todo"
end

and Comparison : sig
  type t = Eq | Ne | Lt | Le [@@deriving sexp, equal, compare, hash]
end =
  Comparison

and Condition : sig
  type t = { cmp : Comparison.t; left : Expression.t; right : Expression.t }
  [@@deriving sexp, equal, compare, hash]
end =
  Condition

and Variable : sig
  type t = Register of Eir.Register.t | Memory of Expression.t
  [@@deriving sexp, equal, compare, hash]

  include Forward_variable_intf.S with type t := t
end = struct
  type t = Register of Eir.Register.t | Memory of Expression.t
  [@@deriving sexp, equal, compare, hash]

  let contains_var _t _var = failwith "todo"
end

include Expression