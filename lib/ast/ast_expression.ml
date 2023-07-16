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

  include Propagator_rhs_intf.S with type t := t and type lhs := Variable.t
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

  let rec contains t var =
    match t with
    | Const _ | Label _ | Getc -> false
    | Var v -> Variable.equal v var
    | Add ts -> List.exists ts ~f:(fun t -> contains t var)
    | Sub (t1, t2) -> contains t1 var || contains t2 var
    | If { left; right; _ } -> contains left var || contains right var
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

  include Propagator_lhs_intf.S with type t := t
end = struct
  type t = Register of Eir.Register.t | Memory of Expression.t
  [@@deriving sexp, equal, compare, hash]

  let contains t var =
    match t with
    | Register _ -> false
    | Memory expr -> Expression.contains expr var
end

include Expression