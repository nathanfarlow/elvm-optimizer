module rec M : sig
  module Comparison : sig
    type t = Eq | Ne | Lt | Le [@@deriving sexp, equal]
  end

  module Condition : sig
    type t = { comp : Comparison.t; left : M.t; right : M.t }
    [@@deriving sexp, equal]
  end

  type t =
    | Const of int
    | Label of string
    | Var of string
    | Memory of t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of Condition.t
  [@@deriving sexp, equal]

  val equal : t -> t -> bool
  val references : t -> string Hash_set.t
end = struct
  module Comparison = struct
    type t = Eq | Ne | Lt | Le [@@deriving sexp, equal]
  end

  module Condition = struct
    type t = { comp : Comparison.t; left : M.t; right : M.t }
    [@@deriving sexp, equal]
  end

  type t =
    | Const of int
    | Label of string
    | Var of string
    | Memory of t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of Condition.t
  [@@deriving sexp, equal]

  let equal = failwith "unimplemented"
  let references = failwith "unimplemented"
end