type comparison = Eq | Ne | Lt | Le [@@deriving sexp, equal]

and condition = { comparison : comparison; a : t; b : t }
[@@deriving sexp, equal]

and t =
  | Const of int
  | Register of Elvm_instruction.register
  | Memory of t
  | Add of t list
  | Sub of t * t
  | Getc
  | Set of condition
[@@deriving sexp]

val simplify : t -> t
val equal : t -> t -> bool
