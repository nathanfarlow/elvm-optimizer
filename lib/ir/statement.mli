open! Core

type variable = Memory of Expression.t | Register of Register.t
[@@deriving sexp, equal]

type assignment = { dst : variable; src : Expression.t }
[@@deriving sexp, equal]

type jump = { target : Expression.t; condition : Expression.condition option }
[@@deriving sexp, equal]

type t =
  | Assign of assignment
  | Putc of Expression.t
  | Jump of jump
  | Exit
  | Nop
[@@deriving sexp, equal]

val references : t -> string list