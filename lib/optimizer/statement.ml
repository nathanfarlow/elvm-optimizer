open Core

type destination = Memory of Expression.t | Register of Register.t
[@@deriving sexp, equal]

type assignment = { dest : destination; src : Expression.t }
[@@deriving sexp, equal]

type jump = { target : Expression.t; condition : Expression.t option }
[@@deriving sexp, equal]

type t = Assign of assignment | Putc of Expression.t | Jmp of jump
[@@deriving sexp, equal]
