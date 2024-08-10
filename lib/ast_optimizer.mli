open! Core
open Ast

val optimize_expression : Expression.t -> Expression.t * bool
val optimize_statement : Statement.t -> Statement.t * bool
