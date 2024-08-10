open! Core

type t

include Optimizer_intf.S with type t := t and type target := Ast.Expression.t

val create : unit -> t
