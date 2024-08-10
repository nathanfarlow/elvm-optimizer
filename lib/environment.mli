open! Core
open Ast

type t [@@deriving sexp_of]

type update_result =
  { valid : t
  ; invalid : t
  }

val empty : t
val update : t -> from:Variable.t -> to_:Expression.t -> update_result
val intersection : t -> t -> t
val union : t -> t -> t
val diff : t -> t -> t
val get : t -> Variable.t -> Expression.t option
val to_alist : t -> (Variable.t * Expression.t) list
