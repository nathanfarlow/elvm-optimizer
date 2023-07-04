open! Core

type t = {
  label : string;
  statements : Statement.t list;
  branch : branch option;
}
[@@deriving sexp, equal]

(* for conditional branches, primary is when condition is true *)
and branch = { primary : t; secondary : t option }

val get_statements : t -> Statement.t list
val find_living : t -> Statement.variable list