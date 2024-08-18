open! Core
open Ast

(* TODO: can we do this better? *)
module VarSet : module type of Set.Make (Variable)

type t [@@deriving sexp_of]

val live_in : t -> VarSet.t
val live_out : t -> VarSet.t
val analyze : Statement.t Graph.t -> (t * Statement.t) Graph.t
