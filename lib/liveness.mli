open! Core
open Ast

(* TODO: can we do this better? *)
module VarSet : module type of Set.Make (Variable)

type t =
  { mutable live_in : VarSet.t
  ; mutable live_out : VarSet.t
  }

val analyze : Statement.t Graph.t -> (t * Statement.t) Graph.t
