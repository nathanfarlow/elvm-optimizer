open! Core
open! Ast

(* let substitute_all stmt env = *)
(*   List.fold *)
(*     (Environment.to_alist env) *)
(*     ~init:(stmt, false) *)
(*     ~f:(fun (stmt, did_substitute) (lhs, rhs) -> *)
(*       let stmt', did_substitute' = Substitute_delegate.substitute stmt ~lhs ~rhs in *)
(*       stmt', did_substitute || did_substitute') *)
(* ;; *)
