(* open Core *)
(* open Elvm *)
(* module Analyzer = Liveness_analyzer.Make (Ast.Statement) (Ast.Variable) (Ast.Expression) *)
(* module Var_set = Ast.Expression.Lhs_set *)

(* let print set = print_s (Var_set.sexp_of_t set) *)

(* (\* let%expect_test "simple" = *)
(*    let graph = *)
(*    [ Assign { dst = Register A; src = Var (Register B) } ] *)
(*    |> Ast_test_util.fallthrough *)
(*    in *)
(*    let analyzer = Analyzer.create () in *)
(*    let alive = *\) *)
