open Core
open Elvm
open Ast.Statement

let optimize exp = Ast_optimizer.optimize_statement exp |> [%sexp_of: t] |> print_s
let ugly_exp = Ast.Expression.Add [ Const 1; Const 1 ]

let%expect_test "optimizes assign dst and src" =
  optimize (Assign { dst = Memory ugly_exp; src = ugly_exp });
  [%expect {| (Assign ((dst (Memory (Const 2))) (src (Const 2)))) |}]
;;

let%expect_test "optimizes putc arg" =
  optimize (Putc ugly_exp);
  [%expect {| (Putc (Const 2)) |}]
;;

let%expect_test "optimizes getc arg" =
  optimize (Getc (Memory ugly_exp));
  [%expect {| (Getc (Memory (Const 2))) |}]
;;

let%expect_test "exit is unchanged" =
  optimize Exit;
  [%expect {| Exit |}]
;;

let%expect_test "nop is unchanged" =
  optimize Nop;
  [%expect {| Nop |}]
;;

let%expect_test "optimizes jump target" =
  optimize (Jump { target = ugly_exp; cond = None });
  [%expect {| (Jump ((target (Const 2)) (cond ()))) |}]
;;

let%expect_test "optimizes jump condition" =
  optimize
    (Jump
       { target = ugly_exp
       ; cond = Some { cmp = Eq; left = ugly_exp; right = Var (Register A) }
       });
  [%expect
    {|
    (Jump
     ((target (Const 2))
      (cond (((cmp Eq) (left (Const 2)) (right (Var (Register A))))))))
    |}]
;;

let%expect_test "always true conditional jump is changed to unconditional" =
  let always_true = Some Ast.Condition.{ cmp = Eq; left = ugly_exp; right = ugly_exp } in
  optimize (Jump { target = ugly_exp; cond = always_true });
  [%expect {| (Jump ((target (Const 2)) (cond ()))) |}]
;;

let%expect_test "always false conditional jump is changed to nop" =
  let always_false = Some Ast.Condition.{ cmp = Ne; left = ugly_exp; right = ugly_exp } in
  optimize (Jump { target = ugly_exp; cond = always_false });
  [%expect {| Nop |}]
;;
