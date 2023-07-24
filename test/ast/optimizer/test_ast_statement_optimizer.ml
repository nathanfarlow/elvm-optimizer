open Elvm
module Statement_opt = Ast_statement_optimizer.Make (Ast_expression_optimizer)

let optimizer = Statement_opt.create @@ Ast_expression_optimizer.create ()
let print stmt = print_s [%sexp (stmt : Ast_statement.t)]
let optimize stmt = Statement_opt.optimize optimizer stmt |> fst
let ugly_exp = Ast.Expression.Add [ Const 1; Const 1 ]

let%expect_test "optimizes assign dst and src" =
  optimize (Assign { dst = Memory ugly_exp; src = ugly_exp }) |> print;
  [%expect {| (Assign ((dst (Memory (Const 2))) (src (Const 2)))) |}]

let%expect_test "optimizes putc arg" =
  optimize (Putc ugly_exp) |> print;
  [%expect {| (Putc (Const 2)) |}]

let%expect_test "exit is unchanged" =
  optimize Exit |> print;
  [%expect {| Exit |}]

let%expect_test "nop is unchanged" =
  optimize Nop |> print;
  [%expect {| Nop |}]

let%expect_test "optimizes jump target" =
  optimize (Jump { target = ugly_exp; cond = None }) |> print;
  [%expect {| (Jump ((target (Const 2)) (cond ()))) |}]

let%expect_test "always true conditional jump is changed to unconditional" =
  let always_true =
    Some Ast.Condition.{ cmp = Eq; left = ugly_exp; right = ugly_exp }
  in
  optimize (Jump { target = ugly_exp; cond = always_true }) |> print;
  [%expect {| (Jump ((target (Const 2)) (cond ()))) |}]

let%expect_test "always false conditional jump is changed to nop" =
  let always_false =
    Some Ast.Condition.{ cmp = Ne; left = ugly_exp; right = ugly_exp }
  in
  optimize (Jump { target = ugly_exp; cond = always_false }) |> print;
  [%expect {| Nop |}]