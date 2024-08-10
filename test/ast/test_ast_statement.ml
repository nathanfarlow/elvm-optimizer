open Core
open Elvm

let print stmt = print_s [%sexp (stmt : Ast_statement.t)]

let print_branch_type stmt =
  let type_ = Ast_statement.branch_type stmt in
  print_s [%sexp (type_ : Statement_intf.Branch_type.t option)]
;;

let print_assignment stmt =
  let assignment = Ast_statement.get_assignment stmt in
  print_s [%sexp (assignment : Ast_statement.assignment option)]
;;

let%expect_test "test nop" =
  let stmt = Ast_statement.nop in
  print stmt;
  [%expect {| Nop |}]
;;

let%expect_test "test is nop" =
  printf "%b" (Ast_statement.is_nop Nop);
  [%expect {| true |}]
;;

let%expect_test "test is not nop" =
  let stmt = Ast_statement.Putc (Const 0) in
  printf "%b" (Ast_statement.is_nop stmt);
  [%expect {| false |}]
;;

let%expect_test "exit is no branch type" =
  print_branch_type Exit;
  [%expect {| () |}]
;;

let%expect_test "nop is fallthrough branch type" =
  print_branch_type Nop;
  [%expect {| (Fallthrough) |}]
;;

let%expect_test "putc is fallthrough branch type" =
  print_branch_type (Putc (Const 0));
  [%expect {| (Fallthrough) |}]
;;

let%expect_test "assign is fallthrough branch type" =
  print_branch_type (Assign { dst = Register A; src = Const 0 });
  [%expect {| (Fallthrough) |}]
;;

let%expect_test "test jump is unconditional branch type" =
  print_branch_type (Jump { cond = None; target = Const 0 });
  [%expect {| (Unconditional_jump) |}]
;;

let%expect_test "test jump is conditional branch type" =
  print_branch_type
    (Jump { cond = Some { cmp = Eq; left = Const 0; right = Const 0 }; target = Const 0 });
  [%expect {| (Conditional_jump) |}]
;;

let%expect_test "test from assignment" =
  let assignment = Ast_statement.{ from = Register A; to_ = Const 0 } in
  print (Ast_statement.from_assignment assignment);
  [%expect {| (Assign ((dst (Register A)) (src (Const 0)))) |}]
;;

let%expect_test "test get mapping from assignment" =
  let assignment = Ast_statement.Assign { dst = Register A; src = Const 0 } in
  print_assignment assignment;
  [%expect {| (((from (Register A)) (to_ (Const 0)))) |}]
;;

let%expect_test "test get mapping from non assignment" =
  print_assignment Nop;
  [%expect {| () |}]
;;

let%expect_test "test substitute var to exp in assign" =
  let stmt =
    Ast_statement.Assign { dst = Memory (Var (Register A)); src = Var (Register A) }
  in
  let stmt', changed =
    Ast_statement.substitute_lhs_to_rhs
      stmt
      ~from:(Ast.Variable.Register A)
      ~to_:(Ast.Expression.Const 0)
  in
  printf "%b" changed;
  [%expect {| true |}];
  print stmt';
  [%expect {| (Assign ((dst (Memory (Const 0))) (src (Const 0)))) |}]
;;

let%expect_test "test substitute var to exp in putc" =
  let stmt = Ast_statement.Putc (Var (Register A)) in
  let stmt', changed =
    Ast_statement.substitute_lhs_to_rhs
      stmt
      ~from:(Ast.Variable.Register A)
      ~to_:(Ast.Expression.Const 0)
  in
  printf "%b" changed;
  [%expect {| true |}];
  print stmt';
  [%expect {| (Putc (Const 0)) |}]
;;

let%expect_test "test substitute var to exp in jump" =
  let stmt =
    Ast_statement.Jump
      { cond = Some { cmp = Eq; left = Var (Register A); right = Var (Register A) }
      ; target = Var (Register A)
      }
  in
  let stmt', changed =
    Ast_statement.substitute_lhs_to_rhs
      stmt
      ~from:(Ast.Variable.Register A)
      ~to_:(Ast.Expression.Const 0)
  in
  printf "%b" changed;
  [%expect {| true |}];
  print stmt';
  [%expect
    {|
       (Jump
        ((target (Const 0)) (cond (((cmp Eq) (left (Const 0)) (right (Const 0))))))) |}]
;;

let%expect_test "test substitute var to exp in exit" =
  let stmt = Ast_statement.Exit in
  let stmt', changed =
    Ast_statement.substitute_lhs_to_rhs
      stmt
      ~from:(Ast.Variable.Register A)
      ~to_:(Ast.Expression.Const 0)
  in
  printf "%b" changed;
  [%expect {| false |}];
  print stmt';
  [%expect {| Exit |}]
;;

let%expect_test "test substitute var to exp in nop" =
  let stmt = Ast_statement.Nop in
  let stmt', changed =
    Ast_statement.substitute_lhs_to_rhs
      stmt
      ~from:(Ast.Variable.Register A)
      ~to_:(Ast.Expression.Const 0)
  in
  printf "%b" changed;
  [%expect {| false |}];
  print stmt';
  [%expect {| Nop |}]
;;

let%expect_test "test substitute exp to var" =
  let stmt = Ast_statement.Putc (Const 0) in
  let stmt', changed =
    Ast_statement.substitute_rhs_to_lhs
      stmt
      ~from:(Ast.Expression.Const 0)
      ~to_:(Ast.Variable.Register A)
  in
  printf "%b" changed;
  [%expect {| true |}];
  print stmt';
  [%expect {| (Putc (Var (Register A))) |}]
;;
