open Elvm

let print stmt = print_s [%sexp (stmt : Ast_statement.t)]

let print_branch_type stmt =
  let type_ = Ast_statement.branch_type stmt in
  print_s [%sexp (type_ : Statement_intf.Branch_type.t option)]

let print_mapping stmt =
  let mapping = Ast_statement.get_mapping_from_assignment stmt in
  print_s [%sexp (mapping : Ast_statement.mapping option)]

let%expect_test "test nop" =
  let stmt = Ast_statement.nop in
  print stmt;
  [%expect {| Nop |}]

let%expect_test "test is nop" =
  printf "%b" (Ast_statement.is_nop Nop);
  [%expect {| true |}]

let%expect_test "test is not nop" =
  let stmt = Ast_statement.Putc (Const 0) in
  printf "%b" (Ast_statement.is_nop stmt);
  [%expect {| false |}]

let%expect_test "exit is no branch type" =
  print_branch_type Exit;
  [%expect {| () |}]

let%expect_test "nop is fallthrough branch type" =
  print_branch_type Nop;
  [%expect {| (Fallthrough) |}]

let%expect_test "putc is fallthrough branch type" =
  print_branch_type (Putc (Const 0));
  [%expect {| (Fallthrough) |}]

let%expect_test "assign is fallthrough branch type" =
  print_branch_type (Assign { dst = Register A; src = Const 0 });
  [%expect {| (Fallthrough) |}]

let%expect_test "test jump is unconditional branch type" =
  print_branch_type (Jump { cond = None; target = Const 0 });
  [%expect {| (Unconditional_jump) |}]

let%expect_test "test jump is conditional branch type" =
  print_branch_type
    (Jump
       {
         cond = Some { cmp = Eq; left = Const 0; right = Const 0 };
         target = Const 0;
       });
  [%expect {| (Conditional_jump) |}]

let%expect_test "test from mapping" =
  let mapping = Ast_statement.{ from = Register A; to_ = Const 0 } in
  print (Ast_statement.from_mapping mapping);
  [%expect {| (Assign ((dst (Register A)) (src (Const 0)))) |}]

let%expect_test "test get mapping from assignment" =
  let assignment = Ast_statement.Assign { dst = Register A; src = Const 0 } in
  print_mapping assignment;
  [%expect {| (((from (Register A)) (to_ (Const 0)))) |}]

let%expect_test "test get mapping from non assignment" =
  print_mapping Nop;
  [%expect {| () |}]
