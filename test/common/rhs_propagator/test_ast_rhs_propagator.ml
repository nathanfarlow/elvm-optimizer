open Elvm

module Eliminator =
  Rhs_propagator.Make (Ast_statement) (Ast.Variable) (Ast.Expression)

module Graph_tests = Graph.For_tests (Ast_statement)

let eliminator = Eliminator.create ()
let eliminate = Eliminator.optimize eliminator
let print = Test_ast_lhs_propagator.print
let fallthrough = Test_ast_lhs_propagator.fallthrough

let%expect_test "simple expression is eliminated" =
  let graph =
    [ Assign { dst = Register A; src = Const 0 }; Putc (Const 0); Exit ]
    |> fallthrough
  in
  let changed = eliminate graph in
  printf "%b" changed;
  [%expect {| true |}];
  print graph;
  [%expect
    {|
    __L0: Nop
      branch:
        fallthrough to __L1
    __L1: (Putc (Var (Register A)))
      references:
        Fallthrough from __L0
      branch:
        fallthrough to __L3
    __L2: Exit
      references:
        Fallthrough from __L3
    __L3: (Assign ((dst (Register A)) (src (Const 0))))
      references:
        Fallthrough from __L1
      branch:
        fallthrough to __L2 |}]

let%expect_test "expression is not eliminated when invalidated" =
  let graph =
    [
      Assign { dst = Register A; src = Const 0 };
      Assign { dst = Register A; src = Var (Register B) };
      Putc (Const 0);
      Exit;
    ]
    |> fallthrough
  in
  let changed = eliminate graph in
  printf "%b" changed;
  [%expect {| false |}];
  print graph;
  [%expect
    {|
    __L0: Nop
      branch:
        fallthrough to __L1
    __L1: Nop
      references:
        Fallthrough from __L0
      branch:
        fallthrough to __L2
    __L2: (Putc (Const 0))
      references:
        Fallthrough from __L1
      branch:
        fallthrough to __L4
    __L3: Exit
      references:
        Fallthrough from __L4
    __L4: (Assign ((dst (Register A)) (src (Var (Register B)))))
      references:
        Fallthrough from __L2
      branch:
        fallthrough to __L3 |}]
