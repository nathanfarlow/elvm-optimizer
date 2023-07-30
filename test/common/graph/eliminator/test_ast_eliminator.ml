open Elvm
module Mapping = Propagator_mapping.Make (Ast.Variable) (Ast.Expression)
module Eliminator = Eliminator.Make (Ast_statement) (Mapping)
module Graph_tests = Graph.For_tests (Ast_statement)

let eliminator = Eliminator.create ()
let eliminate = Eliminator.optimize eliminator
let print = Test_ast_propagator.print
let fallthrough = Test_ast_propagator.fallthrough

let%expect_test "simple expression is eliminated" =
  let graph =
    [ Assign { dst = Register A; src = Const 0 }; Putc (Const 0) ]
    |> fallthrough
  in
  let changed = eliminate graph in
  printf "%b" changed;
  [%expect {| true |}];
  print graph;
  [%expect {|
    __L0: (Assign ((dst (Register A)) (src (Const 0))))
      branch:
        fallthrough to __L1
    __L1: (Putc (Var (Register A)))
      references:
        Fallthrough from __L0 |}]
