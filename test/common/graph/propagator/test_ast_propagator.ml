open Elvm
module Mapping = Propagator_mapping.Make (Ast.Variable) (Ast.Expression)
module Propagator = Propagator.Make (Ast_statement) (Mapping)
module Graph_tests = Graph.For_tests (Ast_statement)

let propagator = Propagator.create ()
let print graph = Graph_tests.to_string graph |> print_endline
let propagate = Propagator.optimize propagator

let fallthrough (stmts : Ast_statement.t list) =
  let graph = Graph.create @@ Hashtbl.create (module String) in
  let nodes =
    List.map stmts ~f:(fun stmt ->
        Node.create ~label:(Graph.fresh_label graph) ~stmt)
  in
  let pairs, _ = List.zip_with_remainder nodes (List.tl_exn nodes) in
  List.iter pairs ~f:(fun (node, next_node) ->
      Node.set_branch node (Some (Fallthrough next_node));
      Node.set_references next_node [ { from = node; type_ = Fallthrough } ]);
  List.iter nodes ~f:(Graph.add_node graph);
  graph

let%expect_test "test simple variable replacement" =
  let graph =
    [ Assign { dst = Register A; src = Const 0 }; Putc (Var (Register A)) ]
    |> fallthrough
  in
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {| true |}];
  print graph;
  [%expect
    {|
    __L0: (Assign ((dst (Register A)) (src (Const 0))))
      branch:
        fallthrough to __L1
    __L1: (Putc (Const 0))
      references:
        Fallthrough from __L0 |}]

let%expect_test "test updated replacement" =
  let graph =
    [
      Assign { dst = Register A; src = Const 0 };
      Assign { dst = Register A; src = Const 1 };
      Putc (Var (Register A));
    ]
    |> fallthrough
  in
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {| true |}];
  print graph;
  [%expect
    {|
    __L0: (Assign ((dst (Register A)) (src (Const 0))))
      branch:
        fallthrough to __L1
    __L1: (Assign ((dst (Register A)) (src (Const 1))))
      references:
        Fallthrough from __L0
      branch:
        fallthrough to __L2
    __L2: (Putc (Const 1))
      references:
        Fallthrough from __L1 |}]

let%expect_test "test variable is invalidated" =
  let graph =
    [
      Assign { dst = Register A; src = Var (Register B) };
      Assign { dst = Register B; src = Const 1 };
      Putc (Var (Register A));
    ]
    |> fallthrough
  in
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {| false |}];
  print graph;
  [%expect
    {|
    __L0: (Assign ((dst (Register A)) (src (Var (Register B)))))
      branch:
        fallthrough to __L1
    __L1: (Assign ((dst (Register B)) (src (Const 1))))
      references:
        Fallthrough from __L0
      branch:
        fallthrough to __L2
    __L2: (Putc (Var (Register A)))
      references:
        Fallthrough from __L1 |}]
