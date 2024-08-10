open Core
open Elvm
module Statement_opt = Ast_statement_optimizer.Make (Ast_expression_optimizer)
module Graph_opt = Statement_graph_optimizer.Make (Ast.Statement) (Statement_opt)
module Graph_tests = Graph.For_tests (Ast.Statement)

let optimizer =
  Ast_expression_optimizer.create () |> Statement_opt.create |> Graph_opt.create
;;

let optimize = Graph_opt.optimize optimizer
let print result = Graph_tests.to_string result |> print_endline

let graph_of_nodes nodes =
  let alist = List.map nodes ~f:(fun node -> Node.label node, node) in
  Graph.create @@ Hashtbl.of_alist_exn (module String) alist
;;

let%expect_test "optimizes statements" =
  let node =
    Node.create
      ~label:"foo"
      ~stmt:(Ast.Statement.Assign { dst = Register A; src = Add [ Const 1; Const 1 ] })
  in
  let graph = graph_of_nodes [ node ] in
  let changed = optimize graph in
  printf "%b" changed;
  [%expect "true"];
  print graph;
  [%expect {| foo: (Assign ((dst (Register A)) (src (Const 2)))) |}]
;;

let%expect_test "corrects optimized branch" =
  let true_ = Node.create ~label:"true" ~stmt:Ast.Statement.Nop in
  let false_ = Node.create ~label:"false" ~stmt:Ast.Statement.Nop in
  let conditional_jump =
    Node.create
      ~label:"cond_jump"
      ~stmt:
        (Ast.Statement.Jump
           { target = Label "true"
           ; cond = Some { cmp = Eq; left = Const 1; right = Const 1 }
           })
  in
  Node.set_branch conditional_jump (Some (Node.Branch.Conditional_jump { true_; false_ }));
  Node.set_references true_ [ { from = conditional_jump; type_ = Jump } ];
  Node.set_references false_ [ { from = conditional_jump; type_ = Fallthrough } ];
  let graph = graph_of_nodes [ conditional_jump; true_; false_ ] in
  let changed = optimize graph in
  printf "%b" changed;
  [%expect "true"];
  print graph;
  [%expect
    {|
    cond_jump: (Jump ((target (Label true)) (cond ())))
      branch:
        unconditional jump to true
    false: Nop
    true: Nop
      references:
        Jump from cond_jump |}]
;;
