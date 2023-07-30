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

let%expect_test "test simmple variable replacement" =
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
             fallthrough to __L3
         __L1: (Assign ((dst (Register A)) (src (Const 1))))
           references:
             Fallthrough from __L3
           branch:
             fallthrough to __L2
         __L2: (Putc (Const 1))
           references:
             Fallthrough from __L1
         __L3: (Assign ((dst (Register A)) (src (Const 0))))
           references:
             Fallthrough from __L0
           branch:
             fallthrough to __L1 |}]

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
  [%expect {| true |}];
  print graph;
  [%expect
    {|
    __L0: (Assign ((dst (Register A)) (src (Var (Register B)))))
      branch:
        fallthrough to __L3
    __L1: (Assign ((dst (Register B)) (src (Const 1))))
      references:
        Fallthrough from __L3
      branch:
        fallthrough to __L2
    __L2: (Putc (Var (Register A)))
      references:
        Fallthrough from __L1
    __L3: (Assign ((dst (Register A)) (src (Var (Register B)))))
      references:
        Fallthrough from __L0
      branch:
        fallthrough to __L1 |}]

let%expect_test "test merge from two parents" =
  let first_assignment =
    Node.create ~label:"assign_1"
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
  in
  let first_jump =
    Node.create ~label:"jump_1"
      ~stmt:(Ast_statement.Jump { cond = None; target = Label "target" })
  in
  Node.set_branch first_assignment (Some (Fallthrough first_jump));
  Node.set_references first_jump
    [ { from = first_assignment; type_ = Fallthrough } ];
  let second_assignment =
    Node.create ~label:"assign_2"
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
  in
  let second_jump =
    Node.create ~label:"jump_2"
      ~stmt:(Ast_statement.Jump { cond = None; target = Label "target" })
  in
  Node.set_branch second_assignment (Some (Fallthrough second_jump));
  Node.set_references second_jump
    [ { from = second_assignment; type_ = Fallthrough } ];
  let target =
    Node.create ~label:"target" ~stmt:(Ast_statement.Putc (Var (Register A)))
  in
  Node.set_branch first_jump (Some (Unconditional_jump target));
  Node.set_branch second_jump (Some (Unconditional_jump target));
  Node.set_references target
    [
      { from = first_jump; type_ = Jump }; { from = second_jump; type_ = Jump };
    ];
  let graph = Graph.create @@ Hashtbl.create (module String) in
  List.iter
    [ first_assignment; first_jump; second_assignment; second_jump; target ]
    ~f:(Graph.add_node graph);
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {| true |}];
  print graph;
  [%expect
    {|
     assign_1: (Assign ((dst (Register A)) (src (Const 0))))
       branch:
         fallthrough to jump_1
     assign_2: (Assign ((dst (Register A)) (src (Const 0))))
       branch:
         fallthrough to jump_2
     jump_1: (Jump ((target (Label target)) (cond ())))
       references:
         Fallthrough from assign_1
       branch:
         unconditional jump to target
     jump_2: (Jump ((target (Label target)) (cond ())))
       references:
         Fallthrough from assign_2
       branch:
         unconditional jump to target
     target: (Putc (Const 0))
       references:
         Jump from jump_1
         Jump from jump_2 |}]
