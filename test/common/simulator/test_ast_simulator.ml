open Elvm

module Delegate = struct
  type stmt = Ast_statement.t
  type lhs = Ast.Variable.t
  type rhs = Ast.Expression.t

  let substitute stmt ~lhs ~rhs =
    Ast_statement.substitute_lhs_to_rhs stmt ~from:lhs ~to_:rhs
end

module Simulator =
  Simulator.Make (Ast_statement) (Ast.Variable) (Ast.Expression) (Delegate)

module Environment = Simulator.Environment

let to_str env = Environment.sexp_of_t env |> Sexp.to_string_hum

let print_initial graph simulator =
  Ast_test_util.print_over_nodes graph
    ~f:(Simulator.get_initial_env simulator)
    ~to_str

let print_final graph simulator =
  Ast_test_util.print_over_nodes graph
    ~f:(Simulator.get_final_env simulator)
    ~to_str

let%expect_test "simple fallthrough sequence" =
  let graph, diagram = Ast_test_util.graph_simple_fallthrough_sequence in
  print_endline diagram;
  [%expect
    {|
    ┌─────────────┐
    │__L0:        │
    │    A = B    │
    └──────┬──────┘
           ▼
    ┌─────────────┐
    │__L1:        │
    │  mem[A] = 0 │
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │__L2:        │
    │    A = C    │
    └──────┬──────┘
           ▼
    ┌─────────────┐
    │__L3:        │
    │    putc A   │
    └──────┬──────┘
           ▼
    ┌─────────────┐
    │__L4:        │
    │     exit    │
    └─────────────┘ |}];
  let simulator = Simulator.create () in
  print_initial graph simulator;
  [%expect
    {|
    __L0: ()
    __L1: (((Register A) (Var (Register B))))
    __L2: (((Register A) (Var (Register B))) ((Memory (Var (Register B))) (Const 0)))
    __L3: (((Register B) (Var (Register C))))
    __L4: (((Register B) (Var (Register C)))) |}];
  print_final graph simulator;
  [%expect
    {|
    __L0: (((Register A) (Var (Register B))))
    __L1: (((Register A) (Var (Register B))) ((Memory (Var (Register B))) (Const 0)))
    __L2: (((Register B) (Var (Register C))))
    __L3: (((Register B) (Var (Register C))))
    __L4: (((Register B) (Var (Register C)))) |}]

(*
   let%expect_test "test initial env in simple fallthrough sequence" =
     let graph =
       [
         Assign { dst = Register A; src = Var (Register B) };
         Assign { dst = Register A; src = Var (Register C) };
       ]
       |> Ast_test_util.fallthrough
     in
     initial graph;
     [%expect {|
       __L0: ()
       __L1: (((Register A) (Var (Register B)))) |}];
     final graph;
     [%expect
       {|
       __L0: (((Register A) (Var (Register B))))
       __L1: (((Register B) (Var (Register C)))) |}]

   let%expect_test "test merge from two parents when matching" =
     (* A = 0; jump putc; A = 0; jump putc; putc A; exit*)
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
     let exit = Node.create ~label:"exit" ~stmt:Ast_statement.Exit in
     Node.set_branch target (Some (Fallthrough exit));
     Node.set_references exit [ { from = target; type_ = Fallthrough } ];
     let graph = Graph.create @@ Hashtbl.create (module String) in
     List.iter
       [
         first_assignment; first_jump; second_assignment; second_jump; target; exit;
       ]
       ~f:(Graph.register_node graph);
     initial graph;
     [%expect
       {|
       assign_1: ()
       assign_2: ()
       exit: (((Register A) (Const 0)))
       jump_1: (((Register A) (Const 0)))
       jump_2: (((Register A) (Const 0)))
       target: (((Register A) (Const 0))) |}];
     final graph;
     [%expect
       {|
       assign_1: (((Register A) (Const 0)))
       assign_2: (((Register A) (Const 0)))
       exit: (((Register A) (Const 0)))
       jump_1: (((Register A) (Const 0)))
       jump_2: (((Register A) (Const 0)))
       target: (((Register A) (Const 0))) |}]

   let%expect_test "test self loop" =
     (* A = 0; putc A; goto putc *)
     let first_assignment =
       Node.create ~label:"assign_1"
         ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
     in
     let putc =
       Node.create ~label:"putc" ~stmt:(Ast_statement.Putc (Var (Register A)))
     in
     let jump =
       Node.create ~label:"jump"
         ~stmt:(Ast_statement.Jump { cond = None; target = Label "putc" })
     in
     Node.set_branch first_assignment (Some (Fallthrough putc));
     Node.set_branch putc (Some (Fallthrough jump));
     Node.set_branch jump (Some (Unconditional_jump putc));
     Node.set_references putc
       [
         { from = first_assignment; type_ = Fallthrough };
         { from = jump; type_ = Jump };
       ];
     Node.set_references jump [ { from = putc; type_ = Fallthrough } ];
     let graph = Graph.create @@ Hashtbl.create (module String) in
     List.iter [ first_assignment; putc; jump ] ~f:(fun node ->
         Graph.register_node graph node);
     initial graph;
     [%expect
       {|
       assign_1: ()
       jump: (((Register A) (Const 0)))
       putc: (((Register A) (Const 0))) |}];
     final graph;
     [%expect
       {|
       assign_1: (((Register A) (Const 0)))
       jump: (((Register A) (Const 0)))
       putc: (((Register A) (Const 0))) |}]

   let%expect_test "test self loop with contradicting mappings" =
     (* A = 0; putc A; A = 1; goto putc *)
     let first_assignment =
       Node.create ~label:"assign_1"
         ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
     in
     let putc =
       Node.create ~label:"putc" ~stmt:(Ast_statement.Putc (Var (Register A)))
     in
     let second_assignment =
       Node.create ~label:"assign_2"
         ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 1 })
     in
     let jump =
       Node.create ~label:"jump"
         ~stmt:(Ast_statement.Jump { cond = None; target = Label "putc" })
     in
     Node.set_branch first_assignment (Some (Fallthrough putc));
     Node.set_branch putc (Some (Fallthrough second_assignment));
     Node.set_branch second_assignment (Some (Fallthrough jump));
     Node.set_branch jump (Some (Unconditional_jump putc));
     Node.set_references putc
       [
         { from = first_assignment; type_ = Fallthrough };
         { from = jump; type_ = Jump };
       ];
     Node.set_references second_assignment [ { from = putc; type_ = Fallthrough } ];
     Node.set_references jump [ { from = second_assignment; type_ = Fallthrough } ];
     let graph = Graph.create @@ Hashtbl.create (module String) in
     List.iter [ first_assignment; putc; second_assignment; jump ] ~f:(fun node ->
         Graph.register_node graph node);
     initial graph;
     [%expect
       {|
       assign_1: ()
       assign_2: ()
       jump: (((Register A) (Const 1)))
       putc: () |}];
     final graph;
     [%expect
       {|
       assign_1: (((Register A) (Const 0)))
       assign_2: (((Register A) (Const 1)))
       jump: (((Register A) (Const 1)))
       putc: () |}] *)
