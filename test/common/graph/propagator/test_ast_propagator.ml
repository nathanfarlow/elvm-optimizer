open Elvm

module Propagator =
  Propagator.Make (Ast_statement) (Ast.Variable) (Ast.Expression)

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
  List.iter nodes ~f:(Graph.register_node graph);
  graph

let%expect_test "test simple variable replacement" =
  let graph =
    [
      Assign { dst = Register A; src = Const 0 }; Putc (Var (Register A)); Exit;
    ]
    |> fallthrough
  in
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {| true |}];
  print graph;
  [%expect
    {|
           __L0: Nop
             branch:
               fallthrough to __L1
           __L1: (Putc (Const 0))
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

let%expect_test "test updated replacement" =
  let graph =
    [
      Assign { dst = Register A; src = Const 0 };
      Assign { dst = Register A; src = Const 1 };
      Putc (Var (Register A));
      Exit;
    ]
    |> fallthrough
  in
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {|
       true |}];
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
            __L2: (Putc (Const 1))
              references:
                Fallthrough from __L1
              branch:
                fallthrough to __L4
            __L3: Exit
              references:
                Fallthrough from __L4
            __L4: (Assign ((dst (Register A)) (src (Const 1))))
              references:
                Fallthrough from __L2
              branch:
                fallthrough to __L3 |}]

let%expect_test "test variable is invalidated" =
  let graph =
    [
      Assign { dst = Register A; src = Var (Register B) };
      Assign { dst = Register B; src = Const 1 };
      Putc (Var (Register A));
      Exit;
    ]
    |> fallthrough
  in
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {|
       false |}];
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
       __L2: (Putc (Var (Register A)))
         references:
           Fallthrough from __L1
         branch:
           fallthrough to __L4
       __L3: Exit
         references:
           Fallthrough from __L4
       __L4: (Assign ((dst (Register B)) (src (Const 1))))
         references:
           Fallthrough from __L2
         branch:
           fallthrough to __L3 |}]

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
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {|
       true |}];
  print graph;
  [%expect
    {|
        __L0: (Assign ((dst (Register A)) (src (Const 0))))
          references:
            Fallthrough from target
          branch:
            fallthrough to exit
        assign_1: Nop
          branch:
            fallthrough to jump_1
        assign_2: Nop
          branch:
            fallthrough to jump_2
        exit: Exit
          references:
            Fallthrough from __L0
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
            Jump from jump_2
          branch:
            fallthrough to __L0 |}]

let%expect_test "test merge from two parents when conflicting" =
  (* A = 0; jump putc; A = 1; jump putc; putc A; exit*)
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
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 1 })
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
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {|
       false |}];
  print graph;
  [%expect
    {|
       __L0: (Assign ((dst (Register A)) (src (Const 1))))
         references:
           Fallthrough from assign_2
         branch:
           fallthrough to jump_2
       __L1: (Assign ((dst (Register A)) (src (Const 0))))
         references:
           Fallthrough from assign_1
         branch:
           fallthrough to jump_1
       assign_1: Nop
         branch:
           fallthrough to __L1
       assign_2: Nop
         branch:
           fallthrough to __L0
       exit: Exit
         references:
           Fallthrough from target
       jump_1: (Jump ((target (Label target)) (cond ())))
         references:
           Fallthrough from __L1
         branch:
           unconditional jump to target
       jump_2: (Jump ((target (Label target)) (cond ())))
         references:
           Fallthrough from __L0
         branch:
           unconditional jump to target
       target: (Putc (Var (Register A)))
         references:
           Jump from jump_1
           Jump from jump_2
         branch:
           fallthrough to exit |}]

let%expect_test "test substitutes after putc" =
  let graph =
    [
      Assign { dst = Register A; src = Const 0 };
      Putc (Var (Register A));
      Putc (Var (Register A));
    ]
    |> fallthrough
  in
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {|
     true |}];
  print graph;
  [%expect
    {|
    __L0: Nop
      branch:
        fallthrough to __L1
    __L1: (Putc (Const 0))
      references:
        Fallthrough from __L0
      branch:
        fallthrough to __L3
    __L2: (Putc (Const 0))
      references:
        Fallthrough from __L3
    __L3: (Assign ((dst (Register A)) (src (Const 0))))
      references:
        Fallthrough from __L1
      branch:
        fallthrough to __L2 |}]

let%expect_test "test self loop tricky optimization" =
  (* A = 0; putc A; A = 0; goto putc *)
  let first_assignment =
    Node.create ~label:"assign_1"
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
  in
  let putc =
    Node.create ~label:"putc" ~stmt:(Ast_statement.Putc (Var (Register A)))
  in
  let second_assignment =
    Node.create ~label:"assign_2"
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
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
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {|
    true |}];
  print graph;
  [%expect
    {|
     assign_1: Nop
       branch:
         fallthrough to putc
     assign_2: Nop
       references:
         Fallthrough from putc
       branch:
         fallthrough to jump
     jump: (Jump ((target (Label putc)) (cond ())))
       references:
         Fallthrough from assign_2
       branch:
         unconditional jump to putc
     putc: (Putc (Const 0))
       references:
         Fallthrough from assign_1
         Jump from jump
       branch:
         fallthrough to assign_2 |}]

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
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {| false |}];
  print graph;
  [%expect
    {|
       __L0: (Assign ((dst (Register A)) (src (Const 1))))
         references:
           Fallthrough from assign_2
         branch:
           fallthrough to jump
       assign_1: Nop
         branch:
           fallthrough to putc
       assign_2: Nop
         references:
           Fallthrough from putc
         branch:
           fallthrough to __L0
       jump: (Jump ((target (Label putc)) (cond ())))
         references:
           Fallthrough from __L0
         branch:
           unconditional jump to putc
       putc: (Putc (Var (Register A)))
         references:
           Fallthrough from assign_1
           Jump from jump
         branch:
           fallthrough to assign_2 |}]

let%expect_test "test repeated addition" =
  let graph =
    [
      Assign
        {
          dst = Register A;
          src = Add [ Ast.Expression.Var (Register A); Const 1 ];
        };
      Assign
        {
          dst = Register A;
          src = Add [ Ast.Expression.Var (Register A); Const 1 ];
        };
      Assign
        {
          dst = Register A;
          src = Add [ Ast.Expression.Var (Register A); Const 1 ];
        };
      Exit;
    ]
    |> fallthrough
  in
  let changed = propagate graph in
  printf "%b" changed;
  [%expect {|
       false |}];
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
       __L2: Nop
         references:
           Fallthrough from __L1
         branch:
           fallthrough to __L4
       __L3: Exit
         references:
           Fallthrough from __L4
       __L4: (Assign
        ((dst (Register A))
         (src
          (Add ((Const 1) (Add ((Const 1) (Add ((Var (Register A)) (Const 1))))))))))
         references:
           Fallthrough from __L2
         branch:
           fallthrough to __L3 |}]
