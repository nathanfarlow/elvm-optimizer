open Core
open Elvm
module Graph_tests = Graph.For_tests (Ast_statement)

let print_graph graph = Graph_tests.to_string graph |> print_endline

let graph_of_nodes nodes =
  let graph = Graph.create @@ Hashtbl.create (module String) in
  List.iter nodes ~f:(Graph.register_node graph);
  graph

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

let print_over_nodes graph ~f ~to_str =
  Graph.nodes graph |> Hashtbl.to_alist
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.iter ~f:(fun (label, node) ->
         printf "%s: %s\n" label (f node |> to_str))

let graph_with_simple_fallthrough_sequence () =
  let diagram =
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
         ▼
  ┌─────────────┐
  │__L2:        │
  │ putc mem[A] │
  └──────┬──────┘
         ▼
  ┌─────────────┐
  │__L3:        │
  │    A = C    │
  └──────┬──────┘
         ▼
  ┌─────────────┐
  │__L4:        │
  │    putc A   │
  └──────┬──────┘
         ▼
  ┌─────────────┐
  │__L5:        │
  │     exit    │
  └─────────────┘ |}
  in
  let graph =
    [
      Assign { dst = Register A; src = Var (Register B) };
      Assign { dst = Memory (Var (Register A)); src = Const 0 };
      Putc (Var (Memory (Var (Register A))));
      Assign { dst = Register A; src = Var (Register C) };
      Putc (Var (Register A));
      Exit;
    ]
    |> fallthrough
  in
  (graph, diagram)

let graph_with_two_parents () =
  let diagram =
    {| 
    ┌─────────────┐      ┌─────────────┐
    │a_left:      │      │a_right:     │
    │    A = 0    │      │    A = 0    │
    └──────┬──────┘      └──────┬──────┘
           │                    │
           ▼                    ▼
    ┌─────────────┐      ┌─────────────┐
    │b_left:      │      │b_right:     │
    │    B = 0    │      │    B = 1    │
    └──────┬──────┘      └──────┬──────┘
           │                    │
           ▼                    ▼
    ┌─────────────┐      ┌─────────────┐
    │jmp_left:    │      │jmp_right:   │
    │  jmp putc_a │      │  jmp putc_a │
    └──────┬──────┘      └──────┬──────┘
           │                    │
           └───────┐    ┌───────┘
                   ▼    ▼
               ┌─────────────┐
               │putc_a:      │
               │   putc A    │
               └──────┬──────┘
                      │
                      ▼
               ┌─────────────┐
               │putc_b:      │
               │   putc B    │
               └──────┬──────┘
                      │
                      ▼
               ┌─────────────┐
               │exit:        │
               │     exit    │
               └─────────────┘ |}
  in
  let a_left =
    Node.create ~label:"a_left"
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
  in
  let a_right =
    Node.create ~label:"a_right"
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
  in
  let b_left =
    Node.create ~label:"b_left"
      ~stmt:(Ast_statement.Assign { dst = Register B; src = Const 0 })
  in
  let b_right =
    Node.create ~label:"b_right"
      ~stmt:(Ast_statement.Assign { dst = Register B; src = Const 1 })
  in
  let jmp_left =
    Node.create ~label:"jmp_left"
      ~stmt:(Ast_statement.Jump { cond = None; target = Label "putc_a" })
  in
  let jmp_right =
    Node.create ~label:"jmp_right"
      ~stmt:(Ast_statement.Jump { cond = None; target = Label "putc_a" })
  in
  let putc_a =
    Node.create ~label:"putc_a" ~stmt:(Ast_statement.Putc (Var (Register A)))
  in
  let putc_b =
    Node.create ~label:"putc_b" ~stmt:(Ast_statement.Putc (Var (Register B)))
  in
  let exit = Node.create ~label:"exit" ~stmt:Ast_statement.Exit in

  Node.set_branch a_left (Some (Fallthrough b_left));
  Node.set_references b_left [ { from = a_left; type_ = Fallthrough } ];

  Node.set_branch a_right (Some (Fallthrough b_right));
  Node.set_references b_right [ { from = a_right; type_ = Fallthrough } ];

  Node.set_branch b_left (Some (Fallthrough jmp_left));
  Node.set_references jmp_left [ { from = b_left; type_ = Fallthrough } ];

  Node.set_branch b_right (Some (Fallthrough jmp_right));
  Node.set_references jmp_right [ { from = b_right; type_ = Fallthrough } ];

  Node.set_branch jmp_left (Some (Unconditional_jump putc_a));
  Node.set_branch jmp_right (Some (Unconditional_jump putc_a));
  Node.set_references putc_a
    [ { from = jmp_left; type_ = Jump }; { from = jmp_right; type_ = Jump } ];

  Node.set_branch putc_a (Some (Fallthrough putc_b));
  Node.set_references putc_b [ { from = putc_a; type_ = Fallthrough } ];

  Node.set_branch putc_b (Some (Fallthrough exit));
  Node.set_references exit [ { from = putc_b; type_ = Fallthrough } ];

  let graph =
    graph_of_nodes
      [
        a_left;
        a_right;
        b_left;
        b_right;
        jmp_left;
        jmp_right;
        putc_a;
        putc_b;
        exit;
      ]
  in
  (graph, diagram)

let graph_with_unconditional_self_loop () =
  let diagram =
    {|
┌─────────────┐
│a_init:      │
│    A = 0    │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│b_init:      │
│    B = 0    │
└──────┬──────┘
       │
       │
       │    ┌─────┐
       ▼    ▼     │
  ┌─────────────┐ │
  │putc_a:      │ │
  │   putc A    │ │
  └──────┬──────┘ │
         │        │
         ▼        │
  ┌─────────────┐ │
  │putc_b:      │ │
  │   putc B    │ │
  └──────┬──────┘ │
         │        │
         ▼        │
  ┌─────────────┐ │
  │b_assign:    │ │
  │    B = 1    │ │
  └──────┬──────┘ │
         │        │
         ▼        │
  ┌─────────────┐ │
  │jmp:         │ │
  │  jmp putc_a │ │
  └──────┬──────┘ │
         │        │
         └────────┘ |}
  in
  let a_init =
    Node.create ~label:"a_init"
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 0 })
  in
  let b_init =
    Node.create ~label:"b_init"
      ~stmt:(Ast_statement.Assign { dst = Register B; src = Const 0 })
  in
  let putc_a =
    Node.create ~label:"putc_a" ~stmt:(Ast_statement.Putc (Var (Register A)))
  in
  let putc_b =
    Node.create ~label:"putc_b" ~stmt:(Ast_statement.Putc (Var (Register B)))
  in
  let b_assign =
    Node.create ~label:"b_assign"
      ~stmt:(Ast_statement.Assign { dst = Register B; src = Const 1 })
  in
  let jmp =
    Node.create ~label:"jmp"
      ~stmt:(Ast_statement.Jump { cond = None; target = Label "putc_a" })
  in

  Node.set_branch a_init (Some (Fallthrough b_init));
  Node.set_references b_init [ { from = a_init; type_ = Fallthrough } ];

  Node.set_branch b_init (Some (Fallthrough putc_a));

  Node.set_branch putc_a (Some (Fallthrough putc_b));
  Node.set_references putc_b [ { from = putc_a; type_ = Fallthrough } ];

  Node.set_branch putc_b (Some (Fallthrough b_assign));
  Node.set_references b_assign [ { from = putc_b; type_ = Fallthrough } ];

  Node.set_branch b_assign (Some (Fallthrough jmp));
  Node.set_references jmp [ { from = b_assign; type_ = Fallthrough } ];

  Node.set_branch jmp (Some (Unconditional_jump putc_a));
  Node.set_references putc_a
    [ { from = b_init; type_ = Fallthrough }; { from = jmp; type_ = Jump } ];

  let graph =
    graph_of_nodes [ a_init; b_init; putc_a; putc_b; b_assign; jmp ]
  in
  (graph, diagram)

let graph_with_conditional_self_loop () =
  let diagram =
    {| 
    ┌─────────────┐
    │a_init:      │
    │    A = 10   │
    └──────┬──────┘
           │
           │
           │    ┌─────┐
           ▼    ▼     │
      ┌─────────────┐ │
      │putc_a:      │ │
      │   putc A    │ │
      └──────┬──────┘ │
             │        │
             ▼        │
      ┌─────────────┐ │
      │sub_a:       │ │
      │  A = A - 1  │ │
      └──────┬──────┘ │
             │        │
             ▼        │
      ┌─────────────┐ │
      │sub_b:       │ │
      │  B = B - 1  │ │
      └──────┬──────┘ │
             │        │
             ▼        │
      ┌─────────────┐ │
      │sub_c:       │ │
      │  C = C - 1  │ │
      └──────┬──────┘ │
             │        │
             ▼        │
      ┌─────────────┐ │
      │jmp:         │ │
      │ if 0 < a:   │ │
      │  jmp putc_a │ │
      └───┬────┬────┘ │
    false │    │ true │
          │    └──────┘
          │
          ▼
   ┌────────────┐
   │putc_b:     │
   │   putc B   │
   └──────┬─────┘
          │
          │
   ┌──────┴─────┐
   │exit:       │
   │    exit    │
   └────────────┘ |}
  in

  let a_init =
    Node.create ~label:"a_init"
      ~stmt:(Ast_statement.Assign { dst = Register A; src = Const 10 })
  in
  let putc_a =
    Node.create ~label:"putc_a" ~stmt:(Ast_statement.Putc (Var (Register A)))
  in
  let sub_a =
    Node.create ~label:"sub_a"
      ~stmt:
        (Ast_statement.Assign
           { dst = Register A; src = Sub (Var (Register A), Const 1) })
  in
  let sub_b =
    Node.create ~label:"sub_b"
      ~stmt:
        (Ast_statement.Assign
           { dst = Register B; src = Sub (Var (Register B), Const 1) })
  in
  let sub_c =
    Node.create ~label:"sub_c"
      ~stmt:
        (Ast_statement.Assign
           { dst = Register C; src = Sub (Var (Register C), Const 1) })
  in
  let jmp =
    Node.create ~label:"jmp"
      ~stmt:
        (Ast_statement.Jump
           {
             cond = Some { cmp = Lt; left = Const 0; right = Var (Register A) };
             target = Label "putc_a";
           })
  in
  let putc_b =
    Node.create ~label:"putc_b" ~stmt:(Ast_statement.Putc (Var (Register B)))
  in
  let exit = Node.create ~label:"exit" ~stmt:Ast_statement.Exit in

  Node.set_branch a_init (Some (Fallthrough putc_a));

  Node.set_branch putc_a (Some (Fallthrough sub_a));
  Node.set_references sub_a [ { from = putc_a; type_ = Fallthrough } ];

  Node.set_branch sub_a (Some (Fallthrough sub_b));
  Node.set_references sub_b [ { from = sub_a; type_ = Fallthrough } ];

  Node.set_branch sub_b (Some (Fallthrough sub_c));
  Node.set_references sub_c [ { from = sub_b; type_ = Fallthrough } ];

  Node.set_branch sub_c (Some (Fallthrough jmp));
  Node.set_references jmp [ { from = sub_c; type_ = Fallthrough } ];

  Node.set_branch jmp
    (Some (Conditional_jump { true_ = putc_a; false_ = putc_b }));
  Node.set_references putc_a
    [ { from = a_init; type_ = Fallthrough }; { from = jmp; type_ = Jump } ];
  Node.set_references putc_b [ { from = jmp; type_ = Fallthrough } ];

  Node.set_branch putc_b (Some (Fallthrough exit));
  Node.set_references exit [ { from = putc_b; type_ = Fallthrough } ];

  let graph =
    graph_of_nodes [ a_init; putc_a; sub_a; sub_b; sub_c; jmp; putc_b; exit ]
  in
  (graph, diagram)
