open Core
open Elvm

module Delegate = struct
  type stmt = Ast.Statement.t
  type lhs = Ast.Variable.t
  type rhs = Ast.Expression.t

  let substitute stmt ~lhs ~rhs =
    Ast.Statement.substitute_lhs_to_rhs stmt ~from:lhs ~to_:rhs
  ;;
end

module Simulator =
  Simulator.Make (Ast.Statement) (Ast.Variable) (Ast.Expression) (Delegate)

module Environment = Simulator.Environment

let to_str env = Environment.sexp_of_t env |> Sexp.to_string_hum

let print_initial_env graph simulator =
  Ast_test_util.print_over_nodes graph ~f:(Simulator.get_initial_env simulator) ~to_str
;;

let print_final_env graph simulator =
  Ast_test_util.print_over_nodes graph ~f:(Simulator.get_final_env simulator) ~to_str
;;

let%expect_test "simple fallthrough sequence" =
  let graph, graph_as_str = Ast_test_util.graph_with_simple_fallthrough_sequence () in
  print_endline graph_as_str;
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
       └─────────────┘ |}];
  let simulator = Simulator.create () in
  print_initial_env graph simulator;
  [%expect
    {|
       __L0: ()
       __L1: (((Register A) (Var (Register B))))
       __L2: (((Register A) (Var (Register B))) ((Memory (Var (Register B))) (Const 0)))
       __L3: (((Register A) (Var (Register B))) ((Memory (Var (Register B))) (Const 0)))
       __L4: (((Register B) (Var (Register C))))
       __L5: (((Register B) (Var (Register C)))) |}];
  print_final_env graph simulator;
  [%expect
    {|
       __L0: (((Register A) (Var (Register B))))
       __L1: (((Register A) (Var (Register B))) ((Memory (Var (Register B))) (Const 0)))
       __L2: (((Register A) (Var (Register B))) ((Memory (Var (Register B))) (Const 0)))
       __L3: (((Register B) (Var (Register C))))
       __L4: (((Register B) (Var (Register C))))
       __L5: (((Register B) (Var (Register C)))) |}]
;;

let%expect_test "graph with two parents" =
  let _graph, graph_as_str = Ast_test_util.graph_with_two_parents () in
  print_endline graph_as_str;
  [%expect
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
                  └─────────────┘ |}];
  let simulator = Simulator.create () in
  print_initial_env _graph simulator;
  [%expect
    {|
       a_left: ()
       a_right: ()
       b_left: (((Register A) (Const 0)))
       b_right: (((Register A) (Const 0)))
       exit: (((Register A) (Const 0)))
       jmp_left: (((Register A) (Const 0)) ((Register B) (Const 0)))
       jmp_right: (((Register A) (Const 0)) ((Register B) (Const 1)))
       putc_a: (((Register A) (Const 0)))
       putc_b: (((Register A) (Const 0))) |}];
  print_final_env _graph simulator;
  [%expect
    {|
       a_left: (((Register A) (Const 0)))
       a_right: (((Register A) (Const 0)))
       b_left: (((Register A) (Const 0)) ((Register B) (Const 0)))
       b_right: (((Register A) (Const 0)) ((Register B) (Const 1)))
       exit: (((Register A) (Const 0)))
       jmp_left: (((Register A) (Const 0)) ((Register B) (Const 0)))
       jmp_right: (((Register A) (Const 0)) ((Register B) (Const 1)))
       putc_a: (((Register A) (Const 0)))
       putc_b: (((Register A) (Const 0))) |}]
;;

let%expect_test "graph with unconditional self loop" =
  let graph, graph_as_str = Ast_test_util.graph_with_unconditional_self_loop () in
  print_endline graph_as_str;
  [%expect
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
             └────────┘ |}];
  let simulator = Simulator.create () in
  print_initial_env graph simulator;
  [%expect
    {|
    a_init: ()
    b_assign: (((Register A) (Const 0)))
    b_init: (((Register A) (Const 0)))
    jmp: (((Register A) (Const 0)) ((Register B) (Const 1)))
    putc_a: (((Register A) (Const 0)))
    putc_b: (((Register A) (Const 0))) |}];
  print_final_env graph simulator;
  [%expect
    {|
    a_init: (((Register A) (Const 0)))
    b_assign: (((Register A) (Const 0)) ((Register B) (Const 1)))
    b_init: (((Register A) (Const 0)) ((Register B) (Const 0)))
    jmp: (((Register A) (Const 0)) ((Register B) (Const 1)))
    putc_a: (((Register A) (Const 0)))
    putc_b: (((Register A) (Const 0))) |}]
;;

let%expect_test "graph with conditional self loop" =
  let graph, graph_as_str = Ast_test_util.graph_with_conditional_self_loop () in
  print_endline graph_as_str;
  [%expect
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
     └────────────┘ |}];
  let simulator = Simulator.create () in
  print_initial_env graph simulator;
  [%expect
    {|
     a_init: ()
     exit: (((Register A) (Sub (Const 10) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1)))
      ((Register C) (Sub (Var (Register C)) (Const 1))))
     jmp: (((Register A) (Sub (Const 10) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1)))
      ((Register C) (Sub (Var (Register C)) (Const 1))))
     putc_a: ()
     putc_b: (((Register A) (Sub (Const 10) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1)))
      ((Register C) (Sub (Var (Register C)) (Const 1))))
     sub_a: ()
     sub_b: (((Register A) (Sub (Var (Register A)) (Const 1))))
     sub_c: (((Register A) (Sub (Var (Register A)) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1)))) |}];
  print_final_env graph simulator;
  [%expect
    {|
     a_init: (((Register A) (Const 10)))
     exit: (((Register A) (Sub (Const 10) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1)))
      ((Register C) (Sub (Var (Register C)) (Const 1))))
     jmp: (((Register A) (Sub (Const 10) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1)))
      ((Register C) (Sub (Var (Register C)) (Const 1))))
     putc_a: ()
     putc_b: (((Register A) (Sub (Const 10) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1)))
      ((Register C) (Sub (Var (Register C)) (Const 1))))
     sub_a: (((Register A) (Sub (Var (Register A)) (Const 1))))
     sub_b: (((Register A) (Sub (Var (Register A)) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1))))
     sub_c: (((Register A) (Sub (Var (Register A)) (Const 1)))
      ((Register B) (Sub (Var (Register B)) (Const 1)))
      ((Register C) (Sub (Var (Register C)) (Const 1)))) |}]
;;
