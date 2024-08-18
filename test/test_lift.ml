open Core
open Async
open Elvm

let eir s = Eir.parse_exn s |> Lift.f

let print_graph (graph, _) =
  let to_string stmt = [%sexp (stmt : Ast.Statement.t)] |> Sexp.to_string_hum ~indent:2 in
  Graph_util.to_ascii graph to_string >>| print_endline
;;

let print_data (_, data) = [%message (data : Eir.Data.t list Map.M(String).t)] |> print_s

let print (graph, data) =
  print_data (graph, data);
  print_graph (graph, data)
;;

let%expect_test "no insns" =
  let%bind () = eir "" |> print_graph in
  [%expect {| |}];
  return ()
;;

let%expect_test "mov is lifted correctly" =
  let%bind () = eir {|
    .text
    mov A, A
    exit
|} |> print_graph in
  [%expect
    {|
    +--------------------------------------------------------+
    |                                                        |
    | __L0:                                                  |
    |                                                        |
    | (Assign ((dst (Register A)) (src (Var (Register A))))) |
    +--------------------------------------------------------+
      |
      |
      v
    +--------------------------------------------------------+
    |                                                        |
    | __L1:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "add is lifted correctly" =
  let%bind () = eir {|
    .text
    add A, 5
    exit
|} |> print_graph in
  [%expect
    {|
    +--------------------------------------------------------------------------+
    |                                                                          |
    | __L0:                                                                    |
    |                                                                          |
    | (Assign ((dst (Register A)) (src (Add ((Var (Register A)) (Const 5)))))) |
    +--------------------------------------------------------------------------+
      |
      |
      v
    +--------------------------------------------------------------------------+
    |                                                                          |
    | __L1:                                                                    |
    |                                                                          |
    | Exit                                                                     |
    +--------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "sub is lifted correctly" =
  let%bind () = eir {|
    .text
    sub A, 5
    exit
|} |> print_graph in
  [%expect
    {|
    +------------------------------------------------------------------------+
    |                                                                        |
    | __L0:                                                                  |
    |                                                                        |
    | (Assign ((dst (Register A)) (src (Sub (Var (Register A)) (Const 5))))) |
    +------------------------------------------------------------------------+
      |
      |
      v
    +------------------------------------------------------------------------+
    |                                                                        |
    | __L1:                                                                  |
    |                                                                        |
    | Exit                                                                   |
    +------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "load is lifted correctly" =
  let%bind () = eir {|
    .text
    load A, B
    exit
|} |> print_graph in
  [%expect
    {|
    +-----------------------------------------------------------------------+
    |                                                                       |
    | __L0:                                                                 |
    |                                                                       |
    | (Assign ((dst (Register A)) (src (Var (Memory (Var (Register B))))))) |
    +-----------------------------------------------------------------------+
      |
      |
      v
    +-----------------------------------------------------------------------+
    |                                                                       |
    | __L1:                                                                 |
    |                                                                       |
    | Exit                                                                  |
    +-----------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "store is lifted correctly" =
  let%bind () = eir {|
    .text
    store A, B
    exit
|} |> print_graph in
  [%expect
    {|
    +-----------------------------------------------------------------------+
    |                                                                       |
    | __L0:                                                                 |
    |                                                                       |
    | (Assign ((dst (Memory (Var (Register B)))) (src (Var (Register A))))) |
    +-----------------------------------------------------------------------+
      |
      |
      v
    +-----------------------------------------------------------------------+
    |                                                                       |
    | __L1:                                                                 |
    |                                                                       |
    | Exit                                                                  |
    +-----------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "putc is lifted correctly" =
  let%bind () = eir {|
    .text
    putc A
    exit
|} |> print_graph in
  [%expect
    {|
    +---------------------------+
    |                           |
    | __L0:                     |
    |                           |
    | (Putc (Var (Register A))) |
    +---------------------------+
      |
      |
      v
    +---------------------------+
    |                           |
    | __L1:                     |
    |                           |
    | Exit                      |
    +---------------------------+
    |}];
  return ()
;;

let%expect_test "getc is lifted correctly" =
  let%bind () = eir {|
    .text
    getc A
    exit
|} |> print_graph in
  [%expect
    {|
    +---------------------+
    |                     |
    | __L0:               |
    |                     |
    | (Getc (Register A)) |
    +---------------------+
      |
      |
      v
    +---------------------+
    |                     |
    | __L1:               |
    |                     |
    | Exit                |
    +---------------------+
    |}];
  return ()
;;

let%expect_test "exit is lifted correctly" =
  let%bind () = eir {|
    .text
    exit
|} |> print_graph in
  [%expect
    {|
    +-------+
    |       |
    | __L0: |
    |       |
    | Exit  |
    +-------+
    |}];
  return ()
;;

let%expect_test "jump is lifted correctly" =
  let%bind () = eir {|
    .text
    jmp A
    exit
|} |> print_graph in
  [%expect
    {|
    +------------------------------------------------+
    |                                                |
    | __L0:                                          |
    |                                                |
    | (Jump ((target (Var (Register A))) (cond ()))) |
    +------------------------------------------------+
      |
      |
      v
    +------------------------------------------------+
    |                                                |
    | __L1:                                          |
    |                                                |
    | Exit                                           |
    +------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "set is lifted correctly" =
  let%bind () = eir {|
    .text
    ne B, 0
    exit
|} |> print_graph in
  [%expect
    {|
    +---------------------------------------------------------------------+
    |                                                                     |
    | __L0:                                                               |
    |                                                                     |
    | (Assign                                                             |
    | ((dst (Register B))                                                 |
    | (src (If ((cmp Ne) (left (Var (Register B))) (right (Const 0))))))) |
    +---------------------------------------------------------------------+
      |
      |
      v
    +---------------------------------------------------------------------+
    |                                                                     |
    | __L1:                                                               |
    |                                                                     |
    | Exit                                                                |
    +---------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "dump is lifted correctly to nop" =
  let%bind () = eir {|
    .text
    dump
    exit
|} |> print_graph in
  [%expect
    {|
    +-------+
    |       |
    | __L0: |
    |       |
    | Nop   |
    +-------+
      |
      |
      v
    +-------+
    |       |
    | __L1: |
    |       |
    | Exit  |
    +-------+
    |}];
  return ()
;;

let%expect_test "Instructions are chunked" =
  let%bind () =
    eir {|
    .text
    mov A, A
    exit
    exit
    exit
|} |> print_graph
  in
  [%expect
    {|
    +--------------------------------------------------------+
    |                                                        |
    | __L0:                                                  |
    |                                                        |
    | (Assign ((dst (Register A)) (src (Var (Register A))))) |
    +--------------------------------------------------------+
      |
      |
      v
    +--------------------------------------------------------+
    |                                                        |
    | __L1:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    +--------------------------------------------------------+
    |                                                        |
    | __L2:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    +--------------------------------------------------------+
    |                                                        |
    | __L3:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "unconditional branch has edge to target" =
  let%bind () = eir {|
    .text
    jmp foo
    exit
  foo:
    exit
|} |> print_graph in
  [%expect
    {|
    +-----------------------------------------+
    |                                         |
    | __L0:                                   |
    |                                         |
    | (Jump ((target (Label foo)) (cond ()))) |
    +-----------------------------------------+
      |
      |
      v
    +-----------------------------------------+
    |                                         |
    | foo:                                    |
    |                                         |
    | Exit                                    |
    +-----------------------------------------+
    +-----------------------------------------+
    |                                         |
    | __L1:                                   |
    |                                         |
    | Exit                                    |
    +-----------------------------------------+
    |}];
  return ()
;;

let%expect_test "conditional branch have edges to targets" =
  let%bind () =
    eir {|
      .text
    main:
      jeq foo, A, 0
      putc A
    foo:
      exit
|}
    |> print_graph
  in
  [%expect
    {|
    +-------------------------------------------------------------------+
    |                                                                   |
    | main:                                                             |
    |                                                                   |
    | (Jump                                                             |
    | ((target (Label foo))                                             |
    | (cond (((cmp Eq) (left (Var (Register A))) (right (Const 0))))))) | -+
    +-------------------------------------------------------------------+  |
      |                                                                    |
      | false                                                              |
      v                                                                    |
    +-------------------------------------------------------------------+  |
    |                                                                   |  |
    | __L1:                                                             |  |
    |                                                                   |  | true
    | (Putc (Var (Register A)))                                         |  |
    +-------------------------------------------------------------------+  |
      |                                                                    |
      |                                                                    |
      v                                                                    |
    +-------------------------------------------------------------------+  |
    |                                                                   |  |
    | foo:                                                              |  |
    |                                                                   |  |
    | Exit                                                              | <+
    +-------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "data is segmented correctly" =
  eir
    {|
      .data
    foo:
      .long 0
      .long 1
    bar:
      .long 2
    baz:
      .long 3
      .long 4
|}
  |> print_data;
  [%expect
    {|
    (data
     ((_edata ((Label __reserved_heap_base))) (bar ((Const 2)))
      (baz ((Const 3) (Const 4))) (foo ((Const 0) (Const 1)))))
    |}];
  return ()
;;

let%expect_test "program with two top blocks" =
  let%bind () = eir {|
    .text
    mov A, B
    exit
    exit
|} |> print_graph in
  [%expect
    {|
    +--------------------------------------------------------+
    |                                                        |
    | __L0:                                                  |
    |                                                        |
    | (Assign ((dst (Register A)) (src (Var (Register B))))) |
    +--------------------------------------------------------+
      |
      |
      v
    +--------------------------------------------------------+
    |                                                        |
    | __L1:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    +--------------------------------------------------------+
    |                                                        |
    | __L2:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "program with self loop" =
  let%bind () = eir {|
    .text
  main:
    jmp main
|} |> print_graph in
  [%expect
    {|
    +------------------------------------------+
    |                                          |
    | main:                                    | ---+
    |                                          |    |
    | (Jump ((target (Label main)) (cond ()))) | <--+
    +------------------------------------------+
    |}];
  return ()
;;

let%expect_test "program with duplicate labels" =
  let%bind () = eir {|
    .text
  aaa:
  main:
      exit
|} |> print_graph in
  [%expect
    {|
    +-------+
    |       |
    | main: |
    |       |
    | Exit  |
    +-------+
    |}];
  return ()
;;
