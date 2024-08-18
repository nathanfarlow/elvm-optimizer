open Core
open Async
open Elvm

let eir s = Eir.parse_exn s |> Lift.f |> fst |> Liveness.analyze

let print_graph graph =
  let to_string (liveness, stmt) =
    [%message (liveness : Liveness.t) (stmt : Ast.Statement.t)] |> Sexp.to_string_hum
  in
  Graph_util.to_ascii graph to_string >>| print_endline
;;

let%expect_test _ =
  let%bind () = eir {|
    .text
    mov A, B
    exit
    |} |> print_graph in
  [%expect
    {|
    +----------------------------------------------------------------+
    |                                                                |
    | __L0:                                                          |
    |                                                                |
    | ((liveness ((live_in ((Register B))) (live_out ())))           |
    | (stmt (Assign ((dst (Register A)) (src (Var (Register B))))))) |
    +----------------------------------------------------------------+
      |
      |
      v
    +----------------------------------------------------------------+
    |                                                                |
    | __L1:                                                          |
    |                                                                |
    | ((liveness ((live_in ()) (live_out ()))) (stmt Exit))          |
    +----------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test _ =
  let%bind () =
    eir {|
    .text
    mov A, 1
    mov B, 2
    mov C, A
    exit
    |} |> print_graph
  in
  [%expect
    {|
    +------------------------------------------------------------------+
    |                                                                  |
    | __L0:                                                            |
    |                                                                  |
    | ((liveness ((live_in ()) (live_out ((Register A)))))             |
    | (stmt (Assign ((dst (Register A)) (src (Const 1))))))            |
    +------------------------------------------------------------------+
      |
      |
      v
    +------------------------------------------------------------------+
    |                                                                  |
    | __L1:                                                            |
    |                                                                  |
    | ((liveness ((live_in ((Register A))) (live_out ((Register A))))) |
    | (stmt (Assign ((dst (Register B)) (src (Const 2))))))            |
    +------------------------------------------------------------------+
      |
      |
      v
    +------------------------------------------------------------------+
    |                                                                  |
    | __L2:                                                            |
    |                                                                  |
    | ((liveness ((live_in ((Register A))) (live_out ())))             |
    | (stmt (Assign ((dst (Register C)) (src (Var (Register A)))))))   |
    +------------------------------------------------------------------+
      |
      |
      v
    +------------------------------------------------------------------+
    |                                                                  |
    | __L3:                                                            |
    |                                                                  |
    | ((liveness ((live_in ()) (live_out ()))) (stmt Exit))            |
    +------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test _ =
  let%bind () =
    eir {|
    .text
    mov A, 1
    mov B, 2
    mov A, 2
    mov C, A
    exit
    |}
    |> print_graph
  in
  [%expect
    {|
    +----------------------------------------------------------------+
    |                                                                |
    | __L0:                                                          |
    |                                                                |
    | ((liveness ((live_in ()) (live_out ())))                       |
    | (stmt (Assign ((dst (Register A)) (src (Const 1))))))          |
    +----------------------------------------------------------------+
      |
      |
      v
    +----------------------------------------------------------------+
    |                                                                |
    | __L1:                                                          |
    |                                                                |
    | ((liveness ((live_in ()) (live_out ())))                       |
    | (stmt (Assign ((dst (Register B)) (src (Const 2))))))          |
    +----------------------------------------------------------------+
      |
      |
      v
    +----------------------------------------------------------------+
    |                                                                |
    | __L2:                                                          |
    |                                                                |
    | ((liveness ((live_in ()) (live_out ((Register A)))))           |
    | (stmt (Assign ((dst (Register A)) (src (Const 2))))))          |
    +----------------------------------------------------------------+
      |
      |
      v
    +----------------------------------------------------------------+
    |                                                                |
    | __L3:                                                          |
    |                                                                |
    | ((liveness ((live_in ((Register A))) (live_out ())))           |
    | (stmt (Assign ((dst (Register C)) (src (Var (Register A))))))) |
    +----------------------------------------------------------------+
      |
      |
      v
    +----------------------------------------------------------------+
    |                                                                |
    | __L4:                                                          |
    |                                                                |
    | ((liveness ((live_in ()) (live_out ()))) (stmt Exit))          |
    +----------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "cycle" =
  let%bind () =
    eir
      {|
    .text
main:
    mov C, A
    add A, 2
    mov C, A
    mov A, 1
    jmp main
    |}
    |> print_graph
  in
  [%expect
    {|
    +----------------------------------------------------------------------------+
    |                                                                            |
    | __L1:                                                                      |
    |                                                                            |
    | ((liveness ((live_in ()) (live_out ((Register A)))))                       |
    | (stmt                                                                      |
    | (Assign ((dst (Register A)) (src (Add ((Var (Register A)) (Const 2)))))))) | <+
    +----------------------------------------------------------------------------+  |
      |                                                                             |
      |                                                                             |
      v                                                                             |
    +----------------------------------------------------------------------------+  |
    |                                                                            |  |
    | __L2:                                                                      |  |
    |                                                                            |  |
    | ((liveness ((live_in ((Register A))) (live_out ())))                       |  |
    | (stmt (Assign ((dst (Register C)) (src (Var (Register A)))))))             |  |
    +----------------------------------------------------------------------------+  |
      |                                                                             |
      |                                                                             |
      v                                                                             |
    +----------------------------------------------------------------------------+  |
    |                                                                            |  |
    | __L3:                                                                      |  |
    |                                                                            |  |
    | ((liveness ((live_in ()) (live_out ((Register A)))))                       |  |
    | (stmt (Assign ((dst (Register A)) (src (Const 1))))))                      |  |
    +----------------------------------------------------------------------------+  |
      |                                                                             |
      |                                                                             |
      v                                                                             |
    +----------------------------------------------------------------------------+  |
    |                                                                            |  |
    | __L4:                                                                      |  |
    |                                                                            |  |
    | ((liveness ((live_in ((Register A))) (live_out ((Register A)))))           |  |
    | (stmt (Jump ((target (Label main)) (cond ())))))                           |  |
    +----------------------------------------------------------------------------+  |
      |                                                                             |
      |                                                                             |
      v                                                                             |
    +----------------------------------------------------------------------------+  |
    |                                                                            |  |
    | main:                                                                      |  |
    |                                                                            |  |
    | ((liveness ((live_in ((Register A))) (live_out ())))                       |  |
    | (stmt (Assign ((dst (Register C)) (src (Var (Register A)))))))             | -+
    +----------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "branch" =
  let%bind () =
    eir
      {|
    .text
    mov A, 1
    jeq true, A, 1
false:
    # don't use A, but use some B
    putc B
    exit
true:
    # use A
    putc A
    exit
    |}
    |> print_graph
  in
  [%expect
    {|
                                                                      +-------------------------------------------------------------------------------+
                                                                      |                                                                               |
                                                                      | __L0:                                                                         |
                                                                      |                                                                               |
                                                                      | ((liveness ((live_in ((Register B))) (live_out ((Register A) (Register B))))) |
                                                                      | (stmt (Assign ((dst (Register A)) (src (Const 1))))))                         |
                                                                      +-------------------------------------------------------------------------------+
                                                                        |
                                                                        |
                                                                        v
    +-------------------------------------------------------+         +-------------------------------------------------------------------------------+
    |                                                       |         |                                                                               |
    |                                                       |         | __L1:                                                                         |
    |                                                       |         |                                                                               |
    | true:                                                 |         | ((liveness                                                                    |
    |                                                       |         | ((live_in ((Register A) (Register B)))                                        |
    | ((liveness ((live_in ((Register A))) (live_out ())))  |         | (live_out ((Register A) (Register B)))))                                      |
    | (stmt (Putc (Var (Register A)))))                     |         | (stmt                                                                         |
    |                                                       |         | (Jump                                                                         |
    |                                                       |  true   | ((target (Label true))                                                        |
    |                                                       | <------ | (cond (((cmp Eq) (left (Var (Register A))) (right (Const 1)))))))))           |
    +-------------------------------------------------------+         +-------------------------------------------------------------------------------+
      |                                                                 |
      |                                                                 | false
      v                                                                 v
    +-------------------------------------------------------+         +-------------------------------------------------------------------------------+
    |                                                       |         |                                                                               |
    | __L5:                                                 |         | false:                                                                        |
    |                                                       |         |                                                                               |
    | ((liveness ((live_in ()) (live_out ()))) (stmt Exit)) |         | ((liveness ((live_in ((Register B))) (live_out ())))                          |
    |                                                       |         | (stmt (Putc (Var (Register B)))))                                             |
    +-------------------------------------------------------+         +-------------------------------------------------------------------------------+
                                                                        |
                                                                        |
                                                                        v
                                                                      +-------------------------------------------------------------------------------+
                                                                      |                                                                               |
                                                                      | __L3:                                                                         |
                                                                      |                                                                               |
                                                                      | ((liveness ((live_in ()) (live_out ()))) (stmt Exit))                         |
                                                                      +-------------------------------------------------------------------------------+
    |}];
  return ()
;;
