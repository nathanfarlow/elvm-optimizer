open Core
open Async
open Elvm

let eir ?(insns = []) ?(data = []) ?(labels = []) () =
  let labels = Hashtbl.of_alist_exn (module String) labels in
  Eir.create ~insns ~labels ~data |> Lift.f
;;

let print_graph (graph, _) =
  let to_string stmt = [%sexp (stmt : Ast.Statement.t)] |> Sexp.to_string_hum ~indent:2 in
  Graph_util.to_ascii graph to_string >>| print_endline
;;

let print_data (_, data) = [%message (data : Eir.Data.t list Map.M(String).t)] |> print_s

let print (graph, data) =
  print_data (graph, data);
  print_graph (graph, data)
;;

let%expect_test "no insns is empty list" =
  let%bind () = eir ~insns:[] ~data:[] ~labels:[] () |> print in
  [%expect {| (data ()) |}];
  return ()
;;

let main = "main", Eir.Address.{ segment = Text; offset = 0 }

let%expect_test "mov is lifted correctly" =
  let%bind () =
    eir ~insns:[ Mov { dst = A; src = Register A }; Exit ] ~labels:[ main ] ()
    |> print_graph
  in
  [%expect
    {|
    +--------------------------------------------------------+
    |                                                        |
    | main:                                                  |
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
  let%bind () =
    eir ~insns:[ Add { dst = A; src = Label "label" }; Exit ] ~labels:[ main ] ()
    |> print_graph
  in
  [%expect
    {|
    +------------------------------------------------------------------------------+
    |                                                                              |
    | main:                                                                        |
    |                                                                              |
    | (Assign ((dst (Register A)) (src (Add ((Var (Register A)) (Label label)))))) |
    +------------------------------------------------------------------------------+
      |
      |
      v
    +------------------------------------------------------------------------------+
    |                                                                              |
    | __L1:                                                                        |
    |                                                                              |
    | Exit                                                                         |
    +------------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "sub is lifted correctly" =
  let%bind () =
    eir ~insns:[ Sub { dst = A; src = Int 5 }; Exit ] ~labels:[ main ] () |> print_graph
  in
  [%expect
    {|
    +------------------------------------------------------------------------+
    |                                                                        |
    | main:                                                                  |
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
  let%bind () =
    eir ~insns:[ Load { dst = A; src = Register B }; Exit ] ~labels:[ main ] ()
    |> print_graph
  in
  [%expect
    {|
    +-----------------------------------------------------------------------+
    |                                                                       |
    | main:                                                                 |
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
  let%bind () =
    eir ~insns:[ Store { dst = Register A; src = B }; Exit ] ~labels:[ main ] ()
    |> print_graph
  in
  [%expect
    {|
    +-----------------------------------------------------------------------+
    |                                                                       |
    | main:                                                                 |
    |                                                                       |
    | (Assign ((dst (Memory (Var (Register A)))) (src (Var (Register B))))) |
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
  let%bind () =
    eir ~insns:[ Putc (Register A); Exit ] ~labels:[ main ] () |> print_graph
  in
  [%expect
    {|
    +---------------------------+
    |                           |
    | main:                     |
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
  let%bind () = eir ~insns:[ Getc A; Exit ] ~labels:[ main ] () |> print_graph in
  [%expect
    {|
    +---------------------+
    |                     |
    | main:               |
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
  let%bind () = eir ~insns:[ Exit ] ~labels:[ main ] () |> print_graph in
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

let%expect_test "jump is lifted correctly" =
  let%bind () =
    eir ~insns:[ Jump { target = Register A; cond = None }; Exit ] ~labels:[ main ] ()
    |> print_graph
  in
  [%expect
    {|
    +------------------------------------------------+
    |                                                |
    | main:                                          |
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
  let%bind () =
    eir
      ~insns:[ Set { cmp = Eq; args = { dst = B; src = Register A } }; Exit ]
      ~labels:[ main ]
      ()
    |> print_graph
  in
  [%expect
    {|
    +-------------------------------------------------------------------------+
    |                                                                         |
    | main:                                                                   |
    |                                                                         |
    | (Assign                                                                 |
    | ((dst (Register B))                                                     |
    | (src                                                                    |
    | (If ((cmp Eq) (left (Var (Register B))) (right (Var (Register A)))))))) |
    +-------------------------------------------------------------------------+
      |
      |
      v
    +-------------------------------------------------------------------------+
    |                                                                         |
    | __L1:                                                                   |
    |                                                                         |
    | Exit                                                                    |
    +-------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "dump is lifted correctly to nop" =
  let%bind () = eir ~insns:[ Dump; Exit ] ~labels:[ main ] () |> print_graph in
  [%expect
    {|
    +-------+
    |       |
    | main: |
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
    eir
      ~insns:[ Eir.Instruction.Mov { dst = A; src = Register A }; Exit; Exit; Exit ]
      ~labels:[ main; ("foo", Eir.Address.{ segment = Text; offset = 2 }) ]
      ()
    |> print_graph
  in
  [%expect
    {|
    +--------------------------------------------------------+
    |                                                        |
    | __L3:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    +--------------------------------------------------------+
    |                                                        |
    | main:                                                  |
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
    | foo:                                                   |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "unconditional branch has edge to target" =
  let labels = [ main; ("foo", Eir.Address.{ segment = Text; offset = 1 }) ] in
  let insns = [ Eir.Instruction.Jump { target = Label "foo"; cond = None }; Exit ] in
  let%bind () = eir ~insns ~labels () |> print_graph in
  [%expect
    {|
    +-----------------------------------------+
    |                                         |
    | main:                                   |
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
    |}];
  return ()
;;

let%expect_test "conditional branch have edges to targets" =
  let labels = [ main; ("foo", Eir.Address.{ segment = Text; offset = 2 }) ] in
  let insns =
    [ Eir.Instruction.Jump
        { target = Label "foo"
        ; cond = Some { cmp = Eq; args = { dst = A; src = Register B } }
        }
    ; Putc (Register A)
    ; Exit
    ]
  in
  let%bind () = eir ~insns ~labels () |> print_graph in
  [%expect
    {|
    +----------------------------------------------------------------------------+
    |                                                                            |
    | main:                                                                      |
    |                                                                            |
    | (Jump                                                                      |
    | ((target (Label foo))                                                      |
    | (cond (((cmp Eq) (left (Var (Register A))) (right (Var (Register B)))))))) | -+
    +----------------------------------------------------------------------------+  |
      |                                                                             |
      | false                                                                       |
      v                                                                             |
    +----------------------------------------------------------------------------+  |
    |                                                                            |  |
    | __L1:                                                                      |  |
    |                                                                            |  | true
    | (Putc (Var (Register A)))                                                  |  |
    +----------------------------------------------------------------------------+  |
      |                                                                             |
      |                                                                             |
      v                                                                             |
    +----------------------------------------------------------------------------+  |
    |                                                                            |  |
    | foo:                                                                       |  |
    |                                                                            |  |
    | Exit                                                                       | <+
    +----------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "data is segmented correctly" =
  let labels =
    [ ("foo", Eir.Address.{ segment = Data; offset = 0 })
    ; "bar", { segment = Data; offset = 2 }
    ; "baz", { segment = Data; offset = 3 }
    ]
  in
  let data = [ Eir.Data.Const 0; Const 1; Const 2; Const 3; Const 4 ] in
  let%bind () = eir ~labels ~data () |> print in
  [%expect
    {|
    (data
     ((bar ((Const 2))) (baz ((Const 3) (Const 4))) (foo ((Const 0) (Const 1)))))
    |}];
  return ()
;;

let%expect_test "program with two top blocks" =
  let labels = [ main; ("a", Eir.Address.{ segment = Text; offset = 1 }) ] in
  let insns = [ Eir.Instruction.Exit; Mov { dst = A; src = Register B }; Exit ] in
  let%bind () = eir ~insns ~labels () |> print_graph in
  [%expect {|
    +--------------------------------------------------------+
    |                                                        |
    | a:                                                     |
    |                                                        |
    | (Assign ((dst (Register A)) (src (Var (Register B))))) |
    +--------------------------------------------------------+
      |
      |
      v
    +--------------------------------------------------------+
    |                                                        |
    | __L2:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    +--------------------------------------------------------+
    |                                                        |
    | main:                                                  |
    |                                                        |
    | Exit                                                   |
    +--------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "program with self loop" =
  let labels = [ main ] in
  let insns = [ Eir.Instruction.Jump { target = Label "main"; cond = None } ] in
  let%bind () = eir ~insns ~labels () |> print_graph in
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
