open Core
open Async
open Elvm

let eir insns labels data =
  let labels = Hashtbl.of_alist_exn (module String) labels in
  let program = Eir.create ~insns ~labels ~data |> Lift.f in
  [%message (program.data : Program.Data.t list)] |> print_s;
  let to_string stmts =
    [%sexp (stmts : Ast.Statement.t list)] |> Sexp.to_string_hum ~indent:2
  in
  Graph_util.to_ascii program.graph to_string >>| print_endline
;;

let%expect_test "no insns is empty list" =
  let%bind () = eir [] [] [] in
  [%expect {| (program.data (((label __reserved_heap_base) (type_ Heap)))) |}];
  return ()
;;

let%expect_test "mov is lifted correctly" =
  let%bind () = eir [ Mov { dst = A; src = Register A } ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +----------------------------------------------------------+
    |                                                          |
    | __L0:                                                    |
    |                                                          |
    | ((Assign ((dst (Register A)) (src (Var (Register A)))))) |
    +----------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "add is lifted correctly" =
  let%bind () = eir [ Add { dst = A; src = Label "label" } ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------------------------------------------------------------------------------+
    |                                                                                |
    | __L0:                                                                          |
    |                                                                                |
    | ((Assign ((dst (Register A)) (src (Add ((Var (Register A)) (Label label))))))) |
    +--------------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "sub is lifted correctly" =
  let%bind () = eir [ Sub { dst = A; src = Int 5 } ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------------------------------------------------------------------------+
    |                                                                          |
    | __L0:                                                                    |
    |                                                                          |
    | ((Assign ((dst (Register A)) (src (Sub (Var (Register A)) (Const 5)))))) |
    +--------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "load is lifted correctly" =
  let%bind () = eir [ Load { dst = A; src = Register B } ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +-------------------------------------------------------------------------+
    |                                                                         |
    | __L0:                                                                   |
    |                                                                         |
    | ((Assign ((dst (Register A)) (src (Var (Memory (Var (Register B)))))))) |
    +-------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "store is lifted correctly" =
  let%bind () = eir [ Store { dst = Register A; src = B } ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +-------------------------------------------------------------------------+
    |                                                                         |
    | __L0:                                                                   |
    |                                                                         |
    | ((Assign ((dst (Memory (Var (Register A)))) (src (Var (Register B)))))) |
    +-------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "putc is lifted correctly" =
  let%bind () = eir [ Putc (Register A) ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +-----------------------------+
    |                             |
    | __L0:                       |
    |                             |
    | ((Putc (Var (Register A)))) |
    +-----------------------------+
    |}];
  return ()
;;

let%expect_test "getc is lifted correctly" =
  let%bind () = eir [ Getc A ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +-----------------------+
    |                       |
    | __L0:                 |
    |                       |
    | ((Getc (Register A))) |
    +-----------------------+
    |}];
  return ()
;;

let%expect_test "exit is lifted correctly" =
  let%bind () = eir [ Exit ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------+
    |        |
    | __L0:  |
    |        |
    | (Exit) |
    +--------+
    |}];
  return ()
;;

let%expect_test "jump is lifted correctly" =
  let%bind () = eir [ Jump { target = Register A; cond = None } ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------------------------------------------------+
    |                                                  |
    | __L0:                                            |
    |                                                  |
    | ((Jump ((target (Var (Register A))) (cond ())))) |
    +--------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "set is lifted correctly" =
  let%bind () = eir [ Set { cmp = Eq; args = { dst = B; src = Register A } } ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------------------------------------------------------------------------+
    |                                                                          |
    | __L0:                                                                    |
    |                                                                          |
    | ((Assign                                                                 |
    | ((dst (Register B))                                                      |
    | (src                                                                     |
    | (If ((cmp Eq) (left (Var (Register B))) (right (Var (Register A))))))))) |
    +--------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "dump is lifted correctly to nop" =
  let%bind () = eir [ Dump ] [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +-------+
    |       |
    | __L0: |
    |       |
    | (Nop) |
    +-------+
    |}];
  return ()
;;

let%expect_test "many text labels are eliminated" =
  let address = Eir.Address.{ segment = Text; offset = 0 } in
  let labels = [ "foo", address; "baz", address; "bar", address ] in
  let insns = [ Eir.Instruction.Exit ] in
  let%bind () = eir insns labels [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------+
    |        |
    | __L0:  |
    |        |
    | (Exit) |
    +--------+
    |}];
  return ()
;;

let%expect_test "main is not clobbered" =
  let labels = [ ("main", Eir.Address.{ segment = Text; offset = 0 }) ] in
  let insns = [ Eir.Instruction.Exit ] in
  let%bind () = eir insns labels [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------+
    |        |
    | main:  |
    |        |
    | (Exit) |
    +--------+
    |}];
  return ()
;;

let%expect_test "fallthrough branches are added for each instruction" =
  let insns = [ Eir.Instruction.Mov { dst = A; src = Register A }; Exit ] in
  let%bind () = eir insns [] [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +----------------------------------------------------------+
    |                                                          |
    | __L0:                                                    |
    |                                                          |
    | ((Assign ((dst (Register A)) (src (Var (Register A)))))) |
    +----------------------------------------------------------+
      |
      |
      v
    +----------------------------------------------------------+
    |                                                          |
    | __L1:                                                    |
    |                                                          |
    | (Exit)                                                   |
    +----------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "unconditional branch has edge to target" =
  let labels = [ ("foo", Eir.Address.{ segment = Text; offset = 1 }) ] in
  let insns = [ Eir.Instruction.Jump { target = Label "foo"; cond = None }; Exit ] in
  let%bind () = eir insns labels [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------------------------------------------+
    |                                            |
    | __L0:                                      |
    |                                            |
    | ((Jump ((target (Label __L1)) (cond ())))) |
    +--------------------------------------------+
      |
      |
      v
    +--------------------------------------------+
    |                                            |
    | __L1:                                      |
    |                                            |
    | (Exit)                                     |
    +--------------------------------------------+
    |}];
  return ()
;;

let%expect_test "conditional branch have edges to targets" =
  let labels = [ ("foo", Eir.Address.{ segment = Text; offset = 2 }) ] in
  let insns =
    [ Eir.Instruction.Jump
        { target = Label "foo"
        ; cond = Some { cmp = Eq; args = { dst = A; src = Register B } }
        }
    ; Putc (Register A)
    ; Exit
    ]
  in
  let%bind () = eir insns labels [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +-----------------------------------------------------------------------------+
    |                                                                             |
    | __L0:                                                                       |
    |                                                                             |
    | ((Jump                                                                      |
    | ((target (Label __L2))                                                      |
    | (cond (((cmp Eq) (left (Var (Register A))) (right (Var (Register B))))))))) | -+
    +-----------------------------------------------------------------------------+  |
      |                                                                              |
      | false                                                                        |
      v                                                                              |
    +-----------------------------------------------------------------------------+  |
    |                                                                             |  |
    | __L1:                                                                       |  |
    |                                                                             |  | true
    | ((Putc (Var (Register A))))                                                 |  |
    +-----------------------------------------------------------------------------+  |
      |                                                                              |
      |                                                                              |
      v                                                                              |
    +-----------------------------------------------------------------------------+  |
    |                                                                             |  |
    | __L2:                                                                       |  |
    |                                                                             |  |
    | (Exit)                                                                      | <+
    +-----------------------------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "data references are updated for label rewrite" =
  let labels = [ ("foo", Eir.Address.{ segment = Text; offset = 0 }) ] in
  let insns = [ Eir.Instruction.Exit ] in
  let data = [ Eir.Data.Label "foo" ] in
  let%bind () = eir insns labels data in
  [%expect
    {|
    (program.data
     (((label __reserved_heap_base) (type_ Heap))
      ((label __D0) (type_ (Chunk ((Label __L0)))))))
    +--------+
    |        |
    | __L0:  |
    |        |
    | (Exit) |
    +--------+
    |}];
  return ()
;;

let%expect_test "data is segmented correctly" =
  let labels =
    [ ("foo", Eir.Address.{ segment = Data; offset = 0 })
    ; "bar", { segment = Data; offset = 2 }
    ; "baz", { segment = Data; offset = 2 }
    ; "qux", { segment = Data; offset = 3 }
    ]
  in
  let insns = [ Eir.Instruction.Exit ] in
  let data = [ Eir.Data.Const 0; Const 1; Const 2; Const 3; Const 4 ] in
  let%bind () = eir insns labels data in
  [%expect
    {|
    (program.data
     (((label __reserved_heap_base) (type_ Heap))
      ((label foo) (type_ (Chunk ((Const 0) (Const 1)))))
      ((label bar) (type_ (Chunk ((Const 2)))))
      ((label qux) (type_ (Chunk ((Const 3) (Const 4)))))))
    +--------+
    |        |
    | __L0:  |
    |        |
    | (Exit) |
    +--------+
    |}];
  return ()
;;

let%expect_test "text references are updated for label rewrite" =
  let labels =
    [ ("a", Eir.Address.{ segment = Data; offset = 0 })
    ; "b", { segment = Data; offset = 0 }
    ]
  in
  let insns = [ Eir.Instruction.Mov { dst = A; src = Label "b" } ] in
  let data = [ Eir.Data.Const 0 ] in
  let%bind () = eir insns labels data in
  [%expect
    {|
    (program.data
     (((label __reserved_heap_base) (type_ Heap))
      ((label a) (type_ (Chunk ((Const 0)))))))
    +-------------------------------------------------+
    |                                                 |
    | __L0:                                           |
    |                                                 |
    | ((Assign ((dst (Register A)) (src (Label a))))) |
    +-------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "program with no data with data labels has just heap" =
  let labels = [ ("a", Eir.Address.{ segment = Data; offset = 0 }) ] in
  let%bind () = eir [] labels [] in
  [%expect {| (program.data (((label __reserved_heap_base) (type_ Heap)))) |}];
  return ()
;;

let%expect_test "program with no insns with text labels has just heap" =
  let labels = [ ("a", Eir.Address.{ segment = Text; offset = 0 }) ] in
  let%bind () = eir [] labels [] in
  [%expect {| (program.data (((label __reserved_heap_base) (type_ Heap)))) |}];
  return ()
;;

let%expect_test "program with two top blocks" =
  let labels = [ ("a", Eir.Address.{ segment = Text; offset = 1 }) ] in
  let insns = [ Eir.Instruction.Exit; Mov { dst = A; src = Register B }; Dump ] in
  let%bind () = eir insns labels [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +----------------------------------------------------------+
    |                                                          |
    | __L0:                                                    |
    |                                                          |
    | (Exit)                                                   |
    +----------------------------------------------------------+
    +----------------------------------------------------------+
    |                                                          |
    | __L1:                                                    |
    |                                                          |
    | ((Assign ((dst (Register A)) (src (Var (Register B)))))) |
    +----------------------------------------------------------+
      |
      |
      v
    +----------------------------------------------------------+
    |                                                          |
    | __L2:                                                    |
    |                                                          |
    | (Nop)                                                    |
    +----------------------------------------------------------+
    |}];
  return ()
;;

let%expect_test "program with self loop" =
  let labels = [ ("a", Eir.Address.{ segment = Text; offset = 0 }) ] in
  let insns = [ Eir.Instruction.Jump { target = Label "a"; cond = None } ] in
  let%bind () = eir insns labels [] in
  [%expect
    {|
    (program.data (((label __reserved_heap_base) (type_ Heap))))
    +--------------------------------------------+
    |                                            |
    | __L0:                                      | ---+
    |                                            |    |
    | ((Jump ((target (Label __L0)) (cond ())))) | <--+
    +--------------------------------------------+
    |}];
  return ()
;;
