open Core
open Elvm

let eir insns labels data =
  let labels = Hashtbl.of_alist_exn (module String) labels in
  Eir.create ~insns ~labels ~data |> Lift.f |> [%sexp_of: Program.t] |> print_s
;;

let%expect_test "no insns is empty list" =
  eir [] [] [];
  [%expect
    {| ((graph ((nodes ()))) (data (((label __reserved_heap_base) (type_ Heap))))) |}]
;;

let%expect_test "mov is lifted correctly" =
  eir [ Mov { dst = A; src = Register A } ] [] [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v ((Assign ((dst (Register A)) (src (Var (Register A))))))) (in_ ())
           (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "add is lifted correctly" =
  eir [ Add { dst = A; src = Label "label" } ] [] [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v
            ((Assign
              ((dst (Register A)) (src (Add ((Var (Register A)) (Label label))))))))
           (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "sub is lifted correctly" =
  eir [ Sub { dst = A; src = Int 5 } ] [] [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v
            ((Assign
              ((dst (Register A)) (src (Sub (Var (Register A)) (Const 5)))))))
           (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "load is lifted correctly" =
  eir [ Load { dst = A; src = Register B } ] [] [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v
            ((Assign
              ((dst (Register A)) (src (Var (Memory (Var (Register B)))))))))
           (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "store is lifted correctly" =
  eir [ Store { dst = Register A; src = B } ] [] [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v
            ((Assign
              ((dst (Memory (Var (Register A)))) (src (Var (Register B)))))))
           (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "putc is lifted correctly" =
  eir [ Putc (Register A) ] [] [];
  [%expect
    {|
    ((graph
      ((nodes ((__L0 ((v ((Putc (Var (Register A))))) (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "getc is lifted correctly" =
  eir [ Getc A ] [] [];
  [%expect
    {|
    ((graph ((nodes ((__L0 ((v ((Getc (Register A)))) (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "exit is lifted correctly" =
  eir [ Exit ] [] [];
  [%expect
    {|
    ((graph ((nodes ((__L0 ((v (Exit)) (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "jump is lifted correctly" =
  eir [ Jump { target = Register A; cond = None } ] [] [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v ((Jump ((target (Var (Register A))) (cond ()))))) (in_ ())
           (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "set is lifted correctly" =
  eir [ Set { cmp = Eq; args = { dst = B; src = Register A } } ] [] [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v
            ((Assign
              ((dst (Register B))
               (src
                (If
                 ((cmp Eq) (left (Var (Register B))) (right (Var (Register A))))))))))
           (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "dump is lifted correctly to nop" =
  eir [ Dump ] [] [];
  [%expect
    {|
    ((graph ((nodes ((__L0 ((v (Nop)) (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "many text labels are eliminated" =
  let address = Eir.Address.{ segment = Text; offset = 0 } in
  let labels = [ "foo", address; "baz", address; "bar", address ] in
  let insns = [ Eir.Instruction.Exit ] in
  eir insns labels [];
  [%expect
    {|
    ((graph ((nodes ((__L0 ((v (Exit)) (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "main is not clobbered" =
  let labels = [ ("main", Eir.Address.{ segment = Text; offset = 0 }) ] in
  let insns = [ Eir.Instruction.Exit ] in
  eir insns labels [];
  [%expect
    {|
    ((graph ((nodes ((main ((v (Exit)) (in_ ()) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "fallthrough branches are added for each instruction" =
  let insns = [ Eir.Instruction.Mov { dst = A; src = Register A }; Exit ] in
  eir insns [] [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v ((Assign ((dst (Register A)) (src (Var (Register A))))))) (in_ ())
           (out ((Unconditional __L1)))))
         (__L1 ((v (Exit)) (in_ (__L0)) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "unconditional branch has edge to target" =
  let labels = [ ("foo", Eir.Address.{ segment = Text; offset = 1 }) ] in
  let insns = [ Eir.Instruction.Jump { target = Label "foo"; cond = None }; Exit ] in
  eir insns labels [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v ((Jump ((target (Label __L1)) (cond ()))))) (in_ ())
           (out ((Unconditional __L1)))))
         (__L1 ((v (Exit)) (in_ (__L0)) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
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
  eir insns labels [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v
            ((Jump
              ((target (Label __L2))
               (cond
                (((cmp Eq) (left (Var (Register A))) (right (Var (Register B))))))))))
           (in_ ()) (out ((Conditional (true_ __L2) (false_ __L1))))))
         (__L1
          ((v ((Putc (Var (Register A))))) (in_ (__L0))
           (out ((Unconditional __L2)))))
         (__L2 ((v (Exit)) (in_ (__L1 __L0)) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "data references are updated for label rewrite" =
  let labels = [ ("foo", Eir.Address.{ segment = Text; offset = 0 }) ] in
  let insns = [ Eir.Instruction.Exit ] in
  let data = [ Eir.Data.Label "foo" ] in
  eir insns labels data;
  [%expect
    {|
    ((graph ((nodes ((__L0 ((v (Exit)) (in_ ()) (out ())))))))
     (data
      (((label __reserved_heap_base) (type_ Heap))
       ((label __D0) (type_ (Chunk ((Label __L0))))))))
    |}]
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
  eir insns labels data;
  [%expect
    {|
    ((graph ((nodes ((__L0 ((v (Exit)) (in_ ()) (out ())))))))
     (data
      (((label __reserved_heap_base) (type_ Heap))
       ((label foo) (type_ (Chunk ((Const 0) (Const 1)))))
       ((label bar) (type_ (Chunk ((Const 2)))))
       ((label qux) (type_ (Chunk ((Const 3) (Const 4))))))))
    |}]
;;

let%expect_test "text references are updated for label rewrite" =
  let labels =
    [ ("a", Eir.Address.{ segment = Data; offset = 0 })
    ; "b", { segment = Data; offset = 0 }
    ]
  in
  let insns = [ Eir.Instruction.Mov { dst = A; src = Label "b" } ] in
  let data = [ Eir.Data.Const 0 ] in
  eir insns labels data;
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v ((Assign ((dst (Register A)) (src (Label a)))))) (in_ ()) (out ())))))))
     (data
      (((label __reserved_heap_base) (type_ Heap))
       ((label a) (type_ (Chunk ((Const 0))))))))
    |}]
;;

let%expect_test "program with no data with data labels has just heap" =
  let labels = [ ("a", Eir.Address.{ segment = Data; offset = 0 }) ] in
  eir [] labels [];
  [%expect
    {| ((graph ((nodes ()))) (data (((label __reserved_heap_base) (type_ Heap))))) |}]
;;

let%expect_test "program with no insns with text labels has just heap" =
  let labels = [ ("a", Eir.Address.{ segment = Text; offset = 0 }) ] in
  eir [] labels [];
  [%expect
    {| ((graph ((nodes ()))) (data (((label __reserved_heap_base) (type_ Heap))))) |}]
;;

let%expect_test "program with two top blocks" =
  let labels = [ ("a", Eir.Address.{ segment = Text; offset = 1 }) ] in
  let insns = [ Eir.Instruction.Exit; Mov { dst = A; src = Register B }; Dump ] in
  eir insns labels [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0 ((v (Exit)) (in_ ()) (out ())))
         (__L1
          ((v ((Assign ((dst (Register A)) (src (Var (Register B))))))) (in_ ())
           (out ((Unconditional __L2)))))
         (__L2 ((v (Nop)) (in_ (__L1)) (out ())))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;

let%expect_test "program with self loop" =
  let labels = [ ("a", Eir.Address.{ segment = Text; offset = 0 }) ] in
  let insns = [ Eir.Instruction.Jump { target = Label "a"; cond = None } ] in
  eir insns labels [];
  [%expect
    {|
    ((graph
      ((nodes
        ((__L0
          ((v ((Jump ((target (Label __L0)) (cond ()))))) (in_ (__L0))
           (out ((Unconditional __L0)))))))))
     (data (((label __reserved_heap_base) (type_ Heap)))))
    |}]
;;
