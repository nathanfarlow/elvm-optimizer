open Core
open Elvm_opt
open! Instruction
open! Program

let print exp = print_s [%sexp (exp : Ir.t)]

let elvm instructions labels data =
  let labels = Hashtbl.of_alist_exn (module String) labels in
  Program.{ instructions; labels; data }

let statements instructions labels data =
  elvm instructions labels data |> Ir.of_program

let%expect_test "no instructions is empty list" =
  statements [] [] [] |> print;
  [%expect
    {| ((blocks ()) (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "mov is lifted correctly" =
  statements [ Mov { dst = A; src = Register A } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Register A))))))
        (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "add is lifted correctly" =
  statements [ Add { dst = A; src = Label "label" } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements
         ((Assign ((dst (Register A)) (src (Add ((Register A) (Label label))))))))
        (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "sub is lifted correctly" =
  statements [ Sub { dst = A; src = Int 5 } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements
         ((Assign ((dst (Register A)) (src (Sub (Register A) (Const 5)))))))
        (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "load is lifted correctly" =
  statements [ Load { dst = A; src = Register B } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Memory (Register B)))))))
        (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "store is lifted correctly" =
  statements [ Store { dst = Register A; src = B } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements ((Assign ((dst (Memory (Register A))) (src (Register B))))))
        (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "putc is lifted correctly" =
  statements [ Putc (Register A) ] [] [] |> print;
  [%expect
    {|
    ((blocks (((label __L0) (statements ((Putc (Register A)))) (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "getc is lifted correctly" =
  statements [ Getc A ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0) (statements ((Assign ((dst (Register A)) (src Getc)))))
        (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "exit is lifted correctly" =
  statements [ Exit ] [] [] |> print;
  [%expect
    {|
      ((blocks (((label __L0) (statements (Exit)) (branch ()))))
       (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "jump is lifted correctly" =
  statements [ Jump { target = Register A; condition = None } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0) (statements ((Jump ((target (Register A)) (condition ())))))
        (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "set is lifted correctly" =
  statements
    [ Set { comparison = Eq; args = { dst = B; src = Register A } } ]
    [] []
  |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements
         ((Assign
           ((dst (Register B))
            (src (Set ((comparison Eq) (a (Register B)) (b (Register A)))))))))
        (branch ()))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "many labels are eliminated" =
  let address = { segment = Text; offset = 0 } in
  let labels = [ ("foo", address); ("baz", address); ("bar", address) ] in
  let instructions = [ Exit ] in
  statements instructions labels [] |> print;
  [%expect
    {|
      ((blocks (((label __L0) (statements (Exit)) (branch ()))))
       (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "main is not clobbered" =
  let labels = [ ("main", { segment = Text; offset = 0 }) ] in
  let instructions = [ Exit ] in
  statements instructions labels [] |> print;
  [%expect
    {|
      ((blocks (((label main) (statements (Exit)) (branch ()))))
       (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "fallthrough branches are added for each instruction" =
  let instructions = [ Mov { dst = A; src = Register A }; Exit ] in
  statements instructions [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Register A))))))
        (branch
         (((primary ((label __L1) (statements (Exit)) (branch ())))
           (secondary ())))))))
     (data (((label __reserved_heap_base) (data Heap))))) |}]

let%expect_test "data addresses are updated" =
  let labels = [ ("foo", { segment = Text; offset = 0 }) ] in
  let instructions = [ Exit ] in
  let data = [ Label "foo" ] in
  statements instructions labels data |> print;
  [%expect
    {|
      ((blocks (((label __L0) (statements (Exit)) (branch ()))))
       (data
        (((label __reserved_heap_base) (data Heap))
         ((label __D0) (data (Chunk ((Label __L0)))))))) |}]

let%expect_test "data is segmented correctly" =
  let labels =
    [
      ("foo", { segment = Data; offset = 0 });
      ("bar", { segment = Data; offset = 2 });
      ("baz", { segment = Data; offset = 2 });
      ("qux", { segment = Data; offset = 3 });
    ]
  in
  let instructions = [ Exit ] in
  let data = [ Const 0; Const 1; Const 2; Const 3; Const 4 ] in
  statements instructions labels data |> print;
  [%expect
    {|
    ((blocks (((label __L0) (statements (Exit)) (branch ()))))
     (data
      (((label __reserved_heap_base) (data Heap))
       ((label foo) (data (Chunk ((Const 0) (Const 1)))))
       ((label baz) (data (Chunk ((Const 2)))))
       ((label qux) (data (Chunk ((Const 3) (Const 4)))))))) |}]