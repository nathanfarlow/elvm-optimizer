open Core
open Elvm_opt
open Instruction
open Program

let print exp = print_s [%sexp (exp : Ir.t)]

let elvm instructions labels data =
  let labels = Hashtbl.of_alist_exn (module String) labels in
  Program.{ instructions; labels; data }

let ir instructions labels data = elvm instructions labels data |> Ir.of_program

let%expect_test "no instructions is empty list" =
  ir [] [] [] |> print;
  [%expect {| () |}]

let%expect_test "mov is lifted correctly" =
  ir [ Mov { dst = A; src = Register A } ] [] [] |> print;
  [%expect
    {|
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Register A))))))
        (branch ()))) |}]

let%expect_test "add is lifted correctly" =
  ir [ Add { dst = A; src = Label "label" } ] [] [] |> print;
  [%expect
    {|
    (((label __L0)
      (statements
       ((Assign ((dst (Register A)) (src (Add ((Register A) (Label label))))))))
      (branch ()))) |}]

let%expect_test "sub is lifted correctly" =
  ir [ Sub { dst = A; src = Int 5 } ] [] [] |> print;
  [%expect
    {|
      (((label __L0)
        (statements
         ((Assign ((dst (Register A)) (src (Sub (Register A) (Const 5)))))))
        (branch ()))) |}]

let%expect_test "load is lifted correctly" =
  ir [ Load { dst = A; src = Register B } ] [] [] |> print;
  [%expect
    {|
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Memory (Register B)))))))
        (branch ()))) |}]

let%expect_test "store is lifted correctly" =
  ir [ Store { dst = Register A; src = B } ] [] [] |> print;
  [%expect
    {|
      (((label __L0)
        (statements ((Assign ((dst (Memory (Register A))) (src (Register B))))))
        (branch ()))) |}]

let%expect_test "putc is lifted correctly" =
  ir [ Putc (Register A) ] [] [] |> print;
  [%expect
    {| (((label __L0) (statements ((Putc (Register A)))) (branch ()))) |}]

let%expect_test "getc is lifted correctly" =
  ir [ Getc A ] [] [] |> print;
  [%expect
    {|
        (((label __L0) (statements ((Assign ((dst (Register A)) (src Getc)))))
          (branch ()))) |}]

let%expect_test "exit is lifted correctly" =
  ir [ Exit ] [] [] |> print;
  [%expect {| (((label __L0) (statements (Exit)) (branch ()))) |}]

let%expect_test "jump is lifted correctly" =
  ir [ Jump { target = Register A; condition = None } ] [] [] |> print;
  [%expect
    {|
      (((label __L0) (statements ((Jump ((target (Register A)) (condition ())))))
        (branch ()))) |}]

let%expect_test "set is lifted correctly" =
  ir [ Set { comparison = Eq; args = { dst = B; src = Register A } } ] [] []
  |> print;
  [%expect
    {|
      (((label __L0)
        (statements
         ((Assign
           ((dst (Register B))
            (src (Set ((comparison Eq) (a (Register B)) (b (Register A)))))))))
        (branch ()))) |}]

let%expect_test "many labels are eliminated" =
  let address = { segment = Text; offset = 0 } in
  let labels = [ ("foo", address); ("baz", address); ("bar", address) ] in
  let instructions = [ Exit ] in
  ir instructions labels [] |> print;
  [%expect {| (((label __L0) (statements (Exit)) (branch ()))) |}]

let%expect_test "main is not clobbered" =
  let labels = [ ("main", { segment = Text; offset = 0 }) ] in
  let instructions = [ Exit ] in
  ir instructions labels [] |> print;
  [%expect {| (((label main) (statements (Exit)) (branch ()))) |}]

let%expect_test "fallthrough branches are added for each instruction" =
  let instructions = [ Mov { dst = A; src = Register A }; Exit ] in
  ir instructions [] [] |> print;
  [%expect
    {|
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Register A))))))
        (branch
         (((primary ((label __L1) (statements (Exit)) (branch ()))) (secondary ())))))) |}]
