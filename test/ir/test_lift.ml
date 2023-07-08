open! Core
open Elvm
open Instruction
open Program

let print exp = print_s [%sexp (exp : Ir.t)]

let elvm instructions labels data =
  let labels = Hashtbl.of_alist_exn (module String) labels in
  Program.create ~instructions ~labels ~data

let ir instructions labels data = elvm instructions labels data |> Lift.f

let%expect_test "no instructions is empty list" =
  ir [] [] [] |> print;
  [%expect
    {| ((blocks ()) (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "mov is lifted correctly" =
  ir [ Mov { dst = A; src = Register A } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Register A))))))
        (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "add is lifted correctly" =
  ir [ Add { dst = A; src = Label "label" } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements
         ((Assign ((dst (Register A)) (src (Add ((Register A) (Label label))))))))
        (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "sub is lifted correctly" =
  ir [ Sub { dst = A; src = Int 5 } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements
         ((Assign ((dst (Register A)) (src (Sub (Register A) (Const 5)))))))
        (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "load is lifted correctly" =
  ir [ Load { dst = A; src = Register B } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Memory (Register B)))))))
        (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "store is lifted correctly" =
  ir [ Store { dst = Register A; src = B } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements ((Assign ((dst (Memory (Register A))) (src (Register B))))))
        (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "putc is lifted correctly" =
  ir [ Putc (Register A) ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0) (statements ((Putc (Register A)))) (in_edges ())
        (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "getc is lifted correctly" =
  ir [ Getc A ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0) (statements ((Assign ((dst (Register A)) (src Getc)))))
        (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "exit is lifted correctly" =
  ir [ Exit ] [] [] |> print;
  [%expect
    {|
    ((blocks (((label __L0) (statements (Exit)) (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "jump is lifted correctly" =
  ir [ Jump { target = Register A; condition = None } ] [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0) (statements ((Jump ((target (Register A)) (condition ())))))
        (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "set is lifted correctly" =
  ir [ Set { comparison = Eq; args = { dst = B; src = Register A } } ] [] []
  |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements
         ((Assign
           ((dst (Register B))
            (src
             (Set ((comparison Eq) (left (Register B)) (right (Register A)))))))))
        (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "dump is lifted correctly to nop" =
  ir [ Dump ] [] [] |> print;
  [%expect
    {|
    ((blocks (((label __L0) (statements (Nop)) (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "many text labels are eliminated" =
  let address = { segment = Text; offset = 0 } in
  let labels = [ ("foo", address); ("baz", address); ("bar", address) ] in
  let instructions = [ Exit ] in
  ir instructions labels [] |> print;
  [%expect
    {|
    ((blocks (((label __L0) (statements (Exit)) (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "main is not clobbered" =
  let labels = [ ("main", { segment = Text; offset = 0 }) ] in
  let instructions = [ Exit ] in
  ir instructions labels [] |> print;
  [%expect
    {|
    ((blocks (((label main) (statements (Exit)) (in_edges ()) (branch ()))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "fallthrough branches are added for each instruction" =
  let instructions = [ Mov { dst = A; src = Register A }; Exit ] in
  ir instructions [] [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements ((Assign ((dst (Register A)) (src (Register A))))))
        (in_edges ())
        (branch
         ((Fallthrough
           ((label __L1) (statements (Exit))
            (in_edges (((label __L0) (type_ Fallthrough)))) (branch ()))))))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "unconditional branch has edge to target" =
  let labels = [ ("foo", { segment = Text; offset = 1 }) ] in
  let instructions =
    [ Jump { target = Label "foo"; condition = None }; Exit ]
  in
  ir instructions labels [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0) (statements ((Jump ((target (Label __L1)) (condition ())))))
        (in_edges ())
        (branch
         ((Unconditional_jump
           ((label __L1) (statements (Exit))
            (in_edges (((label __L0) (type_ Jump)))) (branch ()))))))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "conditional branch have edges to targets" =
  let labels = [ ("foo", { segment = Text; offset = 2 }) ] in
  let instructions =
    [
      Jump
        {
          target = Label "foo";
          condition =
            Some { comparison = Eq; args = { dst = A; src = Register B } };
        };
      Putc (Register A);
      Exit;
    ]
  in
  ir instructions labels [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0)
        (statements
         ((Jump
           ((target (Label __L2))
            (condition
             (((comparison Eq) (left (Register A)) (right (Register B)))))))))
        (in_edges ())
        (branch
         ((Conditional_jump
           (true_
            ((label __L2) (statements (Exit))
             (in_edges
              (((label __L1) (type_ Fallthrough)) ((label __L0) (type_ Jump))))
             (branch ())))
           (false_
            ((label __L1) (statements ((Putc (Register A))))
             (in_edges (((label __L0) (type_ Fallthrough))))
             (branch
              ((Fallthrough
                ((label __L2) (statements (Exit))
                 (in_edges
                  (((label __L1) (type_ Fallthrough))
                   ((label __L0) (type_ Jump))))
                 (branch ())))))))))))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "data references are updated for label rewrite" =
  let labels = [ ("foo", { segment = Text; offset = 0 }) ] in
  let instructions = [ Exit ] in
  let data = [ Label "foo" ] in
  ir instructions labels data |> print;
  [%expect
    {|
    ((blocks (((label __L0) (statements (Exit)) (in_edges ()) (branch ()))))
     (data
      (((label __reserved_heap_base) (type_ Heap))
       ((label __D0) (type_ (Chunk ((Label __L0)))))))) |}]

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
  ir instructions labels data |> print;
  [%expect
    {|
    ((blocks (((label __L0) (statements (Exit)) (in_edges ()) (branch ()))))
     (data
      (((label __reserved_heap_base) (type_ Heap))
       ((label foo) (type_ (Chunk ((Const 0) (Const 1)))))
       ((label bar) (type_ (Chunk ((Const 2)))))
       ((label qux) (type_ (Chunk ((Const 3) (Const 4)))))))) |}]

let%expect_test "text references are updated for label rewrite" =
  let labels =
    [
      ("a", { segment = Data; offset = 0 });
      ("b", { segment = Data; offset = 0 });
    ]
  in
  let instructions = [ Mov { dst = A; src = Label "b" } ] in
  let data = [ Const 0 ] in
  ir instructions labels data |> print;
  [%expect
    {|
    ((blocks
      (((label __L0) (statements ((Assign ((dst (Register A)) (src (Label a))))))
        (in_edges ()) (branch ()))))
     (data
      (((label __reserved_heap_base) (type_ Heap))
       ((label a) (type_ (Chunk ((Const 0)))))))) |}]

let%expect_test "program with no data with data labels has just heap" =
  let labels = [ ("a", { segment = Data; offset = 0 }) ] in
  ir [] labels [] |> print;
  [%expect
    {| ((blocks ()) (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "program with no instructions with text labels has just heap" =
  let labels = [ ("a", { segment = Text; offset = 0 }) ] in
  ir [] labels [] |> print;
  [%expect
    {| ((blocks ()) (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "program with two top blocks" =
  let labels = [ ("a", { segment = Text; offset = 1 }) ] in
  let instructions = [ Exit; Mov { dst = A; src = Register B }; Dump ] in
  ir instructions labels [] |> print;
  [%expect
    {|
    ((blocks
      (((label __L0) (statements (Exit)) (in_edges ()) (branch ()))
       ((label __L1)
        (statements ((Assign ((dst (Register A)) (src (Register B))))))
        (in_edges ())
        (branch
         ((Fallthrough
           ((label __L2) (statements (Nop))
            (in_edges (((label __L1) (type_ Fallthrough)))) (branch ()))))))))
     (data (((label __reserved_heap_base) (type_ Heap))))) |}]

let%expect_test "program with self loop" =
  let labels = [ ("a", { segment = Text; offset = 0 }) ] in
  let instructions = [ Jump { target = Label "a"; condition = None } ] in
  let ir = ir instructions labels [] in
  printf "%d" (List.length @@ Ir.blocks ir);
  [%expect {| 1 |}];
  let block = List.hd_exn @@ Ir.blocks ir in
  printf "%s" block.label;
  [%expect {| __L0 |}];
  let target = block.branch in
  match target with
  | Some (Unconditional_jump target) ->
      printf "%s" target.label;
      [%expect {| __L0 |}]
  | _ -> assert false