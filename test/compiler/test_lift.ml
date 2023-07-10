open Elvm
open Compiler.Ir
open Eir.Instruction
open Eir.Address

let print program =
  let blocks =
    Program.blocks program |> Hashtbl.to_alist
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  print_s [%sexp (blocks : (string * Block.t) list)];
  print_s [%sexp (Program.data program : Program.Data.t list)]

let elvm insns labels data =
  let labels = Hashtbl.of_alist_exn (module String) labels in
  Eir.create ~insns ~labels ~data

let ir insns labels data = elvm insns labels data |> Compiler.lift

let%expect_test "no insns is empty list" =
  ir [] [] [] |> print;
  [%expect {|
      ()
      (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "mov is lifted correctly" =
  ir [ Mov { dst = A; src = Register A } ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0)
       (statements ((Assign ((dst (Named A)) (src (Var (Named A)))))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "add is lifted correctly" =
  ir [ Add { dst = A; src = Label "label" } ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0)
       (statements
        ((Assign ((dst (Named A)) (src (Add ((Var (Named A)) (Label label))))))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "sub is lifted correctly" =
  ir [ Sub { dst = A; src = Int 5 } ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0)
       (statements
        ((Assign ((dst (Named A)) (src (Sub (Var (Named A)) (Const 5)))))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "load is lifted correctly" =
  ir [ Load { dst = A; src = Register B } ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0)
       (statements
        ((Assign ((dst (Named A)) (src (Var (Memory (Var (Named B)))))))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "store is lifted correctly" =
  ir [ Store { dst = Register A; src = B } ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0)
       (statements
        ((Assign ((dst (Memory (Var (Named A)))) (src (Var (Named B)))))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "putc is lifted correctly" =
  ir [ Putc (Register A) ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0) (statements ((Putc (Var (Named A))))) (in_edges ())
       (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "getc is lifted correctly" =
  ir [ Getc A ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0) (statements ((Assign ((dst (Named A)) (src Getc)))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "exit is lifted correctly" =
  ir [ Exit ] [] [] |> print;
  [%expect
    {|
    ((__L0 ((label __L0) (statements (Exit)) (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "jump is lifted correctly" =
  ir [ Jump { target = Register A; cond = None } ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0) (statements ((Jump ((target (Var (Named A))) (cond ())))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "set is lifted correctly" =
  ir [ Set { cmp = Eq; args = { dst = B; src = Register A } } ] [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0)
       (statements
        ((Assign
          ((dst (Named B))
           (src (If ((cmp Eq) (left (Var (Named B))) (right (Var (Named A))))))))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "dump is lifted correctly to nop" =
  ir [ Dump ] [] [] |> print;
  [%expect
    {|
    ((__L0 ((label __L0) (statements (Nop)) (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "many text labels are eliminated" =
  let address = { segment = Text; offset = 0 } in
  let labels = [ ("foo", address); ("baz", address); ("bar", address) ] in
  let insns = [ Exit ] in
  ir insns labels [] |> print;
  [%expect
    {|
    ((__L0 ((label __L0) (statements (Exit)) (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "main is not clobbered" =
  let labels = [ ("main", { segment = Text; offset = 0 }) ] in
  let insns = [ Exit ] in
  ir insns labels [] |> print;
  [%expect
    {|
    ((main ((label main) (statements (Exit)) (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "fallthrough branches are added for each instruction" =
  let insns = [ Mov { dst = A; src = Register A }; Exit ] in
  ir insns [] [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0)
       (statements ((Assign ((dst (Named A)) (src (Var (Named A)))))))
       (in_edges ())
       (branch
        ((Fallthrough
          ((label __L1) (statements (Exit))
           (in_edges (((label __L0) (type_ Fallthrough)))) (branch ())))))))
     (__L1
      ((label __L1) (statements (Exit))
       (in_edges (((label __L0) (type_ Fallthrough)))) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "unconditional branch has edge to target" =
  let labels = [ ("foo", { segment = Text; offset = 1 }) ] in
  let insns = [ Jump { target = Label "foo"; cond = None }; Exit ] in
  ir insns labels [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0) (statements ((Jump ((target (Label __L1)) (cond ())))))
       (in_edges ())
       (branch
        ((Unconditional_jump
          ((label __L1) (statements (Exit))
           (in_edges (((label __L0) (type_ Jump)))) (branch ())))))))
     (__L1
      ((label __L1) (statements (Exit)) (in_edges (((label __L0) (type_ Jump))))
       (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "conditional branch have edges to targets" =
  let labels = [ ("foo", { segment = Text; offset = 2 }) ] in
  let insns =
    [
      Jump
        {
          target = Label "foo";
          cond = Some { cmp = Eq; args = { dst = A; src = Register B } };
        };
      Putc (Register A);
      Exit;
    ]
  in
  ir insns labels [] |> print;
  [%expect
    {|
    ((__L0
      ((label __L0)
       (statements
        ((Jump
          ((target (Label __L2))
           (cond (((cmp Eq) (left (Var (Named A))) (right (Var (Named B))))))))))
       (in_edges ())
       (branch
        ((Conditional_jump
          (true_
           ((label __L2) (statements (Exit))
            (in_edges
             (((label __L1) (type_ Fallthrough)) ((label __L0) (type_ Jump))))
            (branch ())))
          (false_
           ((label __L1) (statements ((Putc (Var (Named A)))))
            (in_edges (((label __L0) (type_ Fallthrough))))
            (branch
             ((Fallthrough
               ((label __L2) (statements (Exit))
                (in_edges
                 (((label __L1) (type_ Fallthrough)) ((label __L0) (type_ Jump))))
                (branch ()))))))))))))
     (__L1
      ((label __L1) (statements ((Putc (Var (Named A)))))
       (in_edges (((label __L0) (type_ Fallthrough))))
       (branch
        ((Fallthrough
          ((label __L2) (statements (Exit))
           (in_edges
            (((label __L1) (type_ Fallthrough)) ((label __L0) (type_ Jump))))
           (branch ())))))))
     (__L2
      ((label __L2) (statements (Exit))
       (in_edges
        (((label __L1) (type_ Fallthrough)) ((label __L0) (type_ Jump))))
       (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "data references are updated for label rewrite" =
  let labels = [ ("foo", { segment = Text; offset = 0 }) ] in
  let insns = [ Exit ] in
  let data = [ Eir.Data.Label "foo" ] in
  ir insns labels data |> print;
  [%expect
    {|
    ((__L0 ((label __L0) (statements (Exit)) (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))
     ((label __D0) (type_ (Chunk ((Label __L0)))))) |}]

let%expect_test "data is segmented correctly" =
  let labels =
    [
      ("foo", { segment = Data; offset = 0 });
      ("bar", { segment = Data; offset = 2 });
      ("baz", { segment = Data; offset = 2 });
      ("qux", { segment = Data; offset = 3 });
    ]
  in
  let insns = [ Exit ] in
  let data = [ Eir.Data.Const 0; Const 1; Const 2; Const 3; Const 4 ] in
  ir insns labels data |> print;
  [%expect
    {|
    ((__L0 ((label __L0) (statements (Exit)) (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))
     ((label foo) (type_ (Chunk ((Const 0) (Const 1)))))
     ((label bar) (type_ (Chunk ((Const 2)))))
     ((label qux) (type_ (Chunk ((Const 3) (Const 4)))))) |}]

let%expect_test "text references are updated for label rewrite" =
  let labels =
    [
      ("a", { segment = Data; offset = 0 });
      ("b", { segment = Data; offset = 0 });
    ]
  in
  let insns = [ Mov { dst = A; src = Label "b" } ] in
  let data = [ Eir.Data.Const 0 ] in
  ir insns labels data |> print;
  [%expect
    {|
    ((__L0
      ((label __L0) (statements ((Assign ((dst (Named A)) (src (Label a))))))
       (in_edges ()) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))
     ((label a) (type_ (Chunk ((Const 0)))))) |}]

let%expect_test "program with no data with data labels has just heap" =
  let labels = [ ("a", { segment = Data; offset = 0 }) ] in
  ir [] labels [] |> print;
  [%expect {|
      ()
      (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "program with no insns with text labels has just heap" =
  let labels = [ ("a", { segment = Text; offset = 0 }) ] in
  ir [] labels [] |> print;
  [%expect {|
      ()
      (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "program with two top blocks" =
  let labels = [ ("a", { segment = Text; offset = 1 }) ] in
  let insns = [ Exit; Mov { dst = A; src = Register B }; Dump ] in
  ir insns labels [] |> print;
  [%expect
    {|
    ((__L0 ((label __L0) (statements (Exit)) (in_edges ()) (branch ())))
     (__L1
      ((label __L1)
       (statements ((Assign ((dst (Named A)) (src (Var (Named B)))))))
       (in_edges ())
       (branch
        ((Fallthrough
          ((label __L2) (statements (Nop))
           (in_edges (((label __L1) (type_ Fallthrough)))) (branch ())))))))
     (__L2
      ((label __L2) (statements (Nop))
       (in_edges (((label __L1) (type_ Fallthrough)))) (branch ()))))
    (((label __reserved_heap_base) (type_ Heap))) |}]

let%expect_test "program with self loop" =
  let labels = [ ("a", { segment = Text; offset = 0 }) ] in
  let insns = [ Jump { target = Label "a"; cond = None } ] in
  let ir = ir insns labels [] in
  printf "%d" (Hashtbl.length @@ Program.blocks ir);
  [%expect {| 1 |}];
  let block = Hashtbl.find_exn (Program.blocks ir) "__L0" in
  printf "%s" block.label;
  [%expect {| __L0 |}];
  let target = block.branch in
  match target with
  | Some (Unconditional_jump target) ->
      printf "%s" target.label;
      [%expect {| __L0 |}]
  | _ -> assert false
