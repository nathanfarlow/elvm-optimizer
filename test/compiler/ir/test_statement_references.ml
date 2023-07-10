open Elvm.Compiler.Ir

let print refs =
  let sorted = Hash_set.to_list refs |> List.sort ~compare:String.compare in
  print_s [%sexp (sorted : string list)]

let%expect_test "assign has references" =
  Statement.references
    (Assign { dst = Memory (Label "foo"); src = Label "bar" })
  |> print;
  [%expect {| (bar foo) |}]

let%expect_test "putc has references" =
  Statement.references (Putc (Label "foo")) |> print;
  [%expect {| (foo) |}]

let%expect_test "jump has references" =
  Statement.references
    (Jump
       {
         target = Label "foo";
         cond = Some { cmp = Eq; left = Label "bar"; right = Label "baz" };
       })
  |> print;
  [%expect {| (bar baz foo) |}]

let%expect_test "exit has no references" =
  Statement.references Exit |> print;
  [%expect {| () |}]

let%expect_test "nop has no references" =
  Statement.references Nop |> print;
  [%expect {| () |}]

let%expect_test "duplicate labels are removed" =
  Statement.references
    (Assign { dst = Memory (Label "foo"); src = Label "foo" })
  |> print;
  [%expect {| (foo) |}]

let%expect_test "push has references" =
  Statement.references (Push (Label "foo")) |> print;
  [%expect {| (foo) |}]

let%expect_test "pop has references" =
  Statement.references (Pop (Memory (Label "foo"))) |> print;
  [%expect {| (foo) |}]

let%expect_test "call has references" =
  Statement.references (Call { label = "foo"; args = [ Label "bar" ] }) |> print;
  [%expect {| (bar) |}]
