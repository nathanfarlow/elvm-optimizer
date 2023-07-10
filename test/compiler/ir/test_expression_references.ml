open Elvm.Compiler.Ir

let print refs =
  let sorted = Hash_set.to_list refs |> List.sort ~compare:String.compare in
  print_s [%sexp (sorted : string list)]

let%expect_test "label has reference" =
  Expression.references (Label "foo") |> print;
  [%expect {| (foo) |}]

let%expect_test "memory has references" =
  Expression.references (Var (Memory (Label "foo"))) |> print;
  [%expect {| (foo) |}]

let%expect_test "add has references" =
  Expression.references (Add [ Label "foo"; Label "bar" ]) |> print;
  [%expect {| (bar foo) |}]

let%expect_test "sub has references" =
  Expression.references (Sub (Label "foo", Label "bar")) |> print;
  [%expect {| (bar foo) |}]

let%expect_test "set has references" =
  Expression.references
    (If { cmp = Eq; left = Label "foo"; right = Label "bar" })
  |> print;
  [%expect {| (bar foo) |}]

let%expect_test "duplicate labels are removed" =
  Expression.references (Add [ Label "foo"; Label "foo" ]) |> print;
  [%expect {| (foo) |}]
