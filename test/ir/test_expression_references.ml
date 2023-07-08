open Core
open Elvm_opt.Expression

let print refs =
  let sorted = Hash_set.to_list refs |> List.sort ~compare:String.compare in
  print_s [%sexp (sorted : string list)]

let%expect_test "label has reference" =
  references (Label "foo") |> print;
  [%expect {| (foo) |}]

let%expect_test "memory has references" =
  references (Memory (Label "foo")) |> print;
  [%expect {| (foo) |}]

let%expect_test "add has references" =
  references (Add [ Label "foo"; Label "bar" ]) |> print;
  [%expect {| (bar foo) |}]

let%expect_test "sub has references" =
  references (Sub (Label "foo", Label "bar")) |> print;
  [%expect {| (bar foo) |}]

let%expect_test "set has references" =
  references (Set { comparison = Eq; left = Label "foo"; right = Label "bar" })
  |> print;
  [%expect {| (bar foo) |}]

let%expect_test "duplicate labels are removed" =
  references (Add [ Label "foo"; Label "foo" ]) |> print;
  [%expect {| (foo) |}]