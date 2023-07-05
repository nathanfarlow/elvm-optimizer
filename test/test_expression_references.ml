open Core
open Elvm_opt.Expression

let print refs = print_s [%sexp (refs : string list)]

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
  references (Set { comparison = Eq; a = Label "foo"; b = Label "bar" })
  |> print;
  [%expect {| (bar foo) |}]

let%expect_test "duplicate labels are removed" =
  references (Add [ Label "foo"; Label "foo" ]) |> print;
  [%expect {| (foo) |}]