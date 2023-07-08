open Core
open Elvm.Expression
open Elvm.Statement

let print refs =
  let sorted = Hash_set.to_list refs |> List.sort ~compare:String.compare in
  print_s [%sexp (sorted : string list)]

let%expect_test "assign has references" =
  references (Assign { dst = Memory (Label "foo"); src = Label "bar" }) |> print;
  [%expect {| (bar foo) |}]

let%expect_test "putc has references" =
  references (Putc (Label "foo")) |> print;
  [%expect {| (foo) |}]

let%expect_test "jump has references" =
  references
    (Jump
       {
         target = Label "foo";
         condition =
           Some { comparison = Eq; left = Label "bar"; right = Label "baz" };
       })
  |> print;
  [%expect {| (bar baz foo) |}]

let%expect_test "exit has no references" =
  references Exit |> print;
  [%expect {| () |}]

let%expect_test "nop has no references" =
  references Nop |> print;
  [%expect {| () |}]

let%expect_test "duplicate labels are removed" =
  references (Assign { dst = Memory (Label "foo"); src = Label "foo" }) |> print;
  [%expect {| (foo) |}]
