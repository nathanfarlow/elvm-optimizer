open Core
open Elvm_opt
module B = Block.M

let print_refs refs = print_s [%sexp (refs : string list)]

let of_statements statements =
  B.{ label = "foo"; statements; in_edges = []; branch = None }

let%expect_test "references returns statement references" =
  of_statements
    [
      Assign { dst = Memory (Label "foo"); src = Label "bar" };
      Assign { dst = Memory (Label "baz"); src = Label "baq" };
    ]
  |> B.references |> print_refs;
  [%expect {| (baq bar baz foo) |}]

let%expect_test "references removes duplicate references" =
  of_statements [ Assign { dst = Memory (Label "foo"); src = Label "foo" } ]
  |> B.references |> print_refs;
  [%expect {| (foo) |}]