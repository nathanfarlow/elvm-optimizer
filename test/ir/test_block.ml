open Core
open Elvm_opt

let print_refs refs = print_s [%sexp (refs : string list)]

let of_statements statements =
  Block.M.{ label = "foo"; statements; in_edges = []; branch = None }

let%expect_test "references returns statement references" =
  of_statements
    [
      Assign { dst = Memory (Label "foo"); src = Label "bar" };
      Assign { dst = Memory (Label "baz"); src = Label "baq" };
    ]
  |> Block.references |> print_refs;
  [%expect {| (baq bar baz foo) |}]

let%expect_test "references removes duplicate references" =
  of_statements [ Assign { dst = Memory (Label "foo"); src = Label "foo" } ]
  |> Block.references |> print_refs;
  [%expect {| (foo) |}]