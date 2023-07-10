module Block = Elvm.Compiler.Ir.Block

let print_refs refs =
  let sorted = Hash_set.to_list refs |> List.sort ~compare:String.compare in
  print_s [%sexp (sorted : string list)]

let of_statements statements =
  Block.{ label = "foo"; statements; in_edges = []; branch = None }

let%expect_test "references returns statement references" =
  of_statements
    [|
      Assign { dst = Memory (Label "foo"); src = Label "bar" };
      Assign { dst = Memory (Label "baz"); src = Label "baq" };
    |]
  |> Block.references |> print_refs;
  [%expect {| (baq bar baz foo) |}]

let%expect_test "references removes duplicate references" =
  of_statements [| Assign { dst = Memory (Label "foo"); src = Label "foo" } |]
  |> Block.references |> print_refs;
  [%expect {| (foo) |}]