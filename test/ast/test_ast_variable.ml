open Elvm

let print var =
  Ast.Variable.sexp_of_t var |> Sexp.to_string_hum |> print_endline

let%expect_test "substitute register entirely" =
  let var = Ast.Variable.Register A in
  let from = Ast.Expression.Var (Register A) in
  let to_ = Ast.Expression.Var (Register B) in
  let var, changed = Ast.Variable.substitute var ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print var;
  [%expect {| (Register B) |}]

let%expect_test "don't substitute register if not match" =
  let var = Ast.Variable.Register A in
  let from = Ast.Expression.Var (Register B) in
  let to_ = Ast.Expression.Var (Register C) in
  let var, changed = Ast.Variable.substitute var ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print var;
  [%expect {| (Register A) |}]

let%expect_test "substitute within memory" =
  let var = Ast.Variable.Memory (Const 0) in
  let from = Ast.Expression.Const 0 in
  let to_ = Ast.Expression.Const 1 in
  let var, changed = Ast.Variable.substitute var ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print var;
  [%expect {| (Memory (Const 1)) |}]

let%expect_test "substitute memory entirely" =
  let var = Ast.Variable.Memory (Const 0) in
  let from = Ast.Expression.Var var in
  let to_ = Ast.Expression.Var (Register B) in
  let var, changed = Ast.Variable.substitute var ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print var;
  [%expect {| (Register B) |}]