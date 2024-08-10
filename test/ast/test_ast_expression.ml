open Core
open Elvm

let print exp =
  Ast.Expression.sexp_of_t exp |> Sexp.to_string_hum |> print_endline

let%expect_test "substitute const" =
  let exp = Ast.Expression.Const 1 in
  let from = Ast.Expression.Const 1 in
  let to_ = Ast.Expression.Const 2 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 2) |}]

let%expect_test "substitute const not match" =
  let exp = Ast.Expression.Const 1 in
  let from = Ast.Expression.Const 2 in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Const 1) |}]

let%expect_test "substitute label" =
  let exp = Ast.Expression.Label "a" in
  let from = Ast.Expression.Label "a" in
  let to_ = Ast.Expression.Const 2 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 2) |}]

let%expect_test "substitute label not match" =
  let exp = Ast.Expression.Label "a" in
  let from = Ast.Expression.Label "b" in
  let to_ = Ast.Expression.Const 2 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Label a) |}]

let%expect_test "substitute var" =
  let exp = Ast.Expression.Var (Register A) in
  let from = Ast.Expression.Var (Register A) in
  let to_ = Ast.Expression.Const 2 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 2) |}]

let%expect_test "substitute var not match" =
  let exp = Ast.Expression.Var (Register A) in
  let from = Ast.Expression.Var (Register B) in
  let to_ = Ast.Expression.Const 2 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Var (Register A)) |}]

let%expect_test "substitute within add" =
  let exp = Ast.Expression.Add [ Const 1; Const 2 ] in
  let from = Ast.Expression.Const 1 in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Add ((Const 2) (Const 3))) |}]

let%expect_test "substitute within add not match" =
  let exp = Ast.Expression.Add [ Const 1; Const 2 ] in
  let from = Ast.Expression.Const 3 in
  let to_ = Ast.Expression.Const 4 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Add ((Const 1) (Const 2))) |}]

let%expect_test "substitute add entirely" =
  let exp = Ast.Expression.Add [ Const 1; Const 2 ] in
  let from = exp in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 3) |}]

let%expect_test "substitute within sub" =
  let exp = Ast.Expression.Sub (Const 1, Const 2) in
  let from = Ast.Expression.Const 1 in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Sub (Const 3) (Const 2)) |}]

let%expect_test "substitute within sub not match" =
  let exp = Ast.Expression.Sub (Const 1, Const 2) in
  let from = Ast.Expression.Const 3 in
  let to_ = Ast.Expression.Const 4 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Sub (Const 1) (Const 2)) |}]

let%expect_test "substitute sub entirely" =
  let exp = Ast.Expression.Sub (Const 1, Const 2) in
  let from = exp in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 3) |}]

let%expect_test "substitute getc" =
  let exp = Ast.Expression.Getc in
  let from = Ast.Expression.Getc in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 3) |}]

let%expect_test "substitute getc not match" =
  let exp = Ast.Expression.Getc in
  let from = Ast.Expression.Const 1 in
  let to_ = Ast.Expression.Const 2 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| Getc |}]

let%expect_test "substitute within if lhs" =
  let exp = Ast.Expression.If { cmp = Eq; left = Const 1; right = Const 2 } in
  let from = Ast.Expression.Const 1 in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (If ((cmp Eq) (left (Const 3)) (right (Const 2)))) |}]

let%expect_test "substitute within if rhs" =
  let exp = Ast.Expression.If { cmp = Eq; left = Const 1; right = Const 2 } in
  let from = Ast.Expression.Const 2 in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (If ((cmp Eq) (left (Const 1)) (right (Const 3)))) |}]

let%expect_test "substitute if entirely" =
  let exp = Ast.Expression.If { cmp = Eq; left = Const 1; right = Const 2 } in
  let from = exp in
  let to_ = Ast.Expression.Const 3 in
  let exp, changed = Ast.Expression.substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 3) |}]

let%expect_test "contains var in var expression" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.Var var in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| true |}]

let%expect_test "does not contain var in var expression" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.Var (Register B) in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| false |}]

let%expect_test "contains var in add expression" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.Add [ Const 1; Var var ] in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| true |}]

let%expect_test "does not contain var in add expression" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.Add [ Const 1; Var (Register B) ] in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| false |}]

let%expect_test "contains var in sub lhs" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.Sub (Var var, Const 1) in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| true |}]

let%expect_test "contains var in sub rhs" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.Sub (Const 1, Var var) in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| true |}]

let%expect_test "contains var in sub neither" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.Sub (Const 1, Const 2) in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| false |}]

let%expect_test "contains var in if lhs" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.If { cmp = Eq; left = Var var; right = Const 1 } in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| true |}]

let%expect_test "contains var in if rhs" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.If { cmp = Eq; left = Const 1; right = Var var } in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| true |}]

let%expect_test "contains var in if neither" =
  let var = Ast.Variable.Register A in
  let exp = Ast.Expression.If { cmp = Eq; left = Const 1; right = Const 2 } in
  let contains = Ast.Expression.contains exp var in
  printf "%b" contains;
  [%expect {| false |}]
