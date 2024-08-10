open Core
open Elvm
open Ast.Expression
module Var = Ast.Variable

let print exp = [%sexp (exp : t)] |> print_s

let%expect_test "substitute const" =
  let exp = Const 1 in
  let from = Const 1 in
  let to_ = Const 2 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 2) |}]
;;

let%expect_test "substitute const not match" =
  let exp = Const 1 in
  let from = Const 2 in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Const 1) |}]
;;

let%expect_test "substitute label" =
  let exp = Label "a" in
  let from = Label "a" in
  let to_ = Const 2 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 2) |}]
;;

let%expect_test "substitute label not match" =
  let exp = Label "a" in
  let from = Label "b" in
  let to_ = Const 2 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Label a) |}]
;;

let%expect_test "substitute var" =
  let exp = Var (Register A) in
  let from = Var (Register A) in
  let to_ = Const 2 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 2) |}]
;;

let%expect_test "substitute var not match" =
  let exp = Var (Register A) in
  let from = Var (Register B) in
  let to_ = Const 2 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Var (Register A)) |}]
;;

let%expect_test "substitute within add" =
  let exp = Add [ Const 1; Const 2 ] in
  let from = Const 1 in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Add ((Const 2) (Const 3))) |}]
;;

let%expect_test "substitute within add not match" =
  let exp = Add [ Const 1; Const 2 ] in
  let from = Const 3 in
  let to_ = Const 4 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Add ((Const 1) (Const 2))) |}]
;;

let%expect_test "substitute add entirely" =
  let exp = Add [ Const 1; Const 2 ] in
  let from = exp in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 3) |}]
;;

let%expect_test "substitute within sub" =
  let exp = Sub (Const 1, Const 2) in
  let from = Const 1 in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Sub (Const 3) (Const 2)) |}]
;;

let%expect_test "substitute within sub not match" =
  let exp = Sub (Const 1, Const 2) in
  let from = Const 3 in
  let to_ = Const 4 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| (Sub (Const 1) (Const 2)) |}]
;;

let%expect_test "substitute sub entirely" =
  let exp = Sub (Const 1, Const 2) in
  let from = exp in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 3) |}]
;;

let%expect_test "substitute getc" =
  let exp = Getc in
  let from = Getc in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 3) |}]
;;

let%expect_test "substitute getc not match" =
  let exp = Getc in
  let from = Const 1 in
  let to_ = Const 2 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| false |}];
  print exp;
  [%expect {| Getc |}]
;;

let%expect_test "substitute within if lhs" =
  let exp = If { cmp = Eq; left = Const 1; right = Const 2 } in
  let from = Const 1 in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (If ((cmp Eq) (left (Const 3)) (right (Const 2)))) |}]
;;

let%expect_test "substitute within if rhs" =
  let exp = If { cmp = Eq; left = Const 1; right = Const 2 } in
  let from = Const 2 in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (If ((cmp Eq) (left (Const 1)) (right (Const 3)))) |}]
;;

let%expect_test "substitute if entirely" =
  let exp = If { cmp = Eq; left = Const 1; right = Const 2 } in
  let from = exp in
  let to_ = Const 3 in
  let exp, changed = substitute exp ~from ~to_ in
  printf "%b" changed;
  [%expect {| true |}];
  print exp;
  [%expect {| (Const 3) |}]
;;

let%expect_test "contains var in var expression" =
  let var = Var.Register A in
  let exp = Var var in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| true |}]
;;

let%expect_test "does not contain var in var expression" =
  let var = Var.Register A in
  let exp = Var (Register B) in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| false |}]
;;

let%expect_test "contains var in add expression" =
  let var = Var.Register A in
  let exp = Add [ Const 1; Var var ] in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| true |}]
;;

let%expect_test "does not contain var in add expression" =
  let var = Var.Register A in
  let exp = Add [ Const 1; Var (Register B) ] in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| false |}]
;;

let%expect_test "contains var in sub lhs" =
  let var = Var.Register A in
  let exp = Sub (Var var, Const 1) in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| true |}]
;;

let%expect_test "contains var in sub rhs" =
  let var = Var.Register A in
  let exp = Sub (Const 1, Var var) in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| true |}]
;;

let%expect_test "contains var in sub neither" =
  let var = Var.Register A in
  let exp = Sub (Const 1, Const 2) in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| false |}]
;;

let%expect_test "contains var in if lhs" =
  let var = Var.Register A in
  let exp = If { cmp = Eq; left = Var var; right = Const 1 } in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| true |}]
;;

let%expect_test "contains var in if rhs" =
  let var = Var.Register A in
  let exp = If { cmp = Eq; left = Const 1; right = Var var } in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| true |}]
;;

let%expect_test "contains var in if neither" =
  let var = Var.Register A in
  let exp = If { cmp = Eq; left = Const 1; right = Const 2 } in
  let contains = contains exp var in
  printf "%b" contains;
  [%expect {| false |}]
;;
