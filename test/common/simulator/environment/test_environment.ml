open Core
open Elvm
module Environment = Environment.Make (Ast.Variable) (Ast.Expression)

let print mapping = print_s [%sexp (mapping : Environment.t)]

let%expect_test "test empty" =
  print Environment.empty;
  [%expect {| () |}]

let%expect_test "test update from empty" =
  let Environment.{ valid; invalid } =
    Environment.update Environment.empty ~from:(Register A) ~to_:(Const 0)
  in
  print valid;
  [%expect {| (((Register A) (Const 0))) |}];
  print invalid;
  [%expect {| () |}]

let%expect_test "test update which invalidates another mapping bc new var is \
                 old var" =
  let Environment.{ valid; _ } =
    Environment.update Environment.empty ~from:(Register A) ~to_:(Const 0)
  in
  let Environment.{ valid; invalid } =
    Environment.update valid ~from:(Register A) ~to_:(Const 1)
  in
  print valid;
  [%expect {| (((Register A) (Const 1))) |}];
  print invalid;
  [%expect {| (((Register A) (Const 0))) |}]

let%expect_test "test update which invalidates another mapping bc new var is \
                 in old exp" =
  let Environment.{ valid; _ } =
    Environment.update Environment.empty ~from:(Register A)
      ~to_:(Var (Register B))
  in
  let Environment.{ valid; invalid } =
    Environment.update valid ~from:(Register B) ~to_:(Const 1)
  in
  print valid;
  [%expect {| (((Register B) (Const 1))) |}];
  print invalid;
  [%expect {| (((Register A) (Var (Register B)))) |}]

let%expect_test "test intersection with empty" =
  let Environment.{ valid = mapping; _ } =
    Environment.update Environment.empty ~from:(Register A) ~to_:(Const 0)
  in
  let mapping = Environment.intersection Environment.empty mapping in
  print mapping;
  [%expect {| () |}]

let%expect_test "test merge keeps only identical mappings" =
  let Environment.{ valid = first; _ } =
    Environment.update Environment.empty ~from:(Register A) ~to_:(Const 0)
  in
  let Environment.{ valid = second; _ } =
    Environment.update Environment.empty ~from:(Register A) ~to_:(Const 0)
  in
  let Environment.{ valid = second; _ } =
    Environment.update second ~from:(Register B) ~to_:(Const 1)
  in
  let mapping = Environment.intersection first second in
  print mapping;
  [%expect {| (((Register A) (Const 0))) |}]
