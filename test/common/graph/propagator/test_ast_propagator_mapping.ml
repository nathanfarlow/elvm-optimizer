open Elvm
module Mapping = Propagator_mapping.Make (Ast.Variable) (Ast.Expression)

let print mapping = print_s [%sexp (mapping : Mapping.t)]

let%expect_test "test empty" =
  print Mapping.empty;
  [%expect {| () |}]

let%expect_test "test update from empty" =
  let Mapping.{ valid; invalid } =
    Mapping.update Mapping.empty ~from:(Register A) ~to_:(Const 0)
  in
  print valid;
  [%expect {| (((Register A) (Const 0))) |}];
  print invalid;
  [%expect {| () |}]

let%expect_test "test update which invalidates another mapping bc new var is \
                 old var" =
  let Mapping.{ valid; _ } =
    Mapping.update Mapping.empty ~from:(Register A) ~to_:(Const 0)
  in
  let Mapping.{ valid; invalid } =
    Mapping.update valid ~from:(Register A) ~to_:(Const 1)
  in
  print valid;
  [%expect {| (((Register A) (Const 1))) |}];
  print invalid;
  [%expect {| (((Register A) (Const 0))) |}]

let%expect_test "test update which invalidates another mapping bc new var is \
                 in old exp" =
  let Mapping.{ valid; _ } =
    Mapping.update Mapping.empty ~from:(Register A) ~to_:(Var (Register B))
  in
  let Mapping.{ valid; invalid } =
    Mapping.update valid ~from:(Register B) ~to_:(Const 1)
  in
  print valid;
  [%expect {| (((Register B) (Const 1))) |}];
  print invalid;
  [%expect {| (((Register A) (Var (Register B)))) |}]

let%expect_test "test merge with empty" =
  let Mapping.{ valid = mapping; _ } =
    Mapping.update Mapping.empty ~from:(Register A) ~to_:(Const 0)
  in
  let mapping = Mapping.merge Mapping.empty mapping in
  print mapping;
  [%expect {| () |}]

let%expect_test "test merge keeps only identical mappings" =
  let Mapping.{ valid = first; _ } =
    Mapping.update Mapping.empty ~from:(Register A) ~to_:(Const 0)
  in
  let Mapping.{ valid = second; _ } =
    Mapping.update Mapping.empty ~from:(Register A) ~to_:(Const 0)
  in
  let Mapping.{ valid = second; _ } =
    Mapping.update second ~from:(Register B) ~to_:(Const 1)
  in
  let mapping = Mapping.merge first second in
  print mapping;
  [%expect {| (((Register A) (Const 0))) |}]
