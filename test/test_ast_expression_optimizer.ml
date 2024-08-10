open Core
open Elvm
open Ast.Expression

let optimize exp = Ast_optimizer.optimize_expression exp |> [%sexp_of: t] |> print_s

let%expect_test "const remains same" =
  optimize (Const 0);
  [%expect {| (Const 0) |}]
;;

let%expect_test "register remains same" =
  optimize (Var (Register A));
  [%expect {| (Var (Register A)) |}]
;;

let%expect_test "memory address is simplified" =
  optimize (Var (Memory (Sub (Const 1, Const 1))));
  [%expect {| (Var (Memory (Const 0))) |}]
;;

let%expect_test "add's elements are simplified" =
  optimize (Add [ Sub (Const 1, Const 1); Sub (Const 1, Const 1) ]);
  [%expect {| (Const 0) |}]
;;

let%expect_test "add combines constants" =
  optimize (Add [ Const 1; Var (Memory (Const 0)); Const 2 ]);
  [%expect {| (Add ((Const 3) (Var (Memory (Const 0))))) |}]
;;

let%expect_test "add optimizes nested adds" =
  optimize (Add [ Const 1; Add [ Const 2; Var (Memory (Const 3)) ]; Const 4 ]);
  [%expect {| (Add ((Const 7) (Var (Memory (Const 3))))) |}]
;;

let%expect_test "add with one element is that element" =
  optimize (Add [ Const 1 ]);
  [%expect {| (Const 1) |}]
;;

let%expect_test "add optimizes singular nested add" =
  optimize (Add [ Add [ Const 1 ] ]);
  [%expect {| (Const 1) |}]
;;

let%expect_test "add removes const 0" =
  optimize (Add [ Const 0; Var (Register A) ]);
  [%expect {| (Var (Register A)) |}]
;;

let%expect_test "add optimizes nested sub" =
  optimize (Add [ Const 1; Sub (Add [ Var (Register A); Var (Register B) ], Const 1) ]);
  [%expect {| (Add ((Var (Register A)) (Var (Register B)))) |}]
;;

let%expect_test "sub is 0 when args are same" =
  optimize (Sub (Var (Memory (Const 0)), Var (Memory (Const 0))));
  [%expect {| (Const 0) |}]
;;

let%expect_test "sub evaluates constants" =
  optimize (Sub (Const 2, Const 1));
  [%expect {| (Const 1) |}]
;;

let%expect_test "sub simplifies to add for constants" =
  optimize (Sub (Var (Register A), Const 1));
  [%expect {| (Add ((Const -1) (Var (Register A)))) |}]
;;

let%expect_test "if optimizes children" =
  optimize
    (If { cmp = Eq; left = Sub (Const 1, Const 1); right = Var (Memory (Const 0)) });
  [%expect {| (If ((cmp Eq) (left (Const 0)) (right (Var (Memory (Const 0)))))) |}];
  optimize
    (If { cmp = Eq; left = Var (Memory (Const 0)); right = Sub (Const 1, Const 1) });
  [%expect {| (If ((cmp Eq) (left (Var (Memory (Const 0)))) (right (Const 0)))) |}]
;;

let%expect_test "if optimizes eq when args are equal" =
  optimize
    (If { cmp = Eq; left = Var (Memory (Const 0)); right = Var (Memory (Const 0)) });
  [%expect {| (Const 1) |}]
;;

let%expect_test "if optimizes ne when args are equal" =
  optimize
    (If { cmp = Ne; left = Var (Memory (Const 0)); right = Var (Memory (Const 0)) });
  [%expect {| (Const 0) |}]
;;

let%expect_test "if optimizes lt when args are equal" =
  optimize
    (If { cmp = Lt; left = Var (Memory (Const 0)); right = Var (Memory (Const 0)) });
  [%expect {| (Const 0) |}]
;;

let%expect_test "if optimizes le when args are equal" =
  optimize
    (If { cmp = Le; left = Var (Memory (Const 0)); right = Var (Memory (Const 0)) });
  [%expect {| (Const 1) |}]
;;

let%expect_test "if optimizes eq when args are const" =
  optimize (If { cmp = Eq; left = Const 0; right = Const 0 });
  [%expect {| (Const 1) |}];
  optimize (If { cmp = Eq; left = Const 1; right = Const 0 });
  [%expect {| (Const 0) |}]
;;

let%expect_test "if optimizes ne when args are const" =
  optimize (If { cmp = Ne; left = Const 0; right = Const 0 });
  [%expect {| (Const 0) |}];
  optimize (If { cmp = Ne; left = Const 1; right = Const 0 });
  [%expect {| (Const 1) |}]
;;

let%expect_test "if optimizes lt when args are const" =
  optimize (If { cmp = Lt; left = Const 0; right = Const 0 });
  [%expect {| (Const 0) |}];
  optimize (If { cmp = Lt; left = Const 1; right = Const 0 });
  [%expect {| (Const 0) |}];
  optimize (If { cmp = Lt; left = Const 0; right = Const 1 });
  [%expect {| (Const 1) |}]
;;

let%expect_test "if optimizes le when args are const" =
  optimize (If { cmp = Le; left = Const 0; right = Const 0 });
  [%expect {| (Const 1) |}];
  optimize (If { cmp = Le; left = Const 1; right = Const 0 });
  [%expect {| (Const 0) |}];
  optimize (If { cmp = Le; left = Const 0; right = Const 1 });
  [%expect {| (Const 1) |}]
;;
