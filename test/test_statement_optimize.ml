open Core
open Elvm_opt.Expression
open Elvm_opt.Statement

let print stmt = print_s [%sexp (stmt : t)]
let optimize stmt = optimize stmt |> fst
let ugly_exp = Add [ Const 1; Const 1 ]

let%expect_test "optimizes assign dst and src" =
  optimize (Assign { dst = Memory ugly_exp; src = ugly_exp }) |> print;
  [%expect {| (Assign ((dst (Memory (Const 2))) (src (Const 2)))) |}]

let%expect_test "optimizes putc arg" =
  optimize (Putc ugly_exp) |> print;
  [%expect {| (Putc (Const 2)) |}]

let%expect_test "exit is unchanged" =
  optimize Exit |> print;
  [%expect {| Exit |}]

let%expect_test "nop is unchanged" =
  optimize Nop |> print;
  [%expect {| Nop |}]

let%expect_test "optimizes jump target" =
  optimize (Jump { target = ugly_exp; condition = None }) |> print;
  [%expect {| (Jump ((target (Const 2)) (condition ()))) |}]

let%expect_test "always true conditional jump is changed to unconditional" =
  let always_true = Some { comparison = Eq; a = ugly_exp; b = ugly_exp } in
  optimize (Jump { target = ugly_exp; condition = always_true }) |> print;
  [%expect {| (Jump ((target (Const 2)) (condition ()))) |}]

let%expect_test "always false conditional jump is changed to nop" =
  let always_false = Some { comparison = Ne; a = ugly_exp; b = ugly_exp } in
  optimize (Jump { target = ugly_exp; condition = always_false }) |> print;
  [%expect {| Nop |}]
