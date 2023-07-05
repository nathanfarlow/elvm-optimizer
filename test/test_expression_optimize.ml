open Core
open Elvm_opt.Expression

let print exp = print_s [%sexp (exp : t)]

let%expect_test "const remains same" =
  optimize (Const 0) |> print;
  [%expect {| (Const 0) |}]

let%expect_test "register remains same" =
  optimize (Register A) |> print;
  [%expect {| (Register A) |}]

let%expect_test "getc remains same" =
  optimize Getc |> print;
  [%expect {| Getc |}]

let%expect_test "memory address is simplified" =
  optimize (Memory (Sub (Const 1, Const 1))) |> print;
  [%expect {| (Memory (Const 0)) |}]

let%expect_test "add's elements are simplified" =
  optimize (Add [ Sub (Const 1, Const 1); Sub (Const 1, Const 1) ]) |> print;
  [%expect {| (Const 0) |}]

let%expect_test "add combines constants" =
  optimize (Add [ Const 1; Memory (Const 0); Const 2 ]) |> print;
  [%expect {| (Add ((Const 3) (Memory (Const 0)))) |}]

let%expect_test "add optimizes nested adds" =
  optimize (Add [ Const 1; Add [ Const 2; Memory (Const 3) ]; Const 4 ])
  |> print;
  [%expect {| (Add ((Const 7) (Memory (Const 3)))) |}]

let%expect_test "add with one element is that element" =
  optimize (Add [ Const 1 ]) |> print;
  [%expect {| (Const 1) |}]

let%expect_test "add optimizes singular nested add" =
  optimize (Add [ Add [ Const 1 ] ]) |> print;
  [%expect {| (Const 1) |}]

let%expect_test "add removes const 0" =
  optimize (Add [ Const 0; Register A ]) |> print;
  [%expect {| (Register A) |}]

let%expect_test "add optimizes nested sub" =
  optimize (Add [ Const 1; Sub (Add [ Register A; Register B ], Const 1) ])
  |> print;
  [%expect {| (Add ((Register A) (Register B))) |}]

let%expect_test "sub is 0 when args are same" =
  optimize (Sub (Memory (Const 0), Memory (Const 0))) |> print;
  [%expect {| (Const 0) |}]

let%expect_test "sub evaluates constants" =
  optimize (Sub (Const 2, Const 1)) |> print;
  [%expect {| (Const 1) |}]

let%expect_test "sub simplifies to add for constants" =
  optimize (Sub (Register A, Const 1)) |> print;
  [%expect {| (Add ((Const -1) (Register A))) |}]

let%expect_test "set optimizes children" =
  optimize
    (Set { comparison = Eq; a = Sub (Const 1, Const 1); b = Memory (Const 0) })
  |> print;
  [%expect {| (Set ((comparison Eq) (a (Const 0)) (b (Memory (Const 0))))) |}];
  optimize
    (Set { comparison = Eq; a = Memory (Const 0); b = Sub (Const 1, Const 1) })
  |> print;
  [%expect {| (Set ((comparison Eq) (a (Memory (Const 0))) (b (Const 0)))) |}]

let%expect_test "set optimizes eq when args are equal" =
  optimize (Set { comparison = Eq; a = Memory (Const 0); b = Memory (Const 0) })
  |> print;
  [%expect {| (Const 1) |}]

let%expect_test "set optimizes ne when args are equal" =
  optimize (Set { comparison = Ne; a = Memory (Const 0); b = Memory (Const 0) })
  |> print;
  [%expect {| (Const 0) |}]

let%expect_test "set optimizes lt when args are equal" =
  optimize (Set { comparison = Lt; a = Memory (Const 0); b = Memory (Const 0) })
  |> print;
  [%expect {| (Const 0) |}]

let%expect_test "set optimizes le when args are equal" =
  optimize (Set { comparison = Le; a = Memory (Const 0); b = Memory (Const 0) })
  |> print;
  [%expect {| (Const 1) |}]

let%expect_test "set optimizes eq when args are const" =
  optimize (Set { comparison = Eq; a = Const 0; b = Const 0 }) |> print;
  [%expect {| (Const 1) |}];
  optimize (Set { comparison = Eq; a = Const 1; b = Const 0 }) |> print;
  [%expect {| (Const 0) |}]

let%expect_test "set optimizes ne when args are const" =
  optimize (Set { comparison = Ne; a = Const 0; b = Const 0 }) |> print;
  [%expect {| (Const 0) |}];
  optimize (Set { comparison = Ne; a = Const 1; b = Const 0 }) |> print;
  [%expect {| (Const 1) |}]

let%expect_test "set optimizes lt when args are const" =
  optimize (Set { comparison = Lt; a = Const 0; b = Const 0 }) |> print;
  [%expect {| (Const 0) |}];
  optimize (Set { comparison = Lt; a = Const 1; b = Const 0 }) |> print;
  [%expect {| (Const 0) |}];
  optimize (Set { comparison = Lt; a = Const 0; b = Const 1 }) |> print;
  [%expect {| (Const 1) |}]

let%expect_test "set optimizes le when args are const" =
  optimize (Set { comparison = Le; a = Const 0; b = Const 0 }) |> print;
  [%expect {| (Const 1) |}];
  optimize (Set { comparison = Le; a = Const 1; b = Const 0 }) |> print;
  [%expect {| (Const 0) |}];
  optimize (Set { comparison = Le; a = Const 0; b = Const 1 }) |> print;
  [%expect {| (Const 1) |}]