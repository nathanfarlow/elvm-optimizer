open Core
open Elvm_opt.Expression

let print exp = print_s [%sexp (exp : t)]

let%expect_test "const remains same" =
  print @@ simplify (Const 0);
  [%expect {| (Const 0) |}]

let%expect_test "register remains same" =
  print @@ simplify (Register A);
  [%expect {| (Register A) |}]

let%expect_test "getc remains same" =
  print @@ simplify Getc;
  [%expect {| Getc |}]

let%expect_test "memory address is simplified" =
  print @@ simplify (Memory (Sub (Const 1, Const 1)));
  [%expect {| (Memory (Const 0)) |}]

let%expect_test "add's elements are simplified" =
  print @@ simplify (Add [ Sub (Const 1, Const 1); Sub (Const 1, Const 1) ]);
  [%expect {| (Const 0) |}]

let%expect_test "add combines constants" =
  print @@ simplify (Add [ Const 1; Memory (Const 0); Const 2 ]);
  [%expect {| (Add ((Const 3) (Memory (Const 0)))) |}]

let%expect_test "add simplifies nested adds" =
  print
  @@ simplify (Add [ Const 1; Add [ Const 2; Memory (Const 3) ]; Const 4 ]);
  [%expect {| (Add ((Const 7) (Memory (Const 3)))) |}]

let%expect_test "add with one element is that element" =
  print @@ simplify (Add [ Const 1 ]);
  [%expect {| (Const 1) |}]

let%expect_test "add simplifies singular nested add" =
  print @@ simplify (Add [ Add [ Const 1 ] ]);
  [%expect {| (Const 1) |}]

let%expect_test "sub is 0 when args are same" =
  print @@ simplify (Sub (Memory (Const 0), Memory (Const 0)));
  [%expect {| (Const 0) |}]

let%expect_test "sub evaluates constants" =
  print @@ simplify (Sub (Const 2, Const 1));
  [%expect {| (Const 1) |}]

let%expect_test "set simplifies children" =
  print
  @@ simplify
       (Set
          { comparison = Eq; a = Sub (Const 1, Const 1); b = Memory (Const 0) });
  [%expect {| (Set ((comparison Eq) (a (Const 0)) (b (Memory (Const 0))))) |}];
  print
  @@ simplify
       (Set
          { comparison = Eq; a = Memory (Const 0); b = Sub (Const 1, Const 1) });
  [%expect {| (Set ((comparison Eq) (a (Memory (Const 0))) (b (Const 0)))) |}]

let%expect_test "set simplifies eq when args are equal" =
  print
  @@ simplify
       (Set { comparison = Eq; a = Memory (Const 0); b = Memory (Const 0) });
  [%expect {| (Const 1) |}]

let%expect_test "set simplifies ne when args are equal" =
  print
  @@ simplify
       (Set { comparison = Ne; a = Memory (Const 0); b = Memory (Const 0) });
  [%expect {| (Const 0) |}]

let%expect_test "set simplifies lt when args are equal" =
  print
  @@ simplify
       (Set { comparison = Lt; a = Memory (Const 0); b = Memory (Const 0) });
  [%expect {| (Const 0) |}]

let%expect_test "set simplifies le when args are equal" =
  print
  @@ simplify
       (Set { comparison = Le; a = Memory (Const 0); b = Memory (Const 0) });
  [%expect {| (Const 1) |}]

let%expect_test "set simplifies eq when args are const" =
  print @@ simplify (Set { comparison = Eq; a = Const 0; b = Const 0 });
  [%expect {| (Const 1) |}];
  print @@ simplify (Set { comparison = Eq; a = Const 1; b = Const 0 });
  [%expect {| (Const 0) |}]

let%expect_test "set simplifies ne when args are const" =
  print @@ simplify (Set { comparison = Ne; a = Const 0; b = Const 0 });
  [%expect {| (Const 0) |}];
  print @@ simplify (Set { comparison = Ne; a = Const 1; b = Const 0 });
  [%expect {| (Const 1) |}]

let%expect_test "set simplifies lt when args are const" =
  print @@ simplify (Set { comparison = Lt; a = Const 0; b = Const 0 });
  [%expect {| (Const 0) |}];
  print @@ simplify (Set { comparison = Lt; a = Const 1; b = Const 0 });
  [%expect {| (Const 0) |}];
  print @@ simplify (Set { comparison = Lt; a = Const 0; b = Const 1 });
  [%expect {| (Const 1) |}]

let%expect_test "set simplifies le when args are const" =
  print @@ simplify (Set { comparison = Le; a = Const 0; b = Const 0 });
  [%expect {| (Const 1) |}];
  print @@ simplify (Set { comparison = Le; a = Const 1; b = Const 0 });
  [%expect {| (Const 0) |}];
  print @@ simplify (Set { comparison = Le; a = Const 0; b = Const 1 });
  [%expect {| (Const 1) |}]