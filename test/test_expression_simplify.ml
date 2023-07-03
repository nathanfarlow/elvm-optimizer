open! Core
open Elvm_opt.Expression

let%test "const remains same" =
  let a = Const 0 in
  equal a (simplify a)

let%test "register remains same" =
  let a = Register A in
  equal a (simplify a)

let%test "getc remains same" =
  let a = Getc in
  equal a (simplify a)

let%test "memory address is simplified" =
  let ugly = Memory (Sub (Const 1, Const 1)) in
  let simplified = Memory (Const 0) in
  equal simplified (simplify ugly)

let%test "add's elements are simplified" =
  let ugly = Sub (Const 1, Const 1) in
  let ugly = Add [ ugly; ugly ] in
  let simplified = Const 0 in
  equal simplified (simplify ugly)

let%test "add combines constants" =
  let ugly = Add [ Const 1; Memory (Const 0); Const 2 ] in
  let simplified = Add [ Const 3; Memory (Const 0) ] in
  equal simplified (simplify ugly)

let%test "add simplifies nested adds" =
  let ugly = Add [ Const 1; Add [ Const 2; Memory (Const 3) ]; Const 4 ] in
  let simplified = Add [ Const 7; Memory (Const 3) ] in
  equal simplified (simplify ugly)

let%test "add with one element is that element" =
  let ugly = Add [ Const 1 ] in
  let simplified = Const 1 in
  equal simplified (simplify ugly)

let%test "add simplifies singular nested add" =
  let ugly = Add [ Add [ Const 1 ] ] in
  let simplified = Const 1 in
  equal simplified (simplify ugly)

let%test "sub is 0 when args are same" =
  let ugly = Sub (Memory (Const 0), Memory (Const 0)) in
  let simplified = Const 0 in
  equal simplified (simplify ugly)

let%test "sub evaluates constants" =
  let ugly = Sub (Const 2, Const 1) in
  let simplified = Const 1 in
  equal simplified (simplify ugly)

let%test_unit "set simplifies children" =
  let ugly =
    Set { comparison = Eq; a = Sub (Const 1, Const 1); b = Memory (Const 0) }
  in
  let simplified = Set { comparison = Eq; a = Const 0; b = Memory (Const 0) } in
  [%test_eq: bool] true (equal simplified (simplify ugly));
  let ugly =
    Set { comparison = Eq; a = Memory (Const 0); b = Sub (Const 1, Const 1) }
  in
  let simplified = Set { comparison = Eq; a = Memory (Const 0); b = Const 0 } in
  [%test_eq: bool] true (equal simplified (simplify ugly))

let%test "set simplifies eq when args are equal" =
  let ugly =
    Set { comparison = Eq; a = Memory (Const 0); b = Memory (Const 0) }
  in
  let simplified = Const 1 in
  equal simplified (simplify ugly)

let%test "set simplifies ne when args are equal" =
  let ugly =
    Set { comparison = Ne; a = Memory (Const 0); b = Memory (Const 0) }
  in
  let simplified = Const 0 in
  equal simplified (simplify ugly)

let%test "set simplifies lt when args are equal" =
  let ugly =
    Set { comparison = Lt; a = Memory (Const 0); b = Memory (Const 0) }
  in
  let simplified = Const 0 in
  equal simplified (simplify ugly)

let%test "set simplifies le when args are equal" =
  let ugly =
    Set { comparison = Le; a = Memory (Const 0); b = Memory (Const 0) }
  in
  let simplified = Const 1 in
  equal simplified (simplify ugly)

let%test_unit "set simplifies eq when args are const" =
  let ugly = Set { comparison = Eq; a = Const 0; b = Const 0 } in
  let simplified = Const 1 in
  [%test_eq: bool] true (equal simplified (simplify ugly));
  let ugly = Set { comparison = Eq; a = Const 0; b = Const 1 } in
  let simplified = Const 0 in
  [%test_eq: bool] true (equal simplified (simplify ugly))

let%test_unit "set simplifies ne when args are const" =
  let ugly = Set { comparison = Ne; a = Const 0; b = Const 0 } in
  let simplified = Const 0 in
  [%test_eq: bool] true (equal simplified (simplify ugly));
  let ugly = Set { comparison = Ne; a = Const 0; b = Const 1 } in
  let simplified = Const 1 in
  [%test_eq: bool] true (equal simplified (simplify ugly))

let%test_unit "set simplifies lt when args are const" =
  let ugly = Set { comparison = Lt; a = Const 0; b = Const 0 } in
  let simplified = Const 0 in
  [%test_eq: bool] true (equal simplified (simplify ugly));
  let ugly = Set { comparison = Lt; a = Const 1; b = Const 0 } in
  let simplified = Const 0 in
  [%test_eq: bool] true (equal simplified (simplify ugly));
  let ugly = Set { comparison = Lt; a = Const 0; b = Const 1 } in
  let simplified = Const 1 in
  [%test_eq: bool] true (equal simplified (simplify ugly))

let%test_unit "set simplifies le when args are const" =
  let ugly = Set { comparison = Le; a = Const 0; b = Const 0 } in
  let simplified = Const 1 in
  [%test_eq: bool] true (equal simplified (simplify ugly));
  let ugly = Set { comparison = Le; a = Const 1; b = Const 0 } in
  let simplified = Const 0 in
  [%test_eq: bool] true (equal simplified (simplify ugly));
  let ugly = Set { comparison = Le; a = Const 0; b = Const 1 } in
  let simplified = Const 1 in
  [%test_eq: bool] true (equal simplified (simplify ugly))