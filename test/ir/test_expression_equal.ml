open Core
open Elvm_opt.Expression

let%test "constants equal" =
  let a = Const 0 in
  let b = Const 0 in
  equal a b

let%test "constants not equal" =
  let a = Const 0 in
  let b = Const 1 in
  not (equal a b)

let%test "same registers are equal" =
  let a = Register A in
  let b = Register A in
  equal a b

let%test "different registers are not equal" =
  let a = Register A in
  let b = Register B in
  not @@ equal a b

let%test "same memory is equal" =
  let a = Memory (Const 0) in
  let b = Memory (Const 0) in
  equal a b

let%test "different memory is not equal" =
  let a = Memory (Const 0) in
  let b = Memory (Const 1) in
  not @@ equal a b

let%test "add in same order is equal" =
  let a = Add [ Const 0; Const 1 ] in
  let b = Add [ Const 0; Const 1 ] in
  equal a b

let%test "add in different order is equal" =
  let a = Add [ Const 0; Const 1 ] in
  let b = Add [ Const 1; Const 0 ] in
  equal a b

let%test "add with different elements is not equal" =
  let a = Add [ Const 0; Const 1 ] in
  let b = Add [ Const 0; Const 2 ] in
  not @@ equal a b

let%test "add with different number of elements is not equal" =
  let a = Add [ Const 0; Const 1 ] in
  let b = Add [ Const 0; Const 1; Const 2 ] in
  not @@ equal a b

let%test "adds with no elements are equal" =
  let a = Add [] in
  let b = Add [] in
  equal a b

let%test "sub with same elements is equal" =
  let a = Sub (Const 0, Const 1) in
  let b = Sub (Const 0, Const 1) in
  equal a b

let%test "sub with different elements is not equal" =
  let a = Sub (Const 0, Const 1) in
  let b = Sub (Const 0, Const 2) in
  not @@ equal a b

let%test "getc is equal" =
  let a = Getc in
  let b = Getc in
  equal a b

let%test "same conditions are equal" =
  let a = Set { comparison = Eq; left = Const 0; right = Const 1 } in
  let b = Set { comparison = Eq; left = Const 0; right = Const 1 } in
  equal a b

let%test "different conditions are not equal" =
  let a = Set { comparison = Eq; left = Const 0; right = Const 1 } in
  let b = Set { comparison = Eq; left = Const 0; right = Const 2 } in
  not @@ equal a b