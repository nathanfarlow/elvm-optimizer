open Core
open Elvm_opt.Expression

let () =
  let a = Add [ Const 0; Const 1 ] in
  let b = Add [ Const 1; Const 0 ] in
  let eq = equal a b in
  printf "%b\n" eq
