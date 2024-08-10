open Core
open Elvm

let parse = Eir.parse_exn

let print_insns eir =
  let insns = Eir.insns eir in
  print_s [%sexp (insns : Eir.Instruction.t list)]
;;

let print_data eir =
  let data = Eir.data eir in
  print_s [%sexp (data : Eir.Data.t list)]
;;

let print_labels eir =
  let labels =
    Eir.labels eir
    |> Hashtbl.to_alist
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  print_s [%sexp (labels : (string * Eir.Address.t) list)]
;;

let%expect_test "parses mov instruction" =
  parse {|
  .text
  mov A, 1
  |} |> print_insns;
  [%expect {| ((Mov ((src (Int 1)) (dst A)))) |}]
;;

let%expect_test "parses add instruction" =
  parse {|
  .text
  add A, 1
  |} |> print_insns;
  [%expect {| ((Add ((src (Int 1)) (dst A)))) |}]
;;

let%expect_test "parses sub instruction" =
  parse {|
  .text
  sub A, 1
  |} |> print_insns;
  [%expect {| ((Sub ((src (Int 1)) (dst A)))) |}]
;;

let%expect_test "parses load instruction" =
  parse {|
  .text
  load A, 1
  |} |> print_insns;
  [%expect {| ((Load ((src (Int 1)) (dst A)))) |}]
;;

let%expect_test "parses store instruction" =
  parse {|
  .text
  store A, 1
  |} |> print_insns;
  [%expect {| ((Store (src A) (dst (Int 1)))) |}]
;;

let%expect_test "parses putc instruction" =
  parse {|
  .text
  putc A
  |} |> print_insns;
  [%expect {| ((Putc (Register A))) |}]
;;

let%expect_test "parses getc instruction" =
  parse {|
  .text
  getc A
  |} |> print_insns;
  [%expect {| ((Getc A)) |}]
;;

let%expect_test "parses exit instruction" =
  parse {|
  .text
  exit
  |} |> print_insns;
  [%expect {| (Exit) |}]
;;

let%expect_test "parses unconditional jump instruction" =
  parse {|
  .text
  label:
  jmp label
  |} |> print_insns;
  [%expect {| ((Jump (target (Label label)) (cond ()))) |}]
;;

let%expect_test "parses jeq instruction" =
  parse {|
  .text
  label:
  jeq label, A, B
  |} |> print_insns;
  [%expect
    {|
      ((Jump (target (Label label))
        (cond (((cmp Eq) (args ((src (Register B)) (dst A)))))))) |}]
;;

let%expect_test "parses jne instruction" =
  parse {|
  .text
  label:
  jne label, A, B
  |} |> print_insns;
  [%expect
    {|
      ((Jump (target (Label label))
        (cond (((cmp Ne) (args ((src (Register B)) (dst A)))))))) |}]
;;

let%expect_test "parses jlt instruction" =
  parse {|
  .text
  label:
  jlt label, A, B
  |} |> print_insns;
  [%expect
    {|
      ((Jump (target (Label label))
        (cond (((cmp Lt) (args ((src (Register B)) (dst A)))))))) |}]
;;

let%expect_test "parses jle instruction" =
  parse {|
  .text
  label:
  jle label, A, B
  |} |> print_insns;
  [%expect
    {|
      ((Jump (target (Label label))
        (cond (((cmp Le) (args ((src (Register B)) (dst A)))))))) |}]
;;

let%expect_test "parses jgt instruction" =
  parse {|
  .text
  label:
  jgt label, A, B
  |} |> print_insns;
  [%expect
    {|
      ((Jump (target (Label label))
        (cond (((cmp Gt) (args ((src (Register B)) (dst A)))))))) |}]
;;

let%expect_test "parses jge instruction" =
  parse {|
  .text
  label:
  jge label, A, B
  |} |> print_insns;
  [%expect
    {|
      ((Jump (target (Label label))
        (cond (((cmp Ge) (args ((src (Register B)) (dst A)))))))) |}]
;;

let%expect_test "parses eq instruction" =
  parse {|
  .text
  eq A, B
  |} |> print_insns;
  [%expect {| ((Set ((cmp Eq) (args ((src (Register B)) (dst A)))))) |}]
;;

let%expect_test "parses ne instruction" =
  parse {|
  .text
  ne A, B
  |} |> print_insns;
  [%expect {| ((Set ((cmp Ne) (args ((src (Register B)) (dst A)))))) |}]
;;

let%expect_test "parses lt instruction" =
  parse {|
  .text
  lt A, B
  |} |> print_insns;
  [%expect {| ((Set ((cmp Lt) (args ((src (Register B)) (dst A)))))) |}]
;;

let%expect_test "parses le instruction" =
  parse {|
  .text
  le A, B
  |} |> print_insns;
  [%expect {| ((Set ((cmp Le) (args ((src (Register B)) (dst A)))))) |}]
;;

let%expect_test "parses gt instruction" =
  parse {|
  .text
  gt A, B
  |} |> print_insns;
  [%expect {| ((Set ((cmp Gt) (args ((src (Register B)) (dst A)))))) |}]
;;

let%expect_test "parses ge instruction" =
  parse {|
  .text
  ge A, B
  |} |> print_insns;
  [%expect {| ((Set ((cmp Ge) (args ((src (Register B)) (dst A)))))) |}]
;;

let%expect_test "parses dump instruction" =
  parse {|
  .text
  dump
  |} |> print_insns;
  [%expect {| (Dump) |}]
;;

let%expect_test "parses with no instructions" =
  parse ".text" |> print_insns;
  [%expect {| () |}]
;;

let%expect_test "parses const data declaration" =
  parse ".data\n  .long 5" |> print_data;
  [%expect {| ((Const 5) (Label __reserved_heap_base)) |}]
;;

let%expect_test "parses label data declaration" =
  parse {|.data
  foo:
  .long foo
  |} |> print_data;
  [%expect {| ((Label foo) (Label __reserved_heap_base)) |}]
;;

let%expect_test "parses string data declaration" =
  parse {|.data
  .string "foo"|} |> print_data;
  [%expect
    {| ((Const 102) (Const 111) (Const 111) (Const 0) (Label __reserved_heap_base)) |}]
;;

let%expect_test "ignores comments" =
  parse {|.text
  #foo
  .file a
  .loc b
  |} |> print_insns;
  [%expect {| () |}]
;;

let%expect_test "data labels are correct" =
  parse {|
  .data
  foo:
  .long 0
  |} |> print_labels;
  [%expect
    {|
    ((__reserved_heap_base ((segment Data) (offset 2)))
     (_edata ((segment Data) (offset 1))) (foo ((segment Data) (offset 0)))) |}]
;;

let%expect_test "text labels are correct" =
  parse {|
  .text
  foo:
  exit
  |} |> print_labels;
  [%expect
    {|
    ((__reserved_heap_base ((segment Data) (offset 1)))
     (_edata ((segment Data) (offset 0))) (foo ((segment Text) (offset 0)))) |}]
;;

let%expect_test "parses insane subsection behavior" =
  let eir =
    parse
      {|.data 0
  foo:
    .data 1
    .L3:
    .string "foofoofoo"
    .data 0
    .long .L3
  |}
  in
  print_data eir;
  [%expect
    {|
    ((Label .L3) (Const 102) (Const 111) (Const 111) (Const 102) (Const 111)
     (Const 111) (Const 102) (Const 111) (Const 111) (Const 0)
     (Label __reserved_heap_base)) |}];
  print_labels eir;
  [%expect
    {|
    ((.L3 ((segment Data) (offset 1)))
     (__reserved_heap_base ((segment Data) (offset 12)))
     (_edata ((segment Data) (offset 11))) (foo ((segment Data) (offset 0)))) |}]
;;

let%expect_test "converts elvm's weird signed 24 bit constants" =
  parse {|
  .data
  .long 16777215
  |} |> print_data;
  [%expect {| ((Const -1) (Label __reserved_heap_base)) |}]
;;

let%expect_test "unescapes string constants" =
  parse {|
  .data
  .string "\n"
  |} |> print_data;
  [%expect {| ((Const 10) (Const 0) (Label __reserved_heap_base)) |}]
;;
