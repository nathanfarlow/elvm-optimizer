open Core
open Async
open Elvm

let compile s =
  let graph, _ = Eir.parse_exn s |> Lift.f in
  Compile.go graph
;;

let print_graph graph =
  let to_string stmt =
    [%sexp (stmt : Eir.Instruction.t)] |> Sexp.to_string_hum ~indent:2
  in
  Graph_util.to_ascii graph to_string >>| print_endline
;;

let print_data (_, data) = [%message (data : Eir.Data.t list Map.M(String).t)] |> print_s

let%expect_test "ast" =
  let open Ast in
  let exp = Expression.Add [ Const 1; Const 2; Const 3 ] in
  let stmt = Statement.Assign { dst = Register "foo"; src = exp } in
  let graph = Graph.create () in
  let _node = Graph.add graph "foo" stmt in
  let yuh = Compile.go graph in
  let%bind () = print_graph yuh in
  [%expect
    {|
    +-----------------------------------------+
    |                                         |
    | foo:                                    |
    |                                         |
    | (Mov ((src (Int 1)) (dst reg0)))        |
    +-----------------------------------------+
      |
      |
      v
    +-----------------------------------------+
    |                                         |
    | __L0:                                   |
    |                                         |
    | (Add ((src (Register reg0)) (dst foo))) |
    +-----------------------------------------+
      |
      |
      v
    +-----------------------------------------+
    |                                         |
    | __L1:                                   |
    |                                         |
    | (Mov ((src (Int 2)) (dst reg0)))        |
    +-----------------------------------------+
      |
      |
      v
    +-----------------------------------------+
    |                                         |
    | __L2:                                   |
    |                                         |
    | (Add ((src (Register reg0)) (dst foo))) |
    +-----------------------------------------+
      |
      |
      v
    +-----------------------------------------+
    |                                         |
    | __L3:                                   |
    |                                         |
    | (Mov ((src (Int 3)) (dst reg0)))        |
    +-----------------------------------------+
      |
      |
      v
    +-----------------------------------------+
    |                                         |
    | __L4:                                   |
    |                                         |
    | (Add ((src (Register reg0)) (dst foo))) |
    +-----------------------------------------+
    |}];
  return ()
;;

let%expect_test "mov round trips" =
  let%bind () =
    compile
      {|
    .text
    mov A, B
    exit
|}
    |> print_graph
  in
  [%expect
    {|
    +------------------------------------+
    |                                    |
    | __L0:                              |
    |                                    |
    | (Mov ((src (Register B)) (dst A))) |
    +------------------------------------+
      |
      |
      v
    +------------------------------------+
    |                                    |
    | __L1:                              |
    |                                    |
    | Exit                               |
    +------------------------------------+
    |}];
  return ()
;;
