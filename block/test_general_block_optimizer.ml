open! Core
open Elvm_opt
module Expression_opt = Elvm_opt.Expression_optimizer
module Statement_opt = Elvm_opt.Statement_optimizer.Make (Expression_opt)
module Block_opt = Elvm_opt.General_block_optimizer.Make (Statement_opt)

let optimizer =
  Expression_opt.create () |> Statement_opt.create |> Block_opt.create

let print result = print_s [%sexp (result : Block.t * bool)]
let optimize = Block_opt.optimize optimizer

let%expect_test "optimizes statements" =
  let label = "foo" in
  let statements =
    [ Statement.Assign { dst = Register A; src = Add [ Const 1; Const 1 ] } ]
  in
  let in_edges = [] in
  let branch = None in
  let block = Block.create ~label ~statements ~in_edges ~branch in
  optimize block |> print;
  [%expect
    {|
    (((label foo) (statements ((Assign ((dst (Register A)) (src (Const 2))))))
      (in_edges ()) (branch ()))
     true) |}]

let%expect_test "corrects optimized branch" =
  let baz =
    let label = "baz" in
    let in_edges = [ Block.Edge.{ target = "foo"; type_ = Jump } ] in
    let statements = [ Statement.Nop ] in
    let branch = None in
    Block.create ~label ~statements ~in_edges ~branch
  in
  let bar =
    let in_edges = [ Block.Edge.{ target = "foo"; type_ = Fallthrough } ] in
    Block.create ~label:"bar" ~statements:[ Nop ] ~in_edges ~branch:None
  in
  let foo =
    let statements =
      [
        Statement.Jump
          {
            target = Label "baz";
            (* should be simplified into an unconditional jump *)
            condition =
              Some { comparison = Eq; left = Const 1; right = Const 1 };
          };
      ]
    in
    let in_edges = [] in
    let branch = Some (Block.Conditional_jump { true_ = baz; false_ = bar }) in
    Block.create ~label:"foo" ~statements ~in_edges ~branch
  in
  optimize foo |> print;
  [%expect
    {|
    (((label foo) (statements ((Jump ((target (Label baz)) (condition ())))))
      (in_edges ())
      (branch
       ((Unconditional_jump
         ((label baz) (statements (Nop)) (in_edges (((target foo) (type_ Jump))))
          (branch ()))))))
     true) |}]