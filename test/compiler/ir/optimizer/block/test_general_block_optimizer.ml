open Elvm.Compiler.Ir
open Elvm.Compiler.Optimizer
module Statement_opt = Statement_optimizer.Make (Expression_optimizer)
module Block_opt = General_block_optimizer.Make (Statement_opt)

let optimizer =
  Expression_optimizer.create () |> Statement_opt.create |> Block_opt.create

let print_block result = print_s [%sexp (result : Block.t)]
let optimize = Block_opt.optimize optimizer

let%expect_test "optimizes statements" =
  let label = "foo" in
  let statements =
    [| Statement.Assign { dst = Register A; src = Add [ Const 1; Const 1 ] } |]
  in
  let in_edges = [] in
  let branch = None in
  let block = Block.{ label; statements; in_edges; branch } in
  printf "%b" (optimize block);
  [%expect "true"];
  print_block block;
  [%expect
    {|
    ((label foo) (statements ((Assign ((dst (Register A)) (src (Const 2))))))
     (in_edges ()) (branch ())) |}]

let%expect_test "corrects optimized branch" =
  let baz =
    let label = "baz" in
    let in_edges = [ Block.Edge.{ label = "foo"; type_ = Jump } ] in
    let statements = [| Statement.Nop |] in
    let branch = None in
    Block.{ label; statements; in_edges; branch }
  in
  let bar =
    let in_edges = [ Block.Edge.{ label = "foo"; type_ = Fallthrough } ] in
    Block.{ label = "bar"; statements = [| Nop |]; in_edges; branch = None }
  in
  let foo =
    let statements =
      [|
        Statement.Jump
          {
            target = Label "baz";
            (* should be simplified into an unconditional jump *)
            cond = Some { cmp = Eq; left = Const 1; right = Const 1 };
          };
      |]
    in
    let in_edges = [] in
    let branch =
      Some (Block.Branch.Conditional_jump { true_ = baz; false_ = bar })
    in
    Block.{ label = "foo"; statements; in_edges; branch }
  in
  printf "%b" (optimize foo);
  [%expect "true"];
  print_block foo;
  [%expect
    {|
    ((label foo) (statements ((Jump ((target (Label baz)) (cond ())))))
     (in_edges ())
     (branch
      ((Unconditional_jump
        ((label baz) (statements (Nop)) (in_edges (((label foo) (type_ Jump))))
         (branch ())))))) |}];
  print_block bar;
  (* there should be no fallthrough branch from foo *)
  [%expect {| ((label bar) (statements (Nop)) (in_edges ()) (branch ())) |}]