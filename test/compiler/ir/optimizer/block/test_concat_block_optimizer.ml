open Elvm.Compiler.Ir
open Elvm.Compiler.Optimizer

module Mock_reference_provider : sig
  type t

  include Reference_provider_intf.S with type t := t

  val create : unit -> t
end = struct
  type t = unit

  let create = Fn.id
  let has_reference _ label = String.equal label "block_with_ref"
  let all_references _ = assert false
end

module Block_opt = Concat_block_optimizer.Make (Mock_reference_provider)

let optimizer = Mock_reference_provider.create () |> Block_opt.create
let print_block result = print_s [%sexp (result : Block.t)]
let optimize = Block_opt.optimize optimizer

let%expect_test "simplifies unconditional jump to target which has no \
                 fallthrough edges" =
  let target =
    Block.
      {
        label = "block_with_ref";
        statements = [| Exit |];
        in_edges = [ { label = "parent"; type_ = Jump } ];
        branch = None;
      }
  in
  let parent =
    Block.
      {
        label = "parent";
        statements =
          [| Statement.Jump { target = Label "block_with_ref"; cond = None } |];
        in_edges = [];
        branch = Some (Unconditional_jump target);
      }
  in
  optimize parent |> printf "%b";
  [%expect {| true |}];
  print_block parent;
  [%expect
    {|
    ((label parent) (statements ()) (in_edges ())
     (branch
      ((Fallthrough
        ((label block_with_ref) (statements (Exit))
         (in_edges (((label parent) (type_ Fallthrough)))) (branch ())))))) |}]

let%expect_test "does not simplify unconditional jump to target which has \
                 fallthrough edge" =
  let target =
    Block.
      {
        label = "block_with_ref";
        statements = [| Exit |];
        in_edges =
          [
            { label = "parent"; type_ = Jump };
            { label = "other_fallthrough"; type_ = Fallthrough };
          ];
        branch = None;
      }
  in
  let parent =
    Block.
      {
        label = "parent";
        statements =
          [| Statement.Jump { target = Label "block_with_ref"; cond = None } |];
        in_edges = [];
        branch = Some (Unconditional_jump target);
      }
  in
  optimize parent |> printf "%b";
  [%expect {| false |}];
  print_block parent;
  [%expect
    {|
    ((label parent)
     (statements ((Jump ((target (Label block_with_ref)) (cond ())))))
     (in_edges ())
     (branch
      ((Unconditional_jump
        ((label block_with_ref) (statements (Exit))
         (in_edges
          (((label parent) (type_ Jump))
           ((label other_fallthrough) (type_ Fallthrough))))
         (branch ())))))) |}]

let%expect_test "concatenates fallthrough blocks with no references" =
  let target =
    Block.
      {
        label = "block_with_no_ref";
        statements = [| Exit |];
        in_edges = [ { label = "parent"; type_ = Fallthrough } ];
        branch = None;
      }
  in
  let parent =
    Block.
      {
        label = "parent";
        statements = [| Nop |];
        in_edges = [];
        branch = Some (Fallthrough target);
      }
  in
  optimize parent |> printf "%b";
  [%expect {| true |}];
  print_block parent;
  [%expect
    {| ((label parent) (statements (Nop Exit)) (in_edges ()) (branch ())) |}]

let%expect_test "does not concatenate fallthrough blocks with references" =
  let target =
    Block.
      {
        label = "block_with_ref";
        statements = [| Exit |];
        in_edges = [ { label = "parent"; type_ = Fallthrough } ];
        branch = None;
      }
  in
  let parent =
    Block.
      {
        label = "parent";
        statements = [| Nop |];
        in_edges = [];
        branch = Some (Fallthrough target);
      }
  in
  optimize parent |> printf "%b";
  [%expect {| false |}];
  print_block parent;
  [%expect
    {|
    ((label parent) (statements (Nop)) (in_edges ())
     (branch
      ((Fallthrough
        ((label block_with_ref) (statements (Exit))
         (in_edges (((label parent) (type_ Fallthrough)))) (branch ())))))) |}]