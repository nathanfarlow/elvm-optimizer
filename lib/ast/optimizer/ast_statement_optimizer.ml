open Core

module Make (Expression_optimizer : Optimizer_intf.S with type target := Ast.Expression.t) =
struct
  type t = Expression_optimizer.t

  let optimize_variable optimize_exp = function
    | Ast.Variable.Memory exp ->
      let exp, changed = optimize_exp exp in
      Ast.Variable.Memory exp, changed
    | Register _ as e -> e, false
  ;;

  let optimize' optimize_exp = function
    | Ast.Statement.Assign { dst; src } ->
      let dst, dst_changed = optimize_variable optimize_exp dst in
      let src, src_changed = optimize_exp src in
      Ast.Statement.Assign { dst; src }, dst_changed || src_changed
    | Putc exp ->
      let exp, exp_changed = optimize_exp exp in
      Putc exp, exp_changed
    | Jump { target; cond } ->
      let target, target_changed = optimize_exp target in
      let optimized_cond = Option.map cond ~f:(fun c -> optimize_exp (If c)) in
      (match optimized_cond with
       | Some (Const 1, _) -> Jump { target; cond = None }, true
       | Some (Const 0, _) -> Nop, true
       | _ -> Jump { target; cond }, target_changed)
    | Exit -> Exit, false
    | Nop -> Nop, false
  ;;

  let optimize expression_optimizer stmt =
    let optimizer_expr = Expression_optimizer.optimize expression_optimizer in
    Optimizer_util.optimize_until_unchanging (optimize' optimizer_expr) stmt
  ;;

  let create = Fn.id
end
