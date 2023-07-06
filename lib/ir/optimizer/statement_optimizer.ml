open Core
open Statement

module Make
    (Expression_optimizer : Optimizer_intf.S with type target := Expression.t) =
struct
  type t = Expression_optimizer.t

  let optimize_variable optimize_exp = function
    | Memory exp ->
        let exp, changed = optimize_exp exp in
        (Memory exp, changed)
    | Register reg -> (Register reg, false)

  let optimize' optimize_exp = function
    | Assign { dst; src } ->
        let dst, dst_changed = optimize_variable optimize_exp dst in
        let src, src_changed = optimize_exp src in
        (Assign { dst; src }, dst_changed || src_changed)
    | Putc exp ->
        let exp, exp_changed = optimize_exp exp in
        (Putc exp, exp_changed)
    | Jump { target; condition } -> (
        let target, target_changed = optimize_exp target in
        let optimized_condition =
          Option.map condition ~f:(fun c -> optimize_exp (Set c))
        in
        match optimized_condition with
        | Some (Const 1, _) -> (Jump { target; condition = None }, true)
        | Some (Const 0, _) -> (Nop, true)
        | _ -> (Jump { target; condition }, target_changed))
    | Exit -> (Exit, false)
    | Nop -> (Nop, false)

  let optimize expression_optimizer t =
    let optimizer_expr = Expression_optimizer.optimize expression_optimizer in
    Optimizer_util.optimize_until_unchanging (optimize' optimizer_expr) t

  let create = Fn.id
end
