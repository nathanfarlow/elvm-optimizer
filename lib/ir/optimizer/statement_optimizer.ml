open Core
open Statement

module Make
    (Expression_optimizer : Optimizer_intf.S with type target := Expression.t) =
struct
  type t = Expression_optimizer.t

  let optimize_variable exp_opt = function
    | Memory exp ->
        let exp, changed = exp_opt exp in
        (Memory exp, changed)
    | Register reg -> (Register reg, false)

  let optimize' exp_opt = function
    | Assign { dst; src } ->
        let dst, dst_changed = optimize_variable exp_opt dst in
        let src, src_changed = exp_opt src in
        (Assign { dst; src }, dst_changed || src_changed)
    | Putc exp ->
        let exp, exp_changed = exp_opt exp in
        (Putc exp, exp_changed)
    | Jump { target; condition } -> (
        let target, target_changed = exp_opt target in
        let optimized_condition =
          Option.map condition ~f:(fun c -> exp_opt (Set c))
        in
        match optimized_condition with
        | Some (Const 1, _) -> (Jump { target; condition = None }, true)
        | Some (Const 0, _) -> (Nop, true)
        | _ -> (Jump { target; condition }, target_changed))
    | Exit -> (Exit, false)
    | Nop -> (Nop, false)

  let optimize expression_optimizer t =
    let optimizer_expr = Expression_optimizer.optimize expression_optimizer in
    let rec aux t changed_in_past =
      let t, just_changed = optimize' optimizer_expr t in
      if just_changed then aux t true else (t, changed_in_past || just_changed)
    in
    aux t false

  let create = Fn.id
end
