open Core
open Statement

module Make
    (Expression_optimizer : Optimizer_intf.S with type target := Expression.t) =
struct
  type t = Expression_optimizer.t

  let optimize_variable optimize_exp = function
    | Expression.Variable.Memory exp ->
        let exp, changed = optimize_exp exp in
        (Expression.Variable.Memory exp, changed)
    | (Named _ | Register _) as e -> (e, false)

  let optimize' optimize_exp = function
    | Statement.Assign { dst; src } ->
        let dst, dst_changed = optimize_variable optimize_exp dst in
        let src, src_changed = optimize_exp src in
        (Assign { dst; src }, dst_changed || src_changed)
    | Putc exp ->
        let exp, exp_changed = optimize_exp exp in
        (Putc exp, exp_changed)
    | Jump { target; cond } -> (
        let target, target_changed = optimize_exp target in
        let optimized_condition =
          Option.map cond ~f:(fun c -> optimize_exp (If c))
        in
        match optimized_condition with
        | Some (Const 1, _) -> (Jump { target; cond = None }, true)
        | Some (Const 0, _) -> (Nop, true)
        | _ -> (Jump { target; cond }, target_changed))
    | Push exp ->
        let exp, exp_changed = optimize_exp exp in
        (Push exp, exp_changed)
    | Pop dst ->
        let dst, dst_changed = optimize_variable optimize_exp dst in
        (Pop dst, dst_changed)
    | Enter _ as e -> (e, false)
    | Return -> (Return, false)
    | Call { label; args } -> (
        let args, args_changed =
          List.map args ~f:(fun arg ->
              let arg, arg_changed = optimize_exp arg in
              (arg, arg_changed))
          |> List.unzip
        in
        let args_changed = List.exists args_changed ~f:Fn.id in
        match args_changed with
        | true -> (Call { label; args }, true)
        | false -> (Call { label; args }, false))
    | Exit -> (Exit, false)
    | Nop -> (Nop, false)

  let optimize expression_optimizer stmt =
    let optimizer_expr = Expression_optimizer.optimize expression_optimizer in
    Optimizer_util.optimize_until_unchanging (optimize' optimizer_expr) stmt

  let create = Fn.id
end