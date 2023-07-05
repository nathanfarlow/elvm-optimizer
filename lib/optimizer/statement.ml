open Core

type variable = Memory of Expression.t | Register of Register.t
[@@deriving sexp, equal]

type assignment = { dst : variable; src : Expression.t }
[@@deriving sexp, equal]

type jump = { target : Expression.t; condition : Expression.condition option }
[@@deriving sexp, equal]

type t =
  | Assign of assignment
  | Putc of Expression.t
  | Jump of jump
  | Exit
  | Nop
[@@deriving sexp, equal]

let optimize_variable = function
  | Memory exp ->
      let exp, changed = Expression.optimize exp in
      (Memory exp, changed)
  | Register reg -> (Register reg, false)

let optimize' = function
  | Assign { dst; src } ->
      let dst, dst_changed = optimize_variable dst in
      let src, src_changed = Expression.optimize src in
      (Assign { dst; src }, dst_changed || src_changed)
  | Putc exp ->
      let exp, exp_changed = Expression.optimize exp in
      (Putc exp, exp_changed)
  | Jump { target; condition } -> (
      let target, target_changed = Expression.optimize target in
      let optimized_condition =
        Option.map condition ~f:(fun c -> Expression.optimize (Set c))
      in
      match optimized_condition with
      | Some (Const 1, _) -> (Jump { target; condition = None }, true)
      | Some (Const 0, _) -> (Nop, true)
      | _ -> (Jump { target; condition }, target_changed))
  | Exit -> (Exit, false)
  | Nop -> (Nop, false)

let optimize t =
  let rec aux t changed_in_past =
    let t, just_changed = optimize' t in
    if just_changed then aux t true else (t, changed_in_past || just_changed)
  in
  aux t false
