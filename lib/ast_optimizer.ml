open Core
open Ast

let optimize_until_stable optimize e =
  let rec loop e has_changed =
    match optimize e with
    | e', true -> loop e' true
    | _, false -> e, has_changed
  in
  loop e false
;;

module Expression = struct
  let rec optimize e = optimize_until_stable optimize' e

  and optimize' (e : Expression.t) =
    match e with
    | Const _ as e -> e, false
    | Label _ as e -> e, false
    | Var (Register _) as e -> e, false
    | Var (Memory addr) ->
      let addr, did_change = optimize addr in
      Var (Memory addr), did_change
    | Add xs -> optimize_add xs
    | Sub (a, b) -> optimize_sub a b
    | Getc -> Getc, false
    | If { cmp; left; right } -> optimize_if cmp left right

  and optimize_add xs =
    let xs, xs_changed = List.map xs ~f:optimize |> List.unzip in
    let xs_changed = List.exists xs_changed ~f:Fn.id in
    (* flatten nested adds *)
    let xs =
      List.concat_map xs ~f:(function
        | Add xs -> xs
        | x -> [ x ])
    in
    match xs with
    | [] -> Const 0, true
    | [ x ] -> x, true
    | _ ->
      (* constant folding *)
      let consts, non_consts =
        List.partition_tf
          ~f:(function
            | Const _ -> true
            | _ -> false)
          xs
      in
      let const_sum =
        List.fold consts ~init:0 ~f:(fun acc ->
            function
            | Const x -> acc + x
            | _ -> assert false)
      in
      let optimized_terms =
        (if const_sum = 0 then non_consts else Const const_sum :: non_consts)
        (* sort to maintain canonical order *)
        |> List.sort ~compare:Expression.compare
      in
      let did_delete_consts = List.length optimized_terms <> List.length xs in
      Add optimized_terms, xs_changed || did_delete_consts

  and optimize_sub a b =
    let a, a_changed = optimize a in
    let b, b_changed = optimize b in
    match a, b with
    | Const a, Const b -> Const (a - b), true
    | a, Const b when b = 0 -> a, true
    | a, Const b -> Add [ a; Const (-b) ], true
    | _ when Expression.equal a b -> Const 0, true
    | _ -> Sub (a, b), a_changed || b_changed

  and optimize_if cmp left right =
    let left, left_changed = optimize left in
    let right, right_changed = optimize right in
    match left, right with
    | Const left, Const right ->
      let cmp_result =
        match cmp with
        | Comparison.Eq -> left = right
        | Ne -> left <> right
        | Lt -> left < right
        | Le -> left <= right
      in
      Const (Bool.to_int cmp_result), true
    | _ ->
      let equal = Expression.equal left right in
      (match cmp with
       | Eq when equal -> Const 1, true
       | Ne when equal -> Const 0, true
       | Lt when equal -> Const 0, true
       | Le when equal -> Const 1, true
       | _ -> If { cmp; left; right }, left_changed || right_changed)
  ;;
end

let optimize_expression = Expression.optimize

module Statement = struct
  let optimize_variable = function
    | Variable.Memory exp ->
      let exp, changed = optimize_expression exp in
      Variable.Memory exp, changed
    | Register _ as e -> e, false
  ;;

  let optimize' = function
    | Statement.Assign { dst; src } ->
      let dst, dst_changed = optimize_variable dst in
      let src, src_changed = optimize_expression src in
      Statement.Assign { dst; src }, dst_changed || src_changed
    | Putc exp ->
      let exp, exp_changed = optimize_expression exp in
      Putc exp, exp_changed
    | Jump { target; cond } ->
      let target, target_changed = optimize_expression target in
      let optimized_cond = Option.map cond ~f:(fun c -> optimize_expression (If c)) in
      (match optimized_cond with
       | Some (Const 1, _) -> Jump { target; cond = None }, true
       | Some (Const 0, _) -> Nop, true
       | _ -> Jump { target; cond }, target_changed)
    | Exit -> Exit, false
    | Nop -> Nop, false
  ;;

  let optimize = optimize_until_stable optimize'
end

let optimize_statement = Statement.optimize
