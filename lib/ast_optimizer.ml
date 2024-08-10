open Core
open Ast

let rec optimize_until_stable ~opt ~equal e =
  let e' = opt e in
  match equal e e' with
  | false -> optimize_until_stable ~opt ~equal e'
  | true -> e
;;

module Expression : sig
  val optimize : Expression.t -> Expression.t
end = struct
  let rec optimize e = optimize_until_stable ~opt:optimize' ~equal:Expression.equal e

  and optimize' = function
    | Const _ as e -> e
    | Label _ as e -> e
    | Var (Register _) as e -> e
    | Getc -> Getc
    | Var (Memory addr) -> Var (Memory (optimize addr))
    | Add xs -> optimize_add xs
    | Sub (a, b) -> optimize_sub a b
    | If { cmp; left; right } -> optimize_if cmp left right

  and optimize_add xs =
    List.map xs ~f:optimize
    |> (* flatten nested adds *)
    List.concat_map ~f:(function
      | Add xs -> xs
      | x -> [ x ])
    |> function
    | [] -> Expression.Const 0
    | [ x ] -> x
    | xs ->
      (* constant folding *)
      let consts, non_consts =
        List.partition_map xs ~f:(function
          | Const c -> First c
          | e -> Second e)
      in
      let const_sum = List.sum (module Int) consts ~f:Fn.id in
      (if const_sum = 0 then non_consts else Const const_sum :: non_consts)
      (* sort to maintain canonical order *)
      |> List.sort ~compare:Expression.compare
      |> Expression.Add

  and optimize_sub a b =
    match optimize a, optimize b with
    | Const a, Const b -> Const (a - b)
    | a, Const b when b = 0 -> a
    | a, Const b -> Add [ a; Const (-b) ]
    | _ when Expression.equal a b -> Const 0
    | _ -> Sub (a, b)

  and optimize_if cmp left right =
    match optimize left, optimize right with
    | Const left, Const right ->
      (match cmp with
       | Comparison.Eq -> left = right
       | Ne -> left <> right
       | Lt -> left < right
       | Le -> left <= right)
      |> Bool.to_int
      |> Const
    | left, right ->
      let equal = Expression.equal left right in
      (match cmp with
       | Eq when equal -> Const 1
       | Ne when equal -> Const 0
       | Lt when equal -> Const 0
       | Le when equal -> Const 1
       | _ -> If { cmp; left; right })
  ;;
end

let optimize_expression = Expression.optimize

module Statement : sig
  val optimize : Statement.t -> Statement.t
end = struct
  let optimize_variable = function
    | Variable.Memory exp ->
      let exp = optimize_expression exp in
      Variable.Memory exp
    | Register _ as e -> e
  ;;

  let optimize' = function
    | Statement.Assign { dst; src } ->
      let dst = optimize_variable dst in
      let src = optimize_expression src in
      Statement.Assign { dst; src }
    | Putc exp -> Putc (optimize_expression exp)
    | Jump { target; cond } ->
      let target = optimize_expression target in
      let optimized_cond = Option.map cond ~f:(fun c -> optimize_expression (If c)) in
      (match optimized_cond with
       | Some (Const 1) -> Jump { target; cond = None }
       | Some (Const 0) -> Nop
       | _ -> Jump { target; cond })
    | Exit -> Exit
    | Nop -> Nop
  ;;

  let optimize = optimize_until_stable ~opt:optimize' ~equal:Statement.equal
end

let optimize_statement = Statement.optimize
