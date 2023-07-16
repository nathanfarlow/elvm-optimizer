type t = unit

let rec optimize t = Optimizer_util.optimize_until_unchanging (optimize' t)

and optimize' t (e : Ast.Expression.t) =
  match e with
  | Const _ as e -> (e, false)
  | Label _ as e -> (e, false)
  | Var (Register _) as e -> (e, false)
  | Var (Memory addr) ->
      let addr, did_change = optimize t addr in
      (Var (Memory addr), did_change)
  | Add xs -> optimize_add t xs
  | Sub (a, b) -> optimize_sub t a b
  | Getc -> (Getc, false)
  | If { cmp; left; right } -> optimize_if t cmp left right

and optimize_add t xs =
  let xs, xs_changed = List.map xs ~f:(optimize t) |> List.unzip in
  let xs_changed = List.exists xs_changed ~f:Fn.id in
  (* flatten nested adds *)
  let xs = List.concat_map xs ~f:(function Add xs -> xs | x -> [ x ]) in
  match xs with
  | [] -> (Const 0, true)
  | [ x ] -> (x, true)
  | _ ->
      (* constant folding *)
      let consts, non_consts =
        List.partition_tf ~f:(function Const _ -> true | _ -> false) xs
      in
      let const_sum =
        List.fold consts ~init:0 ~f:(fun acc -> function
          | Const x -> acc + x | _ -> assert false)
      in
      let optimized_terms =
        (if const_sum = 0 then non_consts else Const const_sum :: non_consts)
        (* sort to maintain canonical order *)
        |> List.sort ~compare:Ast.Expression.compare
      in
      let did_delete_consts = List.length optimized_terms <> List.length xs in
      (Add optimized_terms, xs_changed || did_delete_consts)

and optimize_sub t a b =
  let a, a_changed = optimize t a in
  let b, b_changed = optimize t b in
  match (a, b) with
  | Const a, Const b -> (Const (a - b), true)
  | a, Const b when b = 0 -> (a, true)
  | a, Const b -> (Add [ a; Const (-b) ], true)
  | _ when Ast.Expression.equal a b -> (Const 0, true)
  | _ -> (Sub (a, b), a_changed || b_changed)

and optimize_if t cmp left right =
  let left, left_changed = optimize t left in
  let right, right_changed = optimize t right in
  match (left, right) with
  | Const left, Const right ->
      let cmp_result =
        match cmp with
        | Ast.Comparison.Eq -> left = right
        | Ne -> left <> right
        | Lt -> left < right
        | Le -> left <= right
      in
      (Const (Bool.to_int cmp_result), true)
  | _ -> (
      let equal = Ast.Expression.equal left right in
      match cmp with
      | Eq when equal -> (Const 1, true)
      | Ne when equal -> (Const 0, true)
      | Lt when equal -> (Const 0, true)
      | Le when equal -> (Const 1, true)
      | _ -> (If { cmp; left; right }, left_changed || right_changed))

let create () = ()