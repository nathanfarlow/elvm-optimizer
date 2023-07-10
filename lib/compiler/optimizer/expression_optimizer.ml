open Expression

type t = unit

let rec optimize t = Optimizer_util.optimize_until_unchanging (optimize' t)

and optimize' t = function
  | Const _ as e -> (e, false)
  | Label _ as e -> (e, false)
  | Var (Named _) as e -> (e, false)
  | Var (Register _) as e -> (e, false)
  | Var (Memory addr) ->
      let addr, did_change = optimize t addr in
      (Var (Memory addr), did_change)
  | Add xs -> optimize_add t xs
  | Sub (a, b) -> optimize_sub t a b
  | Getc -> (Getc, false)
  | If { cmp; left; right } -> optimize_set t cmp left right

and optimize_add t xs =
  (* flatten adds *)
  let xs = List.concat_map xs ~f:(function Add xs -> xs | x -> [ x ]) in
  match xs with
  | [] -> (Const 0, true)
  | [ x ] -> optimize t x
  | _ ->
      let consts, non_consts =
        List.partition_tf ~f:(function Const _ -> true | _ -> false) xs
      in
      (* flatten constants *)
      let constant_sum =
        List.fold consts ~init:0 ~f:(fun acc -> function
          | Const x -> acc + x | _ -> assert false)
      in
      let did_constants_change =
        (* don't add with 0 *)
        (constant_sum = 0 && List.length consts >= 1)
        (* simplified more than one constant to 1 *)
        || List.length consts > 1
      in
      (* optimize non constants *)
      let non_consts, non_consts_changes =
        List.map non_consts ~f:(optimize t) |> List.unzip
      in
      let did_others_change = List.exists non_consts_changes ~f:Fn.id in
      let optimized_terms =
        if constant_sum = 0 then Add non_consts
        else Add (Const constant_sum :: non_consts)
      in
      (optimized_terms, did_others_change || did_constants_change)

and optimize_sub t a b =
  let a, a_changed = optimize t a in
  let b, b_changed = optimize t b in
  match (a, b) with
  | Const a, Const b -> (Const (a - b), true)
  | a, Const b when b = 0 -> (a, true)
  | a, Const b -> (Add [ a; Const (-b) ], true)
  | _ when equal a b -> (Const 0, true)
  | _ -> (Sub (a, b), a_changed || b_changed)

and optimize_set t cmp left right =
  let left, left_changed = optimize t left in
  let right, right_changed = optimize t right in
  match (left, right) with
  | Const left, Const right ->
      let cmp_result =
        match cmp with
        | Comparison.Eq -> left = right
        | Ne -> left <> right
        | Lt -> left < right
        | Le -> left <= right
      in
      (Const (if cmp_result then 1 else 0), true)
  | _ -> (
      let equal = equal left right in
      match cmp with
      | Eq when equal -> (Const 1, true)
      | Ne when equal -> (Const 0, true)
      | Lt when equal -> (Const 0, true)
      | Le when equal -> (Const 1, true)
      | _ -> (If { cmp; left; right }, left_changed || right_changed))

let create () = ()