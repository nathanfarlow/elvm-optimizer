open Core
open Expression

type t = unit

let rec optimize' = function
  | Const _ as e -> (e, false)
  | Label _ as e -> (e, false)
  | Register _ as e -> (e, false)
  | Memory addr ->
      let addr, did_change = optimize' addr in
      (Memory addr, did_change)
  | Add xs -> optimize_add xs
  | Sub (a, b) -> optimize_sub a b
  | Getc -> (Getc, false)
  | Set { comparison; a; b } -> optimize_set comparison a b

and optimize_add xs =
  (* flatten adds *)
  let xs = List.concat_map xs ~f:(function Add xs -> xs | x -> [ x ]) in
  match xs with
  | [] -> (Const 0, true)
  | [ x ] -> optimize' x
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
        List.map non_consts ~f:optimize' |> List.unzip
      in
      let did_others_change = List.exists non_consts_changes ~f:Fn.id in
      let optimized_terms =
        if constant_sum = 0 then Add non_consts
        else Add (Const constant_sum :: non_consts)
      in
      (optimized_terms, did_others_change || did_constants_change)

and optimize_sub a b =
  let a, a_changed = optimize' a in
  let b, b_changed = optimize' b in
  match (a, b) with
  | Const a, Const b -> (Const (a - b), true)
  | a, Const b when b = 0 -> (a, true)
  | a, Const b -> (Add [ a; Const (-b) ], true)
  | _ when equal a b -> (Const 0, true)
  | _ -> (Sub (a, b), a_changed || b_changed)

and optimize_set comparison a b =
  let a, a_changed = optimize' a in
  let b, b_changed = optimize' b in
  match (a, b) with
  | Const a, Const b ->
      let const_compare comparison a b =
        match comparison with
        | Eq -> a = b
        | Ne -> a <> b
        | Lt -> a < b
        | Le -> a <= b
      in
      (Const (if const_compare comparison a b then 1 else 0), true)
  | _ -> (
      let equal = equal a b in
      match comparison with
      | Eq when equal -> (Const 1, true)
      | Ne when equal -> (Const 0, true)
      | Lt when equal -> (Const 0, true)
      | Le when equal -> (Const 1, true)
      | _ -> (Set { comparison; a; b }, a_changed || b_changed))

let optimize _ = Optimizer_util.optimize_until_unchanging optimize'
let create () = ()