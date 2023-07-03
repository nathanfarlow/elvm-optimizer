open Core

type comparison = Eq | Ne | Lt | Le [@@deriving sexp, equal]

and condition = { comparison : comparison; a : t; b : t }
[@@deriving sexp, equal]

and t =
  | Const of int
  | Register of Elvm_instruction.register
  | Memory of t
  | Add of t list
  | Sub of t * t
  | Getc
  | Set of condition
[@@deriving sexp]

let const_compare comparison a b =
  match comparison with
  | Eq -> a = b
  | Ne -> a <> b
  | Lt -> a < b
  | Le -> a <= b

let rec equal a b =
  match (a, b) with
  | Const x, Const y -> x = y
  | Register x, Register y -> Elvm_instruction.equal_register x y
  | Memory x, Memory y -> equal x y
  | Add xs, Add ys ->
      if List.length xs <> List.length ys then false
      else
        let used_ys = Array.create ~len:(List.length ys) false in
        let find_match x =
          let rec find_match' i = function
            | [] -> None
            | y :: ys ->
                if (not used_ys.(i)) && equal x y then (
                  used_ys.(i) <- true;
                  Some y)
                else find_match' (i + 1) ys
          in
          find_match' 0 ys
        in
        List.for_all xs ~f:(fun x -> Option.is_some (find_match x))
  | Sub (x1, y1), Sub (x2, y2) -> equal x1 x2 && equal y1 y2
  | Getc, Getc -> true
  | ( Set { comparison = c1; a = a1; b = b1 },
      Set { comparison = c2; a = a2; b = b2 } ) ->
      equal_comparison c1 c2 && equal a1 a2 && equal b1 b2
  | _ -> false

let rec simplify' = function
  | Const _ as x -> (x, false)
  | Register _ as x -> (x, false)
  | Memory addr ->
      let x, did_change = simplify' addr in
      (Memory x, did_change)
  | Add xs -> simplify_add xs
  | Sub (x, y) -> simplify_sub x y
  | Getc -> (Getc, false)
  | Set { comparison; a; b } -> simplify_set comparison a b

and simplify_add xs =
  let xs = List.concat_map xs ~f:(function Add xs -> xs | x -> [ x ]) in
  match xs with
  | [] -> (Const 0, false)
  | [ x ] -> simplify' x
  | _ ->
      let constants, others =
        List.partition_tf ~f:(function Const _ -> true | _ -> false) xs
      in
      let constant_sum =
        List.fold constants ~init:0 ~f:(fun acc x ->
            match x with Const x -> acc + x | _ -> assert false)
      in
      let did_simp_const = List.length constants > 1 in
      let others, other_changes = List.map others ~f:simplify' |> List.unzip in
      let did_change = did_simp_const || List.exists other_changes ~f:Fn.id in
      (Add (Const constant_sum :: others), did_change)

and simplify_sub x y =
  let x, x_changed = simplify' x in
  let y, y_changed = simplify' y in
  match (x, y) with
  | Const x, Const y -> (Const (x - y), true)
  | _ when equal x y -> (Const 0, true)
  | _ -> (Sub (x, y), x_changed || y_changed)

and simplify_set comparison a b =
  let a, a_changed = simplify' a in
  let b, b_changed = simplify' b in
  match (a, b) with
  | Const a, Const b ->
      (Const (if const_compare comparison a b then 1 else 0), true)
  | _ -> (
      let equal = equal a b in
      match comparison with
      | Eq when equal -> (Const 1, true)
      | Ne when equal -> (Const 0, true)
      | Lt when equal -> (Const 0, true)
      | Le when equal -> (Const 1, true)
      | _ -> (Set { comparison; a; b }, a_changed || b_changed))

let rec simplify t =
  let t, did_change = simplify' t in
  if did_change then simplify t else t
