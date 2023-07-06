open Core

type comparison = Eq | Ne | Lt | Le [@@deriving sexp, equal]

and condition = { comparison : comparison; a : t; b : t }
[@@deriving sexp, equal]

and t =
  | Const of int
  | Label of string
  | Register of Register.t
  | Memory of t
  | Add of t list
  | Sub of t * t
  | Getc
  | Set of condition
[@@deriving sexp]

let rec equal a b =
  match (a, b) with
  | Const x, Const y -> x = y
  | Register x, Register y -> Register.equal x y
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

let references t =
  let rec aux = function
    | Label label -> [ label ]
    | Memory addr -> aux addr
    | Add xs -> List.concat_map xs ~f:aux
    | Sub (x, y) -> aux x @ aux y
    | Set { a; b; _ } -> aux a @ aux b
    | _ -> []
  in
  List.dedup_and_sort ~compare:String.compare (aux t)