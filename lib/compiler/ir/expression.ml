module rec M : sig
  type t =
    | Const of int
    | Label of string
    | Var of M.Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of M.Condition.t
  [@@deriving sexp, hash]

  module Comparison : sig
    type t = Eq | Ne | Lt | Le [@@deriving sexp, equal, hash]
  end

  module Condition : sig
    type t = { cmp : Comparison.t; left : M.t; right : M.t }
    [@@deriving sexp, equal, hash]
  end

  module Variable : sig
    type t = Named of string | Memory of M.t [@@deriving sexp, equal, hash]
  end

  val equal : t -> t -> bool
  val references : t -> string Hash_set.t
end = struct
  type t =
    | Const of int
    | Label of string
    | Var of M.Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of M.Condition.t
  [@@deriving sexp, hash]

  module Comparison = struct
    type t = Eq | Ne | Lt | Le [@@deriving sexp, equal, hash]
  end

  module Condition = struct
    type t = { cmp : Comparison.t; left : M.t; right : M.t }
    [@@deriving sexp, equal, hash]
  end

  module Variable = struct
    type t = Named of string | Memory of M.t [@@deriving sexp, equal, hash]
  end

  let rec equal a b =
    match (a, b) with
    | Const x, Const y -> x = y
    | Var x, Var y -> Variable.equal x y
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
    | ( If { cmp = c1; left = a1; right = b1 },
        If { cmp = c2; left = a2; right = b2 } ) ->
        Comparison.equal c1 c2 && equal a1 a2 && equal b1 b2
    | _ -> false

  let references t =
    let set = Hash_set.create (module String) in
    let rec aux = function
      | Label label -> Hash_set.add set label
      | Var (Memory addr) -> aux addr
      | Add xs -> List.iter xs ~f:aux
      | Sub (x, y) ->
          aux x;
          aux y
      | If { left; right; _ } ->
          aux left;
          aux right
      | _ -> ()
    in
    aux t;
    set
end

include M