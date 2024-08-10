module rec Expression : sig
  type t =
    | Const of int
    | Label of string
    | Var of Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of Condition.t
  [@@deriving sexp, equal, compare, hash]

  include Environment_rhs_intf.S with type t := t and type lhs := Variable.t

  include
    Liveness_analyzer_rhs_intf.S with type t := t and type lhs := Variable.t

  val substitute : t -> from:t -> to_:t -> t * bool
end = struct
  module Lhs = Variable
  module Lhs_set = Set.Make (Lhs)

  type t =
    | Const of int
    | Label of string
    | Var of Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of Condition.t
  [@@deriving sexp, equal, compare, hash]

  let rec substitute t ~from ~to_ =
    if equal t from then (to_, true)
    else
      match t with
      | Const _ | Label _ | Getc -> (t, false)
      | Var v ->
          let v, changed = Variable.substitute v ~from ~to_ in
          (Var v, changed)
      | Add ts ->
          let changed, ts =
            List.fold_map ts ~init:false ~f:(fun changed t ->
                let t, t_changed = substitute t ~from ~to_ in
                (changed || t_changed, t))
          in
          (* sort to maintain canonical order *)
          let ts = List.sort ts ~compare in
          (Add ts, changed)
      | Sub (t1, t2) ->
          let t1, t1_changed = substitute t1 ~from ~to_ in
          let t2, t2_changed = substitute t2 ~from ~to_ in
          (Sub (t1, t2), t1_changed || t2_changed)
      | If { cmp; left; right } ->
          let left, left_changed = substitute left ~from ~to_ in
          let right, right_changed = substitute right ~from ~to_ in
          (If { cmp; left; right }, left_changed || right_changed)

  let rec contains t var =
    match t with
    | Const _ | Label _ | Getc -> false
    | Var v -> Variable.equal v var
    | Add ts -> List.exists ts ~f:(fun t -> contains t var)
    | Sub (t1, t2) -> contains t1 var || contains t2 var
    | If { left; right; _ } -> contains left var || contains right var

  let rec get_all_lhs_dependencies t =
    match t with
    | Const _ | Label _ | Getc -> Lhs_set.empty
    | Var v -> Lhs_set.singleton v
    | Add ts -> List.map ts ~f:get_all_lhs_dependencies |> Lhs_set.union_list
    | Sub (t1, t2) ->
        Set.union (get_all_lhs_dependencies t1) (get_all_lhs_dependencies t2)
    | If { left; right; _ } ->
        Set.union
          (get_all_lhs_dependencies left)
          (get_all_lhs_dependencies right)
end

and Comparison : sig
  type t = Eq | Ne | Lt | Le [@@deriving sexp, equal, compare, hash]
end = struct
  type t = Eq | Ne | Lt | Le [@@deriving sexp, equal, compare, hash]
end

and Condition : sig
  type t = { cmp : Comparison.t; left : Expression.t; right : Expression.t }
  [@@deriving sexp, equal, compare, hash]

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
end = struct
  type t = { cmp : Comparison.t; left : Expression.t; right : Expression.t }
  [@@deriving sexp, equal, compare, hash]

  let substitute t ~from ~to_ =
    let left, left_changed = Expression.substitute t.left ~from ~to_ in
    let right, right_changed = Expression.substitute t.right ~from ~to_ in
    ({ cmp = t.cmp; left; right }, left_changed || right_changed)
end

and Variable : sig
  type t = Register of Eir.Register.t | Memory of Expression.t
  [@@deriving sexp, equal, compare, hash]

  include Environment_lhs_intf.S with type t := t
  include Liveness_analyzer_lhs_intf.S with type t := t

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
end = struct
  type t = Register of Eir.Register.t | Memory of Expression.t
  [@@deriving sexp, equal, compare, hash]

  let substitute t ~from ~to_ =
    match (t, from, to_) with
    | _, Expression.Var from, Expression.Var to_ when equal t from -> (to_, true)
    | Register _, _, _ -> (t, false)
    | Memory expr, from, to_ ->
        let expr, expr_changed = Expression.substitute expr ~from ~to_ in
        (Memory expr, expr_changed)

  let contains t var =
    if equal t var then true
    else
      match t with
      | Register _ -> false
      | Memory expr -> Expression.contains expr var
end
