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

  include Propagator_rhs_intf.S with type t := t and type lhs := Variable.t

  val substitute : t -> from:t -> to_:t -> t * bool
end = struct
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
end

and Comparison : sig
  type t = Eq | Ne | Lt | Le [@@deriving sexp, equal, compare, hash]
end =
  Comparison

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

  include Propagator_lhs_intf.S with type t := t

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
end = struct
  type t = Register of Eir.Register.t | Memory of Expression.t
  [@@deriving sexp, equal, compare, hash]

  let substitute t ~from ~to_ =
    match t with
    | Register _ -> (t, false)
    | Memory expr ->
        let expr, expr_changed = Expression.substitute expr ~from ~to_ in
        (Memory expr, expr_changed)

  let contains t var =
    match t with
    | Register _ -> false
    | Memory expr -> Expression.contains expr var
end
