open Core

module rec Expression : sig
  type t =
    | Const of int
    | Label of string
    | Var of Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of Condition.t
  [@@deriving sexp_of, equal, compare, hash]

  val substitute : t -> from:t -> to_:t -> t * bool
  val contains : t -> Variable.t -> bool
end = struct
  type t =
    | Const of int
    | Label of string
    | Var of Variable.t
    | Add of t list
    | Sub of t * t
    | Getc
    | If of Condition.t
  [@@deriving sexp_of, equal, compare, hash]

  let rec substitute t ~from ~to_ =
    if equal t from
    then to_, true
    else (
      match t with
      | Const _ | Label _ | Getc -> t, false
      | Var v ->
        let v, changed = Variable.substitute v ~from ~to_ in
        Var v, changed
      | Add ts ->
        let changed, ts =
          List.fold_map ts ~init:false ~f:(fun changed t ->
            let t, t_changed = substitute t ~from ~to_ in
            changed || t_changed, t)
        in
        (* sort to maintain canonical order *)
        let ts = List.sort ts ~compare in
        Add ts, changed
      | Sub (t1, t2) ->
        let t1, t1_changed = substitute t1 ~from ~to_ in
        let t2, t2_changed = substitute t2 ~from ~to_ in
        Sub (t1, t2), t1_changed || t2_changed
      | If { cmp; left; right } ->
        let left, left_changed = substitute left ~from ~to_ in
        let right, right_changed = substitute right ~from ~to_ in
        If { cmp; left; right }, left_changed || right_changed)
  ;;

  let rec contains t var =
    match t with
    | Const _ | Label _ | Getc -> false
    | Var v -> Variable.equal v var
    | Add ts -> List.exists ts ~f:(fun t -> contains t var)
    | Sub (t1, t2) -> contains t1 var || contains t2 var
    | If { left; right; _ } -> contains left var || contains right var
  ;;

  (* let rec get_all_lhs_dependencies t = *)
  (*   match t with *)
  (*   | Const _ | Label _ | Getc -> Set.empty (module Variable) *)
  (*   | Var v -> Set.singleton (module Variable) v *)
  (*   | Add ts -> *)
  (*     List.map ts ~f:get_all_lhs_dependencies |> Set.union_list (module Variable) *)
  (*   | Sub (t1, t2) -> *)
  (*     Set.union (get_all_lhs_dependencies t1) (get_all_lhs_dependencies t2) *)
  (*   | If { left; right; _ } -> *)
  (*     Set.union (get_all_lhs_dependencies left) (get_all_lhs_dependencies right) *)
  (* ;; *)
end

and Comparison : sig
  type t =
    | Eq
    | Ne
    | Lt
    | Le
  [@@deriving sexp_of, equal, compare, hash]
end = struct
  type t =
    | Eq
    | Ne
    | Lt
    | Le
  [@@deriving sexp_of, equal, compare, hash]
end

and Condition : sig
  type t =
    { cmp : Comparison.t
    ; left : Expression.t
    ; right : Expression.t
    }
  [@@deriving sexp_of, equal, compare, hash]

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
end = struct
  type t =
    { cmp : Comparison.t
    ; left : Expression.t
    ; right : Expression.t
    }
  [@@deriving sexp_of, equal, compare, hash]

  let substitute t ~from ~to_ =
    let left, left_changed = Expression.substitute t.left ~from ~to_ in
    let right, right_changed = Expression.substitute t.right ~from ~to_ in
    { cmp = t.cmp; left; right }, left_changed || right_changed
  ;;
end

and Variable : sig
  type t =
    | Register of Eir.Register.t
    | Memory of Expression.t
  [@@deriving sexp_of, equal, compare, hash]

  val substitute : t -> from:Expression.t -> to_:Expression.t -> t * bool
  val contains : t -> t -> bool

  include Comparator.S with type t := t
end = struct
  type t =
    | Register of Eir.Register.t
    | Memory of Expression.t
  [@@deriving sexp_of, equal, compare, hash]

  let substitute t ~from ~to_ =
    match t, from, to_ with
    | _, Expression.Var from, Expression.Var to_ when equal t from -> to_, true
    | Register _, _, _ -> t, false
    | Memory expr, from, to_ ->
      let expr, expr_changed = Expression.substitute expr ~from ~to_ in
      Memory expr, expr_changed
  ;;

  let contains t var =
    if equal t var
    then true
    else (
      match t with
      | Register _ -> false
      | Memory expr -> Expression.contains expr var)
  ;;

  include Comparator.Make (struct
      type nonrec t = t [@@deriving sexp_of]

      let compare = compare
    end)
end

module Statement = struct
  module Assignment = struct
    type t =
      { dst : Variable.t
      ; src : Expression.t
      }
    [@@deriving sexp_of, equal, compare, hash]
  end

  module Jump = struct
    type t =
      { target : Expression.t
      ; cond : Condition.t option
      }
    [@@deriving sexp_of, equal, compare, hash]
  end

  type t =
    | Assign of Assignment.t
    | Putc of Expression.t
    | Jump of Jump.t
    | Exit
    | Nop
  [@@deriving sexp_of, equal, compare, hash]

  type assignment =
    { from : Variable.t
    ; to_ : Expression.t
    }
  [@@deriving sexp_of]

  (* let branch_type = function *)
  (*   | Exit -> None *)
  (*   | Assign _ | Putc _ | Nop -> Some Statement_intf.Branch_type.Fallthrough *)
  (*   | Jump { cond = None; _ } -> Some Unconditional_jump *)
  (*   | Jump { cond = Some _; _ } -> Some Conditional_jump *)
  (* ;; *)

  let substitute t ~from ~to_ =
    match t with
    | Assign { dst; src } ->
      let dst, dst_changed = Variable.substitute dst ~from ~to_ in
      let src, src_changed = Expression.substitute src ~from ~to_ in
      Assign { dst; src }, dst_changed || src_changed
    | Putc e ->
      let e, changed = Expression.substitute e ~from ~to_ in
      Putc e, changed
    | Jump { target; cond } ->
      let target, target_changed = Expression.substitute target ~from ~to_ in
      let cond, cond_changed =
        Option.value_map cond ~default:(None, false) ~f:(fun cond ->
          let cond, changed = Condition.substitute cond ~from ~to_ in
          Some cond, changed)
      in
      Jump { target; cond }, target_changed || cond_changed
    | Exit -> Exit, false
    | Nop -> Nop, false
  ;;
end
