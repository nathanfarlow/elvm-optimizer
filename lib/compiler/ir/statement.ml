module Assignment = struct
  type t = { dst : Expression.Variable.t; src : Expression.t }
  [@@deriving sexp, equal, hash]
end

module Jump = struct
  type t = { target : Expression.t; cond : Expression.Condition.t option }
  [@@deriving sexp, equal, hash]
end

type t =
  | Assign of Assignment.t
  | Putc of Expression.t
  | Jump of Jump.t
  | Exit
  | Nop
[@@deriving sexp, equal, hash]

let references t =
  let set = Hash_set.create (module String) in
  let add_all = Hash_set.iter ~f:(Hash_set.add set) in
  let add_assignment_refs = function
    | Expression.Variable.Memory exp -> add_all @@ Expression.references exp
    | Local _ -> ()
  in
  let add_condition_refs cond = add_all @@ Expression.references (If cond) in
  (match t with
  | Assign { dst; src } ->
      add_assignment_refs dst;
      add_all @@ Expression.references src
  | Putc exp -> add_all @@ Expression.references exp
  | Jump { target; cond } ->
      add_all @@ Expression.references target;
      Option.iter cond ~f:add_condition_refs
  | Exit | Nop -> ());
  set