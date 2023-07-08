open Core

type variable = Memory of Expression.t | Register of Register.t
[@@deriving sexp, equal]

type assignment = { dst : variable; src : Expression.t }
[@@deriving sexp, equal]

type jump = { target : Expression.t; condition : Expression.condition option }
[@@deriving sexp, equal]

type t =
  | Assign of assignment
  | Putc of Expression.t
  | Jump of jump
  | Exit
  | Nop
[@@deriving sexp, equal]

let references t =
  let set = Hash_set.create (module String) in
  let add_all = Hash_set.iter ~f:(Hash_set.add set) in
  let add_variable_refs = function
    | Memory exp -> add_all @@ Expression.references exp
    | Register _ -> ()
  in
  let add_condition_refs cond = add_all @@ Expression.references (Set cond) in
  (match t with
  | Assign { dst; src } ->
      add_variable_refs dst;
      add_all @@ Expression.references src
  | Putc exp -> add_all @@ Expression.references exp
  | Jump { target; condition } ->
      add_all @@ Expression.references target;
      Option.iter condition ~f:add_condition_refs
  | Exit | Nop -> ());
  set
