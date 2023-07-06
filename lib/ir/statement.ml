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

let references_variable = function
  | Memory exp -> Expression.references exp
  | Register _ -> []

let references_condition cond = Expression.references (Set cond)

let references t =
  let aux = function
    | Assign { dst; src } -> Expression.references src @ references_variable dst
    | Putc exp -> Expression.references exp
    | Jump { target; condition } ->
        Expression.references target
        @ Option.value_map condition ~default:[] ~f:references_condition
    | Exit | Nop -> []
  in
  List.dedup_and_sort ~compare:String.compare (aux t)