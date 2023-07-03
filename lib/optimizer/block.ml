open Core

type t = {
  id : int;
  statements : Statement.t Deque.t;
}

let get_statements t = t.statements
let get_id t = t.id