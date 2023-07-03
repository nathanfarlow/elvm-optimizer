open Core

type t

val get_statements : t -> Statement.t Deque.t
val get_id : t -> int