open! Core

type t [@@deriving sexp, equal]

type branch = Conditional of { true_ : t; false_ : t } | Unconditional of t
[@@deriving sexp, equal]

val create :
  label:string ->
  statements:Statement.t list ->
  in_edges:string list ->
  branch:branch option ->
  t

val label : t -> string
val statements : t -> Statement.t list
val statements_rev : t -> Statement.t list
val in_edges : t -> string list
val branch : t -> branch option
val set_branch : t -> branch option -> unit
val is_top_level : t -> bool
val dependencies : t -> Statement.variable list
val references : t -> string list