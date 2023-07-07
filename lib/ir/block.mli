open! Core

type t [@@deriving sexp, equal]

module Edge : sig
  type type_ = Jump | Fallthrough [@@deriving sexp, equal]
  type t = { target : string; type_ : type_ } [@@deriving sexp, equal]
end

type branch =
  | Conditional_jump of { true_ : t; false_ : t }
  | Unconditional_jump of t
  | Fallthrough of t
[@@deriving sexp, equal]

val create :
  label:string ->
  statements:Statement.t list ->
  in_edges:Edge.t list ->
  branch:branch option ->
  t

val label : t -> string
val statements : t -> Statement.t list
val in_edges : t -> Edge.t list
val branch : t -> branch option
val set_branch : t -> branch option -> unit
val is_top_level : t -> bool
val dependencies : t -> Statement.variable list
val references : t -> string list