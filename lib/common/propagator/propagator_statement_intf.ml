module type S = sig
  type t
  type var
  type exp
  type mapping = { from : var; to_ : exp } [@@deriving sexp]

  val nop : t
  val from_mapping : mapping -> t
  val get_mapping_from_assignment : t -> mapping option
  val substitute_var_to_exp : t -> from:var -> to_:exp -> t * bool
end