module type S = sig
  type t
  type var
  type exp
  type mapping = { from : var; to_ : exp }

  val nop : t
  val from_mapping : mapping -> t
  val get_mapping_from_assignment : t -> mapping option
  val substitute_exp_to_var : t -> from:exp -> to_:var -> t * bool
end
