module Make
    (Variable : Forward_variable_intf.S)
    (Expression : Forward_expression_intf.S with type variable := Variable.t) : sig
  type t

  include
    Mapper_intf.S
      with type t := t
       and type key := Variable.t
       and type value := Expression.t
end
