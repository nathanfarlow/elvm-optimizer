module Make
    (Lhs : Propagator_var_intf.S)
    (Rhs : Propagator_exp_intf.S with type var := Lhs.t) : sig
  type t

  include
    Propagator_mapping_intf.S
      with type t := t
       and type key := Lhs.t
       and type value := Rhs.t
end