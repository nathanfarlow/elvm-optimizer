module Make
    (Statement : Eliminator_statement_intf.S)
    (_ : Propagator_mapping_intf.S
           with type key := Statement.var
            and type value := Statement.exp) : sig
  type t

  val create : unit -> t

  include
    Inplace_optimizer_intf.S
      with type t := t
       and type target := Statement.t Graph.t
end
