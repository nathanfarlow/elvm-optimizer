module Make
    (Substitute_delegate : Substitute_delegate_intf.S)
    (Environment : Environment_intf.S
                     with type lhs := Substitute_delegate.lhs
                      and type rhs := Substitute_delegate.rhs) =
struct
  let substitute_all stmt env =
    List.fold (Environment.to_alist env) ~init:(stmt, false)
      ~f:(fun (stmt, did_substitute) (lhs, rhs) ->
        let stmt', did_substitute' =
          Substitute_delegate.substitute stmt ~lhs ~rhs
        in
        (stmt', did_substitute || did_substitute'))
end
