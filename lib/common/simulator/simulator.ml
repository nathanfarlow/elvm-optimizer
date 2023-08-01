module Make
    (Statement : Simulator_statement_intf.S)
    (Lhs : Environment_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Environment_rhs_intf.S
             with type t = Statement.rhs
              and type lhs := Lhs.t)
    (Substitute_delegate : Substitute_delegate_intf.S
                             with type stmt = Statement.t
                              and type lhs = Lhs.t
                              and type rhs = Rhs.t) =
struct
  module Environment = Environment.Make (Lhs) (Rhs)

  module Substitute_util =
    Substitute_util.Make (Substitute_delegate) (Environment)

  type t = {
    get_initial_env : Statement.t Node.t -> Environment.t;
    get_final_env : Statement.t Node.t -> Environment.t;
  }

  let create () =
    (* gets the mappings which are living at the end of this node *)
    let get_final_env get_initial_env node =
      let initial_env = get_initial_env node in
      let assignment =
        Substitute_util.substitute_all (Node.stmt node)
          (Option.value initial_env ~default:Environment.empty)
        |> fun (stmt, _) -> Statement.get_assignment stmt
      in
      match (initial_env, assignment) with
      | Some initial_env, Some { from; to_ } ->
          Some Environment.(update initial_env ~from ~to_).valid
      | Some initial_env, None -> Some initial_env
      | None, Some { from; to_ } ->
          Some Environment.(update empty ~from ~to_).valid
      | None, None -> None
    in
    (* gets the mappings which are living at the start of this node *)
    let get_initial_env =
      Graph_util.memoize
        ~f:(fun node get_initial_env ->
          Node.references node
          |> List.map ~f:(fun Node.Reference.{ from; _ } ->
                 get_final_env get_initial_env from)
          |> List.filter_opt
          |> List.reduce ~f:Environment.intersection)
        ~on_cycle:(fun _ -> None)
    in
    let wrap f node = f node |> Option.value ~default:Environment.empty in
    {
      get_initial_env = wrap get_initial_env;
      get_final_env = wrap @@ get_final_env get_initial_env;
    }

  let get_initial_env t = t.get_initial_env
  let get_final_env t = t.get_final_env
end
