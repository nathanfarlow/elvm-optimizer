open Core

module Make
    (Statement : Assignable_statement_intf.S)
    (Lhs : Environment_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Environment_rhs_intf.S with type t = Statement.rhs and type lhs := Lhs.t)
    (Substitute_delegate : Substitute_delegate_intf.S
                           with type stmt = Statement.t
                            and type lhs = Lhs.t
                            and type rhs = Rhs.t) =
struct
  module Environment = Environment.Make (Lhs) (Rhs)
  module Substitute_util = Substitute_util.Make (Substitute_delegate) (Environment)

  type t =
    { get_initial_env : Statement.t Node.t -> Environment.t
    ; get_final_env : Statement.t Node.t -> Environment.t
    }

  let create () =
    (* gets the mappings which are living at the end of this node *)
    let get_final_env get_initial_env node =
      let initial_env, is_cycle = get_initial_env node in
      Substitute_util.substitute_all (Node.stmt node) initial_env
      |> fun (stmt, _) ->
      Statement.get_assignment stmt
      |> function
      | Some Statement.{ from; to_ } ->
        Environment.(update initial_env ~from ~to_).valid, is_cycle
      | None -> initial_env, is_cycle
    in
    (* gets the mappings which are living at the start of this node *)
    let get_initial_env =
      Graph_util.memoize
        ~f:(fun node get_initial_env ->
          let cycles, resolved =
            Node.references node
            |> List.map ~f:(fun Node.Reference.{ from; _ } ->
              get_final_env get_initial_env from)
            |> List.partition_tf ~f:(fun (_, is_cycle) -> is_cycle)
          in
          let merge l = List.map l ~f:fst |> List.reduce ~f:Environment.intersection in
          match merge cycles, merge resolved with
          | Some cycles, Some resolved -> Environment.union cycles resolved, false
          | Some cycles, None -> cycles, true
          | None, Some resolved -> resolved, false
          | None, None -> Environment.empty, false)
        ~on_cycle:(fun _ -> Environment.empty, true)
    in
    let wrap f node =
      let env, is_cycle = f node in
      if is_cycle then Environment.empty else env
    in
    { get_initial_env = wrap get_initial_env
    ; get_final_env = wrap @@ get_final_env get_initial_env
    }
  ;;

  let get_initial_env t = t.get_initial_env
  let get_final_env t = t.get_final_env
end
