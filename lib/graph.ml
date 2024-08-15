open Core

module type Sexp_of_m = sig
  type t [@@deriving sexp_of]
end

[@@@warning "-69"]

module Node = struct
  type 'a t =
    { id : string
    ; stmts : 'a list
    ; mutable in_ : 'a t list
    ; mutable out : 'a out option
    }
  [@@deriving fields ~getters ~setters]

  and 'a out =
    | Unconditional of 'a t
    | Conditional of
        { true_ : 'a t
        ; false_ : 'a t
        }

  let equal_id t1 t2 = String.equal t1.id t2.id

  let out_as_list t =
    match t.out with
    | None -> []
    | Some (Unconditional t') -> [ t' ]
    | Some (Conditional { true_; false_ }) -> [ true_; false_ ]
  ;;

  (** Does t have this parent in its in edges? *)
  let has_parent t ~parent = List.mem t.in_ parent ~equal:equal_id

  (** Does t have this child in its out edges? *)
  let has_child t ~child = List.mem (out_as_list t) child ~equal:equal_id

  let add_in t parent = t.in_ <- parent :: t.in_

  let sexp_of_t sexp_of__a t =
    (* Define a custom type for sexp serialization to avoid infinite loops in
       cycles *)
    let module Sexpable = struct
      type 'a t =
        { stmts : 'a list
        ; in_ : string list
        ; out : 'a out option
        }
      [@@deriving sexp_of]

      and 'a out =
        | Unconditional of string
        | Conditional of
            { true_ : string
            ; false_ : string
            }
      [@@deriving sexp_of]
    end
    in
    { Sexpable.stmts = t.stmts
    ; in_ = List.map t.in_ ~f:id
    ; out =
        Option.map t.out ~f:(function
          | Unconditional t -> Sexpable.Unconditional t.id
          | Conditional { true_; false_ } ->
            Conditional { true_ = true_.id; false_ = false_.id })
    }
    |> Sexpable.sexp_of_t sexp_of__a
  ;;
end

type 'a t = { mutable nodes : 'a Node.t Map.M(String).t } [@@deriving sexp_of]

let nodes t = t.nodes
let create () = { nodes = Map.empty (module String) }

let add t id stmts =
  let node = { Node.id; stmts; in_ = []; out = None } in
  t.nodes <- Map.set t.nodes ~key:id ~data:node;
  node
;;

let remove t node =
  t.nodes <- Map.remove t.nodes node.Node.id;
  assert (not @@ List.exists node.in_ ~f:(Node.has_child ~child:node));
  assert (not @@ List.exists (Node.out_as_list node) ~f:(Node.has_parent ~parent:node))
;;

let find t = Map.find t.nodes
let find_exn t = Map.find_exn t.nodes
