open Core

module Node = struct
  type 'a t =
    { id : string
    ; mutable v : 'a
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

  let set_in = set_in_

  let sexp_of_t sexp_of__a t =
    (* Define a custom type for sexp serialization to avoid infinite loops in
       cycles *)
    let module Sexpable = struct
      type 'a t =
        { v : 'a
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
    { Sexpable.v = t.v
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

let add t id v =
  let node = { Node.id; v; in_ = []; out = None } in
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

let to_dot t to_string =
  let buf = Buffer.create 100 in
  let add_line s = Buffer.add_string buf (s ^ "\n") in
  add_line "digraph G {";
  Map.iteri t.nodes ~f:(fun ~key:id ~data:node ->
    let node_str = to_string node.v |> String.split_lines |> String.concat ~sep:"\\l" in
    add_line (sprintf "  %s [label=<\\l%s:\\l\\l%s>];" id id node_str);
    match node.out with
    | None -> ()
    | Some (Unconditional child) -> add_line (sprintf "  %s -> %s;" id child.id)
    | Some (Conditional { true_; false_ }) ->
      add_line (sprintf "  %s -> %s [label=\"true\"];" id true_.id);
      add_line (sprintf "  %s -> %s [label=\"false\"];" id false_.id));
  add_line "}";
  Buffer.contents buf
;;
