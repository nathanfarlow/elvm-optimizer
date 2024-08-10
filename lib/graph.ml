open Core

type 'a t =
  { nodes : (string, 'a Node.t) Hashtbl.t
  ; fresh_label : unit -> string
  }

let create nodes =
  let fresh_label =
    let counter = ref 0 in
    fun () ->
      let label = Printf.sprintf "__L%d" !counter in
      counter := !counter + 1;
      label
  in
  { nodes; fresh_label }
;;

let nodes t = t.nodes
let find_blocks t = Hashtbl.filter t.nodes ~f:Node.is_top_level
let fresh_label t = t.fresh_label ()
let register_node t node = Hashtbl.add_exn t.nodes ~key:(Node.label node) ~data:node
let unregister_node t = Hashtbl.remove t.nodes

module For_tests (E : sig
    type t [@@deriving sexp_of]
  end) =
struct
  module Node_test = Node.For_tests (E)

  let to_string (t : E.t t) =
    t.nodes
    |> Hashtbl.to_alist
    (* sort for deterministic output *)
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.map ~f:(fun (_, node) -> Node_test.to_string node)
    |> String.concat ~sep:"\n"
  ;;
end

(* todo: fix *)
let memo ~f ~on_cycle =
  let memo = Hashtbl.create (module String) in
  let evaluating = Hash_set.create (module String) in
  let rec g node =
    let label = Node.label node in
    match Hashtbl.find memo label with
    | Some res -> res
    | None ->
      if Hash_set.mem evaluating label
      then on_cycle node
      else (
        Hash_set.add evaluating label;
        let data = f node g in
        Hash_set.remove evaluating label;
        (* don't cache computations if we are still evaluating another node.
           this ensures consistency across the cached results *)
        if Hash_set.is_empty evaluating then Hashtbl.set memo ~key:label ~data;
        data)
  in
  g
;;

(* let get_all_branch_targets node = *)
(*   match Node.branch node with *)
(*   | None -> [] *)
(*   | Some (Fallthrough target) | Some (Unconditional_jump target) -> [ target ] *)
(*   | Some (Conditional_jump { true_; false_ }) -> [ true_; false_ ] *)
(* ;; *)
