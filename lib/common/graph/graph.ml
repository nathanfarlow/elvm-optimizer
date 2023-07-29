type 'a t = {
  nodes : (string, 'a Node.t) Hashtbl.t;
  fresh_label : unit -> string;
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

let nodes t = t.nodes
let find_blocks t = Hashtbl.filter t.nodes ~f:Node.is_top_level
let fresh_label t = t.fresh_label ()
let add_node t node = Hashtbl.add_exn t.nodes ~key:(Node.label node) ~data:node

module For_tests (Element : Sexpable.S) = struct
  module Node_test = Node.For_tests (Element)

  let to_string (t : Element.t t) =
    t.nodes |> Hashtbl.to_alist
    (* sort for deterministic output *)
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.map ~f:(fun (_, node) -> Node_test.to_string node)
    |> String.concat ~sep:"\n"
end