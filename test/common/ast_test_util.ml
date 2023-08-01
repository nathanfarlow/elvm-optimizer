open Elvm
module Graph_tests = Graph.For_tests (Ast_statement)

let print_graph graph = Graph_tests.to_string graph |> print_endline

let fallthrough (stmts : Ast_statement.t list) =
  let graph = Graph.create @@ Hashtbl.create (module String) in
  let nodes =
    List.map stmts ~f:(fun stmt ->
        Node.create ~label:(Graph.fresh_label graph) ~stmt)
  in
  let pairs, _ = List.zip_with_remainder nodes (List.tl_exn nodes) in
  List.iter pairs ~f:(fun (node, next_node) ->
      Node.set_branch node (Some (Fallthrough next_node));
      Node.set_references next_node [ { from = node; type_ = Fallthrough } ]);
  List.iter nodes ~f:(Graph.register_node graph);
  graph