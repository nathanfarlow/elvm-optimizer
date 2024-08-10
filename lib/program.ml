open Core

module Data = struct
  type type_ = Chunk of Eir.Data.t list | Heap [@@deriving sexp, equal, hash]
  type t = { label : string; type_ : type_ } [@@deriving sexp, equal, hash]
end

type 'a t = { graph : 'a Graph.t; data : Data.t list } [@@deriving fields]

let create ~graph ~data = { graph; data }

module For_tests (Element : Sexpable) = struct
  module Graph_tests = Graph.For_tests (Element)

  let to_string (t : Element.t t) =
    ((if not @@ List.is_empty t.data then
        [
          "data:";
          Sexp.to_string_hum (List.sexp_of_t Data.sexp_of_t t.data)
          |> String_util.indent_string ~indent:1;
        ]
      else [])
    @
    if not @@ Hashtbl.is_empty (Graph.nodes t.graph) then
      [
        "graph:";
        Graph_tests.to_string t.graph |> String_util.indent_string ~indent:1;
      ]
    else [])
    |> String.concat ~sep:"\n"
end
