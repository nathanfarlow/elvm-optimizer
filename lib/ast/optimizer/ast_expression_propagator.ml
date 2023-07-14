module Mapper = Forward_mapper.Make (Ast_expression.Variable) (Ast_expression)

let get_final_mappings _node = ()
let optimize _ (_graph : Ast_statement.t Graph.t) : bool = false
