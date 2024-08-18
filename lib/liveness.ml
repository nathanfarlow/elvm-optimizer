open! Core
open Ast
module VarSet = Set.Make (Variable)

type t =
  { mutable live_in : VarSet.t
  ; mutable live_out : VarSet.t
  }
[@@deriving sexp_of, fields]

let killed = function
  | Statement.Assign { dst; _ } -> VarSet.singleton dst
  | Getc dst -> VarSet.singleton dst
  | Putc _ | Jump _ | Exit | Nop -> VarSet.empty
;;

let used stmt =
  let rec used = function
    | Expression.Var v -> VarSet.singleton v
    | Add xs -> VarSet.union_list (List.map xs ~f:used)
    | Sub (a, b) -> Set.union (used a) (used b)
    | If { left; right; cmp = _ } -> Set.union (used left) (used right)
    | Const _ | Label _ -> VarSet.empty
  in
  match stmt with
  | Statement.Assign { src; _ } -> used src
  | Putc e -> used e
  | Jump { target; cond } ->
    let target = used target in
    (match cond with
     | Some { left; right; cmp = _ } ->
       VarSet.union_list [ target; used left; used right ]
     | None -> target)
  | Getc _ | Exit | Nop -> VarSet.empty
;;

let analyze graph =
  let graph =
    Graph.map graph ~f:(fun stmt ->
      { live_in = VarSet.empty; live_out = VarSet.empty }, stmt)
  in
  let rec update node =
    let t, stmt = Graph.Node.v node in
    t.live_out
    <- Graph.Node.out_as_list node
       |> List.map ~f:(fun n -> (fst (Graph.Node.v n)).live_in)
       |> VarSet.union_list;
    let live_in = Set.diff (Set.union t.live_out (used stmt)) (killed stmt) in
    if not (Set.equal t.live_in live_in)
    then (
      t.live_in <- live_in;
      Graph.Node.in_ node |> List.iter ~f:update)
  in
  Graph.iter graph ~f:update;
  graph
;;
