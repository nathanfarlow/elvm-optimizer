open Core
open Ast
module Insn = Eir.Instruction

let lift_reg r = Variable.Register r

let lift_imm_or_reg : Insn.Imm_or_reg.t -> Expression.t = function
  | Int x -> Const x
  | Register x -> Var (lift_reg x)
  | Label x -> Label x
;;

let lift_cond Insn.Condition.{ cmp; args } : Condition.t =
  let left = lift_imm_or_reg (Register args.dst) in
  let right = lift_imm_or_reg args.src in
  match cmp with
  | Eq -> { cmp = Eq; left; right }
  | Ne -> { cmp = Ne; left; right }
  | Lt -> { cmp = Lt; left; right }
  | Le -> { cmp = Le; left; right }
  | Gt -> { cmp = Le; left = right; right = left }
  | Ge -> { cmp = Lt; left = right; right = left }
;;

let lift_insn : Insn.t -> Statement.t = function
  | Mov { dst; src } ->
    let src = lift_imm_or_reg src in
    Assign { dst = lift_reg dst; src }
  | Add { dst; src } ->
    let src = lift_imm_or_reg src in
    let dst = lift_reg dst in
    Assign { dst; src = Add [ Var dst; src ] }
  | Sub { dst; src } ->
    let src = lift_imm_or_reg src in
    let dst = lift_reg dst in
    Assign { dst; src = Sub (Var dst, src) }
  | Load { dst; src } ->
    let src = lift_imm_or_reg src in
    let dst = lift_reg dst in
    Assign { dst; src = Var (Memory src) }
  | Store { src; dst } ->
    let src = lift_reg src in
    let dst = lift_imm_or_reg dst in
    Assign { dst = Memory dst; src = Var src }
  | Putc src -> Putc (lift_imm_or_reg src)
  | Getc dst -> Getc (lift_reg dst)
  | Exit -> Exit
  | Jump { target; cond } ->
    let cond = Option.map cond ~f:lift_cond in
    Jump { target = lift_imm_or_reg target; cond }
  | Set ({ args; _ } as cond) ->
    let cond = lift_cond cond in
    Assign { dst = lift_reg args.dst; src = If cond }
  | Dump -> Nop
;;

(** f is a mapping from label -> new label *)
let update_labels eir mapping =
  let f label = Map.find mapping label |> Option.value ~default:label in
  let labels = Hashtbl.create (module String) in
  Hashtbl.iteri (Eir.labels eir) ~f:(fun ~key ~data ->
    Hashtbl.set labels ~key:(f key) ~data);
  let insns =
    let update_imm_or_reg = function
      | Insn.Imm_or_reg.Int x -> Insn.Imm_or_reg.Int x
      | Register x -> Register x
      | Label l -> Label (f l)
    in
    let update_cond Insn.Condition.{ cmp; args = { dst; src } } =
      Insn.Condition.{ cmp; args = { dst; src = update_imm_or_reg src } }
    in
    List.map (Eir.insns eir) ~f:(function
      | Insn.Mov { dst; src } -> Insn.Mov { dst; src = update_imm_or_reg src }
      | Add { dst; src } -> Add { dst; src = update_imm_or_reg src }
      | Sub { dst; src } -> Sub { dst; src = update_imm_or_reg src }
      | Load { dst; src } -> Load { dst; src = update_imm_or_reg src }
      | Store { src; dst } -> Store { src; dst = update_imm_or_reg dst }
      | Putc src -> Putc (update_imm_or_reg src)
      | Getc dst -> Getc dst
      | Exit -> Exit
      | Jump { target; cond } ->
        let target = update_imm_or_reg target in
        let cond = Option.map cond ~f:update_cond in
        Jump { target; cond }
      | Set x -> Set (update_cond x)
      | Dump -> Dump)
  in
  let data =
    List.map (Eir.data eir) ~f:(function
      | Const x -> Eir.Data.Const x
      | Label l -> Label (f l))
  in
  Eir.create ~insns ~labels ~data
;;

let offset_to_label eir segment =
  (* TODO: After we update eir we can remove some of these workarounds *)
  Eir.labels eir
  |> Map.of_hashtbl_exn (module String)
  |> Map.filter_map ~f:(function
    | { segment = s; offset } when Eir.Segment.equal segment s -> Some offset
    | _ -> None)
  |> Map.filter_keys ~f:(String.( <> ) Eir.const_heap_start_label)
  |> Map.to_alist
  |> List.map ~f:Tuple2.swap
;;

let delete_duplicate_labels eir =
  let mappings l =
    l
    |> Map.of_alist_multi (module Int)
    |> Map.data
    |> List.concat_map ~f:(fun l ->
      let name =
        match List.mem l "main" ~equal:String.equal with
        | true -> "main"
        | false -> List.hd_exn l
      in
      List.map l ~f:(fun x -> x, name))
    |> Map.of_alist_exn (module String)
  in
  Map.merge_disjoint_exn
    (mappings (offset_to_label eir Data))
    (mappings (offset_to_label eir Text))
  |> update_labels eir
;;

let chunk l offset_to_label =
  List.zip_exn
    (Map.data offset_to_label)
    (List.groupi l ~break:(fun i _ _ -> Map.mem offset_to_label i))
;;

let make_graph (eir : Eir.t) : Ast.Statement.t list Graph.t =
  let graph = Graph.create () in
  let chunks =
    let offset_to_label = offset_to_label eir Text |> Map.of_alist_exn (module Int) in
    let missing_jump_offsets =
      let num_insns = List.length (Eir.insns eir) in
      List.filter_mapi (Eir.insns eir) ~f:(fun i insn ->
        let next = i + 1 in
        match insn with
        | Jump { target = Label _; _ } when next < num_insns ->
          Some (next, [%string "__L%{next#Int}"])
        | _ -> None)
      |> Map.of_alist_exn (module Int)
    in
    let offset_to_label =
      Map.merge_skewed offset_to_label missing_jump_offsets ~combine:(fun ~key:_ a _ -> a)
    in
    chunk (Eir.insns eir) offset_to_label
  in
  List.iter chunks ~f:(fun (label, insns) ->
    let block = List.map insns ~f:lift_insn in
    Graph.add graph label block |> ignore);
  let rec add_edges = function
    | (cur, insns) :: ((next, _) :: _ as rest) ->
      let cur = Graph.find_exn graph cur in
      let next = Graph.find_exn graph next in
      let out_edge =
        match List.last insns with
        | Some Insn.Exit -> None
        | Some (Jump { target = Label target; cond }) ->
          let target = Graph.find_exn graph target in
          (match cond with
           | Some _ -> Conditional { true_ = target; false_ = next }
           | None -> Unconditional target)
          |> Graph.Node.Jump
          |> Some
        | _ -> Some (Fallthrough next)
      in
      Graph.Node.set_out cur out_edge;
      Graph.Node.(set_in next (cur :: in_ next));
      add_edges rest
    | _ -> ()
  in
  add_edges chunks;
  graph
;;

let f eir =
  let eir = delete_duplicate_labels eir in
  let data =
    offset_to_label eir Data
    |> Map.of_alist_exn (module Int)
    |> chunk (Eir.data eir)
    |> Map.of_alist_exn (module String)
  in
  make_graph eir, data
;;
