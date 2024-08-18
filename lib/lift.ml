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

let make_graph eir =
  let graph = Graph.create () in
  let offset_to_label = offset_to_label eir Text |> Map.of_alist_exn (module Int) in
  let nodes =
    List.mapi (Eir.insns eir) ~f:(fun i insn ->
      let label =
        Map.find offset_to_label i |> Option.value ~default:(Graph.fresh_label graph)
      in
      Graph.add graph label (lift_insn insn))
  in
  let rec add_edges l =
    match l with
    | cur :: rest ->
      let () =
        let open Graph.Node in
        match v cur with
        | Ast.Statement.Exit -> ()
        | Jump { target = Label target; cond } ->
          let target = Graph.find_exn graph target in
          (match cond with
           | Some _ ->
             let false_ = List.hd_exn rest in
             let out = Jump (Conditional { true_ = target; false_ = List.hd_exn rest }) in
             set_out cur (Some out);
             set_in target (cur :: in_ target);
             set_in false_ (cur :: in_ false_)
           | None ->
             set_out cur (Some (Jump (Unconditional target)));
             set_in target (cur :: in_ target))
        | _ ->
          let next = List.hd_exn rest in
          set_out cur (Some (Jump (Unconditional next)));
          set_in next (cur :: in_ next)
      in
      add_edges rest
    | _ -> ()
  in
  add_edges nodes;
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
