open Core
module Insn = Eir.Instruction

let lift_reg r = Ast.Variable.Register r

let lift_imm_or_reg (imm_or_reg : Insn.Imm_or_reg.t) : Ast.Expression.t =
  match imm_or_reg with
  | Int x -> Const x
  | Register x -> Var (lift_reg x)
  | Label x -> Label x
;;

let lift_cond Insn.Condition.{ cmp; args } : Ast.Condition.t =
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

let lift_insn (insn : Insn.t) : Ast.Statement.t =
  match insn with
  | Mov { dst; src } ->
    let src = lift_imm_or_reg src in
    Assign { dst = lift_reg dst; src }
  | Add { dst; src } ->
    let src = lift_imm_or_reg src in
    let dst = lift_reg dst in
    (* sort to maintain canonical order *)
    let args = Ast.Expression.(List.sort [ Var dst; src ] ~compare) in
    Assign { dst; src = Add args }
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
  | Getc dst -> Assign { dst = lift_reg dst; src = Getc }
  | Exit -> Exit
  | Jump { target; cond } ->
    let cond = Option.map cond ~f:lift_cond in
    Jump { target = lift_imm_or_reg target; cond }
  | Set ({ args; _ } as cond) ->
    let cond = lift_cond cond in
    Assign { dst = lift_reg args.dst; src = If cond }
  | Dump -> Nop
;;

(* f is a mapping from label -> new label *)
let update_labels eir ~f =
  let f = Memo.general f in
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

let sorted_alist map ~compare =
  let alist = Hashtbl.to_alist map in
  List.sort alist ~compare:(fun (a, _) (b, _) -> compare a b)
;;

let fresh_label prefix eir =
  let i = ref 0 in
  let rec loop () =
    let label = sprintf "%s%d" prefix !i in
    incr i;
    if Hashtbl.mem (Eir.labels eir) label then loop () else label
  in
  loop
;;

let add_labels_to_every_instruction eir =
  let fresh = fresh_label "__L" eir in
  let num_insns = List.length (Eir.insns eir) in
  let labels = Hashtbl.copy (Eir.labels eir) in
  for i = 0 to num_insns - 1 do
    let label = fresh () in
    Hashtbl.add_exn labels ~key:label ~data:{ segment = Text; offset = i }
  done;
  Eir.create ~insns:(Eir.insns eir) ~labels ~data:(Eir.data eir)
;;

let rec make_offset_to_label_mapping eir segment =
  let exception Eir_changed of Eir.t in
  let offset_to_label = Hashtbl.create (module Int) in
  try
    Hashtbl.filter (Eir.labels eir) ~f:(fun { segment = s; _ } ->
      Eir.Segment.equal s segment)
    (* sort to eliminate nondeterminism in which duplicate label we'll remove*)
    |> sorted_alist ~compare:String.compare
    |> List.iter ~f:(fun (cur_label, { offset; _ }) ->
      match Hashtbl.find offset_to_label offset with
      (* first time we've seen this offset *)
      | None -> Hashtbl.add_exn offset_to_label ~key:offset ~data:cur_label
      | Some prev_label ->
        (* this is an address at the same place as another. Keep the old one
           unless the new one is "main" *)
        let keep, replace =
          if String.equal cur_label "main"
          then cur_label, prev_label
          else prev_label, cur_label
        in
        let eir_with_label_removed =
          update_labels eir ~f:(fun label ->
            if String.equal label replace then keep else label)
        in
        raise (Eir_changed eir_with_label_removed));
    eir, offset_to_label
  with
  | Eir_changed eir -> make_offset_to_label_mapping eir segment
;;

let make_statement_edges eir pc_to_label =
  let statements = Hashtbl.create (module String) in
  let out_edges = Hashtbl.create (module String) in
  let fallthrough_label = ref None in
  List.iteri (Eir.insns eir) ~f:(fun pc insn ->
    let label = Hashtbl.find_exn pc_to_label pc in
    (match !fallthrough_label with
     | Some prev_label ->
       Hashtbl.add_multi
         out_edges
         ~key:prev_label
         ~data:(label, Node.Reference.Fallthrough)
     | None -> ());
    Hashtbl.add_exn statements ~key:label ~data:(lift_insn insn);
    fallthrough_label
    := match insn with
       | Jump { target; cond } ->
         (match target with
          | Label target_label ->
            Hashtbl.add_multi out_edges ~key:label ~data:(target_label, Jump)
          | Register _ -> ()
          | Int _ -> failwith "jump int is undefined. replace it with jump label.");
         Option.map cond ~f:(fun _ -> label)
       | Exit -> None
       | _ -> Some label);
  statements, out_edges
;;

let make_graph statements out_edges =
  (* create nodes without edges *)
  let nodes = Hashtbl.create (module String) in
  Hashtbl.iteri statements ~f:(fun ~key:label ~data:stmt ->
    let node = Node.create ~label ~stmt in
    Hashtbl.add_exn nodes ~key:label ~data:node);
  (* fill in node references with reverse mapping of out_edges *)
  Hashtbl.iteri out_edges ~f:(fun ~key:from ~data:to_ ->
    let from = Hashtbl.find_exn nodes from in
    List.iter to_ ~f:(fun (label, type_) ->
      let node = Hashtbl.find_exn nodes label in
      Node.add_reference node Node.Reference.{ from; type_ }));
  (* fill in branches *)
  Hashtbl.iteri nodes ~f:(fun ~key:label ~data:node ->
    Node.set_branch
      node
      (match Hashtbl.find out_edges label with
       | None -> None
       | Some [ (label, Node.Reference.Fallthrough) ] ->
         let target = Hashtbl.find_exn nodes label in
         Some (Fallthrough target)
       | Some [ (label, Jump) ] ->
         let target = Hashtbl.find_exn nodes label in
         Some (Unconditional_jump target)
       | Some [ (false_, Fallthrough); (true_, Jump) ] ->
         let true_ = Hashtbl.find_exn nodes true_ in
         let false_ = Hashtbl.find_exn nodes false_ in
         Some (Conditional_jump { true_; false_ })
       | _ -> assert false));
  Graph.create nodes
;;

let make_data eir offset_to_label =
  (* remove special heap base label for now. we handle it explicitly *)
  Hashtbl.filter_inplace offset_to_label ~f:(fun label ->
    not (String.equal label Eir.const_heap_start_label));
  let heap_entry = Program.Data.{ label = Eir.const_heap_start_label; type_ = Heap } in
  if not @@ List.is_empty (Eir.data eir)
  then (
    (* add a first data label if there isn't one *)
    let fresh_label = (fresh_label "__D" eir) () in
    ignore @@ Hashtbl.add offset_to_label ~key:0 ~data:fresh_label;
    let sorted_labels_by_offset =
      sorted_alist offset_to_label ~compare:Int.compare |> List.map ~f:snd
    in
    let chunks =
      List.groupi (Eir.data eir) ~break:(fun i _ _ -> Hashtbl.mem offset_to_label i)
      |> List.zip_exn sorted_labels_by_offset
      |> List.map ~f:(fun (label, data) -> Program.Data.{ label; type_ = Chunk data })
    in
    heap_entry :: chunks)
  else [ heap_entry ]
;;

let f eir =
  let eir = add_labels_to_every_instruction eir in
  let eir, pc_to_label = make_offset_to_label_mapping eir Text in
  let eir, data_to_label = make_offset_to_label_mapping eir Data in
  let statements, out_edges = make_statement_edges eir pc_to_label in
  let graph = make_graph statements out_edges in
  let data = make_data eir data_to_label in
  Program.create ~graph ~data
;;
