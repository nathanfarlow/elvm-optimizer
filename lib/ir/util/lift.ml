open Core
open Program
open Instruction
open Expression
open Statement
open Block
open Ir

let lift_imm_or_reg imm_or_reg =
  match imm_or_reg with
  | Int x -> Const x
  | Register x -> Register x
  | Label x -> Label x

let lift_cond { comparison; args } =
  let left = lift_imm_or_reg (Register args.dst) in
  let right = lift_imm_or_reg args.src in
  match comparison with
  | Eq -> { comparison = Eq; left; right }
  | Ne -> { comparison = Ne; left; right }
  | Lt -> { comparison = Lt; left; right }
  | Le -> { comparison = Le; left; right }
  | Gt -> { comparison = Le; left = right; right = left }
  | Ge -> { comparison = Lt; left = right; right = left }

let lift_insn = function
  | Mov { dst; src } ->
      let src = lift_imm_or_reg src in
      Assign { dst = Register dst; src }
  | Add { dst; src } ->
      let src = lift_imm_or_reg src in
      Assign { dst = Register dst; src = Add [ Register dst; src ] }
  | Sub { dst; src } ->
      let src = lift_imm_or_reg src in
      Assign { dst = Register dst; src = Sub (Register dst, src) }
  | Load { dst; src } ->
      let src = lift_imm_or_reg src in
      Assign { dst = Register dst; src = Memory src }
  | Store { src; dst } ->
      let dst = lift_imm_or_reg dst in
      Assign { dst = Memory dst; src = Register src }
  | Putc src -> Putc (lift_imm_or_reg src)
  | Getc dst -> Assign { dst = Register dst; src = Getc }
  | Exit -> Exit
  | Jump { target; condition } ->
      let condition = Option.map condition ~f:lift_cond in
      Jump { target = lift_imm_or_reg target; condition }
  | Set ({ args; _ } as cond) ->
      let cond = lift_cond cond in
      Assign { dst = Register args.dst; src = Set cond }
  | Dump -> Nop

(* f is a mapping from label -> new label *)
let update_labels program ~f =
  let f = Memo.general f in
  let labels = Hashtbl.create (module String) in
  Hashtbl.iteri program.labels ~f:(fun ~key ~data ->
      Hashtbl.set labels ~key:(f key) ~data);
  let instructions =
    let update_imm_or_reg = function
      | Int x -> Int x
      | Register x -> Register x
      | Label l -> Label (f l)
    in
    let update_cond { comparison; args = { dst; src } } =
      { comparison; args = { dst; src = update_imm_or_reg src } }
    in
    List.map program.instructions ~f:(function
      | Mov { dst; src } -> Mov { dst; src = update_imm_or_reg src }
      | Add { dst; src } -> Add { dst; src = update_imm_or_reg src }
      | Sub { dst; src } -> Sub { dst; src = update_imm_or_reg src }
      | Load { dst; src } -> Load { dst; src = update_imm_or_reg src }
      | Store { src; dst } -> Store { src; dst = update_imm_or_reg dst }
      | Putc src -> Putc (update_imm_or_reg src)
      | Getc dst -> Getc dst
      | Exit -> Exit
      | Jump { target; condition } ->
          let target = update_imm_or_reg target in
          let condition = Option.map condition ~f:update_cond in
          Jump { target; condition }
      | Set x -> Set (update_cond x)
      | Dump -> Dump)
  in
  let data =
    List.map program.data ~f:(function
      | Const x -> Program.Const x
      | Label l -> Label (f l))
  in
  { instructions; data; labels }

let sorted_alist map ~compare =
  let alist = Hashtbl.to_alist map in
  List.sort alist ~compare:(fun (a, _) (b, _) -> compare a b)

let fresh_label prefix program =
  let i = ref 0 in
  let rec loop () =
    let label = sprintf "%s%d" prefix !i in
    incr i;
    if Hashtbl.mem program.labels label then loop () else label
  in
  loop

(* Replace jmp int with jmp label. Also, every instruction now
   has at least one corresponding label. *)
let remove_jmp_int program =
  let fresh = fresh_label "__L" program in
  let int_labels =
    Array.init (List.length program.instructions) ~f:(fun _ -> fresh ())
  in
  let instructions =
    List.map program.instructions ~f:(function
      | Jump { target = Int n; condition } ->
          let target = Instruction.Label int_labels.(n) in
          Instruction.Jump { target; condition }
      | insn -> insn)
  in
  let labels = Hashtbl.copy program.labels in
  Array.iteri int_labels ~f:(fun i label ->
      Hashtbl.add_exn labels ~key:label ~data:{ segment = Text; offset = i });
  { program with instructions; labels }

let rec make_offset_to_label_mapping program segment =
  let exception Program_changed of Program.t in
  let offset_to_label = Hashtbl.create (module Int) in
  try
    Hashtbl.filter program.labels ~f:(fun { segment = s; _ } ->
        equal_segment s segment)
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
                 if String.equal cur_label "main" then (cur_label, prev_label)
                 else (prev_label, cur_label)
               in
               let program_with_label_removed =
                 update_labels program ~f:(fun label ->
                     if String.equal label replace then keep else label)
               in
               raise (Program_changed program_with_label_removed));
    (program, offset_to_label)
  with Program_changed program -> make_offset_to_label_mapping program segment

let make_graph program pc_to_label =
  let statements = Hashtbl.create (module String) in
  let out_edges = Hashtbl.create (module String) in

  let fallthrough_label = ref None in

  List.iteri program.instructions ~f:(fun pc insn ->
      let label = Hashtbl.find_exn pc_to_label pc in

      (match !fallthrough_label with
      | Some prev_label ->
          Hashtbl.add_multi out_edges ~key:prev_label ~data:label
      | None -> ());

      Hashtbl.add_exn statements ~key:label ~data:(lift_insn insn);
      fallthrough_label :=
        match insn with
        | Jump { target; condition } ->
            (match target with
            | Label l -> Hashtbl.add_multi out_edges ~key:label ~data:l
            | Register _ -> ()
            (* these should have already been replaced with jump label *)
            | Int _ -> assert false);
            Option.map condition ~f:(fun _ -> label)
        | Exit -> None
        | _ -> Some label);

  (statements, out_edges)

let make_blocks_from_graph statements out_edges =
  (* reverse mapping of out_edges *)
  let in_edges = Hashtbl.create (module String) in
  Hashtbl.iteri out_edges ~f:(fun ~key:label ~data:edges ->
      List.iter edges ~f:(fun edge ->
          Hashtbl.add_multi in_edges ~key:edge ~data:label));

  (* create blocks without branches *)
  let blocks = Hashtbl.create (module String) in
  Hashtbl.iteri statements ~f:(fun ~key:label ~data:stmt ->
      let in_edges = Hashtbl.find_multi in_edges label in
      let block =
        Block.create ~label ~statements:[ stmt ] ~in_edges ~branch:None
      in
      Hashtbl.add_exn blocks ~key:label ~data:block);

  (* fill in branches *)
  Hashtbl.iteri blocks ~f:(fun ~key:label ~data:block ->
      let branch = Hashtbl.find out_edges label in
      let branch =
        match branch with
        | None -> None
        | Some [ target ] ->
            let target = Hashtbl.find_exn blocks target in
            Some (Unconditional target)
        | Some [ true_; false_ ] ->
            let true_ = Hashtbl.find_exn blocks true_ in
            let false_ = Hashtbl.find_exn blocks false_ in
            Some (Conditional { true_; false_ })
        | _ -> assert false
      in
      Block.set_branch block branch);
  blocks

let make_top_level_blocks program pc_to_label =
  let statements, out_edges = make_graph program pc_to_label in
  let blocks = make_blocks_from_graph statements out_edges in
  sorted_alist blocks ~compare:String.compare
  |> List.map ~f:snd
  |> List.filter ~f:Block.is_top_level

let make_data (program : Program.t) offset_to_label =
  (* remove special heap base label for now. we handle it explicitly *)
  Hashtbl.filter_inplace offset_to_label ~f:(fun label ->
      not (String.equal label Program.heap_label));

  let heap_entry = { label = Program.heap_label; data = Heap } in
  if not @@ List.is_empty program.data then (
    (* add a first data label if there isn't one *)
    let fresh_label = (fresh_label "__D" program) () in
    ignore @@ Hashtbl.add offset_to_label ~key:0 ~data:fresh_label;

    let sorted_labels_by_offset =
      sorted_alist offset_to_label ~compare:Int.compare |> List.map ~f:snd
    in
    let chunks =
      List.groupi program.data ~break:(fun i _ _ ->
          Hashtbl.mem offset_to_label i)
      |> List.zip_exn sorted_labels_by_offset
      |> List.map ~f:(fun (label, data) -> { label; data = Chunk data })
    in
    heap_entry :: chunks)
  else [ heap_entry ]

let f program =
  let program = remove_jmp_int program in
  let program, pc_to_label = make_offset_to_label_mapping program Text in
  let program, data_to_label = make_offset_to_label_mapping program Data in
  let blocks = make_top_level_blocks program pc_to_label in
  let data = make_data program data_to_label in
  Ir.create ~blocks ~data
