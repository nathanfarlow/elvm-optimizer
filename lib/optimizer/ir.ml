open Core
open Program
open Instruction
open Expression
open Statement
open Block

type t = Block.t list [@@deriving sexp, equal]

let lift_imm_or_reg imm_or_reg =
  match imm_or_reg with
  | Int x -> Const x
  | Register x -> Register x
  | Label x -> Label x

let lift_cond { comparison; args } =
  let a = lift_imm_or_reg (Register args.dst) in
  let b = lift_imm_or_reg args.src in
  match comparison with
  | Eq -> { comparison = Eq; a; b }
  | Ne -> { comparison = Ne; a; b }
  | Lt -> { comparison = Lt; a; b }
  | Le -> { comparison = Le; a; b }
  | Gt -> { comparison = Le; a = b; b = a }
  | Ge -> { comparison = Lt; a = b; b = a }

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
  | Dump -> Dump

let update_labels program ~f =
  let labels = Hashtbl.create (module String) in
  let update label =
    let old_address = Hashtbl.find_exn program.labels label in
    let new_label, new_address = f label old_address in
    let found =
      Hashtbl.find_or_add labels new_label ~default:(fun () -> new_address)
    in
    assert (Program.equal_address found new_address);
    new_label
  in
  let instructions =
    let update_imm_or_reg = function
      | Int x -> Int x
      | Register x -> Register x
      | Label l -> Label (update l)
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
      | Label l -> Label (update l))
  in
  { instructions; data; labels }

let sorted_alist map compare =
  let alist = Hashtbl.to_alist map in
  List.sort alist ~compare:(fun (a, _) (b, _) -> compare a b)

(* text offset -> label map *)
let rec make_offset_label_mapping program =
  let exception Program_changed of Program.t in
  let offset_to_label = Hashtbl.create (module Int) in
  try
    (* sort to eliminate nondeterminism in which duplicate label we remove*)
    sorted_alist program.labels String.compare
    |> List.iter ~f:(fun (cur_label, { segment; offset }) ->
           match segment with
           | Text -> (
               match Hashtbl.find offset_to_label offset with
               (* first time we've seen this offset *)
               | None ->
                   Hashtbl.add_exn offset_to_label ~key:offset ~data:cur_label
               | Some prev_label ->
                   (* something like this happened:
                       prev_label:
                       cur_label:
                        exit
                       we can only keep one of them. the other one we remove and
                       remap all references to the other. we keep prev_label unless
                       cur_label is "main" *)
                   let keep, replace =
                     if String.equal cur_label "main" then
                       (cur_label, prev_label)
                     else (prev_label, cur_label)
                   in
                   let program_with_label_removed =
                     update_labels program ~f:(fun label addr ->
                         if String.equal label replace then (keep, addr)
                         else (label, addr))
                   in
                   raise (Program_changed program_with_label_removed))
           | Data -> ());
    (program, offset_to_label)
  with Program_changed program -> make_offset_label_mapping program

let make_blocks statement_groups out_edges =
  let blocks = Hashtbl.create (module String) in
  let rec make_block label =
    let statements = Hashtbl.find_exn statement_groups label in
    let edges = Hashtbl.find out_edges label in
    match Hashtbl.find blocks label with
    | Some block -> block
    | None ->
        let branch =
          match edges with
          | None -> None
          | Some [ primary ] ->
              Some { primary = make_block primary; secondary = None }
          | Some [ primary; secondary ] ->
              let primary = make_block primary in
              let secondary = make_block secondary in
              Some { primary; secondary = Some secondary }
          | _ -> assert false
        in
        let block = { label; statements; branch } in
        Hashtbl.add_exn blocks ~key:label ~data:block;
        block
  in
  let parents =
    let in_edges = Hashtbl.create (module String) in
    Hashtbl.iteri out_edges ~f:(fun ~key:label ~data:edges ->
        List.iter edges ~f:(fun target ->
            Hashtbl.add_multi in_edges ~key:target ~data:label));
    let is_parent label =
      match Hashtbl.find in_edges label with
      | None -> true
      (* must be a while true loop *)
      | Some [ l ] when String.equal l label -> true
      | _ -> false
    in
    Hashtbl.keys out_edges |> List.filter ~f:is_parent
  in
  List.map parents ~f:make_block

let fresh_label_gen reserved =
  let i = ref 0 in
  let rec loop () =
    let label = sprintf "__L%d" !i in
    if reserved label then loop () else label
  in
  loop

let of_program program =
  let program, offset_to_label = make_offset_label_mapping program in

  let all_statements = Hashtbl.create (module String) in
  let all_out_edges = Hashtbl.create (module String) in

  let prev_label = ref None in
  let current_statements = Deque.create () in

  let commit () =
    (match !prev_label with
    | Some prev_label ->
        let data = Deque.to_list current_statements in
        Hashtbl.add_exn all_statements ~key:prev_label ~data
    | None -> ());
    Deque.clear current_statements;
    prev_label := None
  in
  let fresh = fresh_label_gen (Hashtbl.mem program.labels) in

  List.iteri program.instructions ~f:(fun i insn ->
      let cur_label =
        match (!prev_label, Hashtbl.find offset_to_label i) with
        | _, Some new_label ->
            commit ();
            new_label
        | None, _ -> fresh ()
        | Some prev_label, None -> prev_label
      in

      prev_label := Some cur_label;

      Deque.enqueue_back current_statements (lift_insn insn);
      match insn with
      | Jump { target; _ } ->
          (match target with
          | Label l -> Hashtbl.add_multi all_out_edges ~key:cur_label ~data:l
          | Register _ -> ()
          | Int _ -> assert false);
          commit ()
      | Exit -> commit ()
      | _ -> ());

  commit ();

  make_blocks all_statements all_out_edges

let optimize _ = failwith "unimplemented"
let to_program _ = failwith "unimplemented"