open Core
open Expression
open Instruction
open Statement

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

let f (program : Program.t) =
  (* offset -> label name *)
  let offset_to_label = Hashtbl.create (module Int) in
  Hashtbl.iteri program.labels ~f:(fun ~key:label ~data:{ segment; offset } ->
      match segment with
      | Text -> Hashtbl.add_exn offset_to_label ~key:offset ~data:label
      | Data -> ());
  let start_label =
    Hashtbl.find_or_add offset_to_label 0 ~default:(fun () -> "__start__")
  in

  let block_insns = Hashtbl.create (module String) in
  let block_edges = Hashtbl.create (module String) in

  let current_label = ref start_label in
  let current_block = Deque.create () in
  let current_is_done = ref false in

  List.iteri program.instructions ~f:(fun i insn ->
      let label = Hashtbl.find offset_to_label i in
      Option.iter label ~f:(fun label ->
          if not @@ String.equal label !current_label then (
            (* move on to next block *)
            let insns = Deque.to_list current_block in
            Hashtbl.add_exn block_insns ~key:!current_label ~data:insns;
            Deque.clear current_block;
            current_label := label;
            current_is_done := false));

      if not @@ !current_is_done then (
        Deque.enqueue_back current_block (lift_insn insn);
        match insn with
        | Jump { target; condition } ->
            let condition = Option.map condition ~f:lift_cond in
            let target =
              match target with
              | Int _ -> assert false
              | Label l -> Some l
              | Register _ -> None
            in
            Hashtbl.add_exn block_edges ~key:!current_label
              ~data:(target, condition)
        | Exit -> current_is_done := true
        | _ -> ()));

  (* compile the maps into the blocks! *)
  failwith "TODO"
