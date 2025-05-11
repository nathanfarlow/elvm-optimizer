open! Core
open Ast
module Insn = Eir.Instruction
module RegSet = Set.Make (Eir.Register)

let fresh =
  let counter = ref 0 in
  fun () ->
    let n = !counter in
    incr counter;
    [%string "reg%{n#Int}"]
;;

let rec lower_exp (reg : Eir.Register.t) (exp : Expression.t) =
  match exp with
  | Const n -> [ Insn.Mov { dst = reg; src = Int n } ]
  | Label l -> [ Mov { dst = reg; src = Label l } ]
  | Var v -> lower_var reg v
  | Sub (a, b) ->
    let a_reg = fresh () in
    let a = lower_exp a_reg a in
    let b_reg = fresh () in
    let b = lower_exp b_reg b in
    let sub = Insn.Sub { dst = a_reg; src = Register b_reg } in
    let mov = Insn.Mov { dst = reg; src = Register a_reg } in
    a @ b @ [ sub; mov ]
  | Expression.Add expressions ->
    let reg' = fresh () in
    List.concat_map expressions ~f:(fun e ->
      let insns = lower_exp reg' e in
      let add = Insn.Add { dst = reg; src = Register reg' } in
      insns @ [ add ])
  | Expression.If cond ->
    let left, right, cmp, cmp_insns = lower_cond cond in
    let cmp_insn = Insn.Set { args = { dst = left; src = Register right }; cmp } in
    let mov = Insn.Mov { dst = reg; src = Register left } in
    cmp_insns @ [ cmp_insn; mov ]

and lower_cond { Ast.Condition.cmp; left; right } =
  let left_reg = fresh () in
  let right_reg = fresh () in
  let cmp = lower_cmp cmp in
  let left = lower_exp left_reg left in
  let right = lower_exp right_reg right in
  left_reg, right_reg, cmp, left @ right

and lower_var reg = function
  | Register r -> [ Insn.Mov { dst = reg; src = Insn.Imm_or_reg.Register r } ]
  | Memory e ->
    let insns = lower_exp reg e in
    let mv = Insn.Load { dst = reg; src = Register reg } in
    insns @ [ mv ]

and lower_cmp : Comparison.t -> Insn.Comparison.t = function
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Lt
  | Le -> Le
;;

let rec lower_stmt (stmt : Statement.t) : Insn.t list =
  match stmt with
  | Assign { dst; src } ->
    let data_reg =
      match dst with
      | Register r -> r
      | Memory _ -> fresh ()
    in
    let data = lower_exp data_reg src in
    (match dst with
     | Register _ -> data
     | Memory address ->
       let address_reg = fresh () in
       let insns = lower_exp address_reg address in
       let mv = Insn.Store { dst = Register address_reg; src = data_reg } in
       data @ insns @ [ mv ])
  | Putc e ->
    let reg = fresh () in
    let insns = lower_exp reg e in
    let putc = Insn.Putc (Register reg) in
    insns @ [ putc ]
  | Getc var ->
    let reg = fresh () in
    let getc = Insn.Getc reg in
    getc :: lower_stmt (Assign { dst = var; src = Var (Register reg) })
  | Jump { target; cond } ->
    let target_reg = fresh () in
    let target = lower_exp target_reg target in
    let cond =
      Option.map cond ~f:(fun cond ->
        let left, right, cmp, cmp_insns = lower_cond cond in
        cmp_insns, { Insn.Condition.cmp; args = { src = Register left; dst = right } })
    in
    (match cond with
     | None -> target @ [ Insn.Jump { target = Register target_reg; cond = None } ]
     | Some (cmp_insns, cond) ->
       let jmp = Insn.Jump { target = Register target_reg; cond = Some cond } in
       target @ cmp_insns @ [ jmp ])
  | Exit -> [ Insn.Exit ]
  | Nop -> [ Insn.Dump ]
;;

let go graph = Graph.map graph ~f:lower_stmt |> Graph.flatten
