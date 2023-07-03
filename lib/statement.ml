type destination =
| Memory of Expression.t
| Register of Elvm_instruction.register

type assignment = {
  dest: destination;
  src: Expression.t;
}

type jump = {
  target: Expression.t;
  condition: Expression.t option;
}

type t =
| Assign of assignment
| Putc of Expression.t
| Jmp of jump