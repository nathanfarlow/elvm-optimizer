open Core

type register = A | B | C | D | SP | BP [@@deriving sexp, equal]

type immediate_or_register =
  | Int of int
  | Label of string
  | Register of register
[@@deriving sexp, equal]

type src_dst = { src : immediate_or_register; dst : register }
[@@deriving sexp, equal]

type comparison = Eq | Ne | Lt | Le | Gt | Ge [@@deriving sexp, equal]

type condition = { comparison : comparison; args : src_dst }
[@@deriving sexp, equal]

type t =
  | Mov of src_dst
  | Add of src_dst
  | Sub of src_dst
  | Load of src_dst
  | Store of { src : register; dst : immediate_or_register }
  | Putc of immediate_or_register
  | Getc of register
  | Exit
  | Jump of { target : immediate_or_register; condition : condition option }
  | Set of condition
  | Dump
[@@deriving sexp, equal]
