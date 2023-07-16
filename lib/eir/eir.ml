module Register = struct
  type t = A | B | C | D | SP | BP [@@deriving sexp, equal, compare, hash]

  let maybe_parse = function
    | "A" -> Some A
    | "B" -> Some B
    | "C" -> Some C
    | "D" -> Some D
    | "SP" -> Some SP
    | "BP" -> Some BP
    | _ -> None

  let parse_exn s =
    match maybe_parse s with
    | Some r -> r
    | None -> failwith @@ s ^ " is not a register"

  let to_string = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"
    | SP -> "SP"
    | BP -> "BP"
end

let maybe_parse_label line =
  match String.split line ~on:':' with [ label; "" ] -> Some label | _ -> None

let maybe_parse_number s =
  Caml.int_of_string_opt s
  (* elvm alternates between using signed and unsigned 24 bit integers
     for some reason? we'll fix that here *)
  |> Option.map ~f:(fun n -> if n > 0x7fffff then n - 0x1000000 else n)

module Instruction = struct
  module Imm_or_reg = struct
    type t = Int of int | Label of string | Register of Register.t
    [@@deriving sexp, equal, hash]

    let parse_exn s ~labels =
      match maybe_parse_number s with
      | Some n -> Int n
      | None -> (
          match Register.maybe_parse s with
          | Some r -> Register r
          | None ->
              if Hash_set.mem labels s then Label s
              else failwith @@ "label not found: " ^ s)
  end

  module Operands = struct
    type t = { src : Imm_or_reg.t; dst : Register.t }
    [@@deriving sexp, equal, hash]

    let parse_exn ~src ~dst ~labels =
      let src = Imm_or_reg.parse_exn src ~labels in
      let dst = Register.parse_exn dst in
      { src; dst }
  end

  module Comparison = struct
    type t = Eq | Ne | Lt | Le | Gt | Ge [@@deriving sexp, equal, hash]
  end

  module Condition = struct
    type t = { cmp : Comparison.t; args : Operands.t }
    [@@deriving sexp, equal, hash]

    let parse_exn cmp ~src ~dst ~labels =
      let args = Operands.parse_exn ~src ~dst ~labels in
      { cmp; args }
  end

  type t =
    | Mov of Operands.t
    | Add of Operands.t
    | Sub of Operands.t
    | Load of Operands.t
    | Store of { src : Register.t; dst : Imm_or_reg.t }
    | Putc of Imm_or_reg.t
    | Getc of Register.t
    | Exit
    | Jump of { target : Imm_or_reg.t; cond : Condition.t option }
    | Set of Condition.t
    | Dump
  [@@deriving sexp, equal, hash]

  let maybe_parse_exn line ~labels =
    let line = String.filter line ~f:(fun c -> not @@ Char.equal c ',') in

    let parse_cond_jump cmp target ~src ~dst =
      let target = Imm_or_reg.parse_exn target ~labels in
      let cond = Condition.parse_exn cmp ~src ~dst ~labels in
      Some (Jump { target; cond = Some cond })
    in

    let parse_set cmp ~src ~dst =
      Some (Set (Condition.parse_exn cmp ~src ~dst ~labels))
    in

    match Str.split (Str.regexp "[ \t]+") line with
    | [ "mov"; dst; src ] -> Some (Mov (Operands.parse_exn ~src ~dst ~labels))
    | [ "add"; dst; src ] -> Some (Add (Operands.parse_exn ~src ~dst ~labels))
    | [ "sub"; dst; src ] -> Some (Sub (Operands.parse_exn ~src ~dst ~labels))
    | [ "load"; dst; src ] -> Some (Load (Operands.parse_exn ~src ~dst ~labels))
    | [ "store"; src; dst ] ->
        let src = Register.parse_exn src in
        let dst = Imm_or_reg.parse_exn dst ~labels in
        Some (Store { src; dst })
    | [ "putc"; src ] -> Some (Putc (Imm_or_reg.parse_exn src ~labels))
    | [ "getc"; dst ] -> Some (Getc (Register.parse_exn dst))
    | [ "exit" ] -> Some Exit
    | [ "jmp"; target ] ->
        let target = Imm_or_reg.parse_exn target ~labels in
        Some (Jump { target; cond = None })
    | [ "jeq"; target; dst; src ] -> parse_cond_jump Eq target ~src ~dst
    | [ "jne"; target; dst; src ] -> parse_cond_jump Ne target ~src ~dst
    | [ "jlt"; target; dst; src ] -> parse_cond_jump Lt target ~src ~dst
    | [ "jle"; target; dst; src ] -> parse_cond_jump Le target ~src ~dst
    | [ "jgt"; target; dst; src ] -> parse_cond_jump Gt target ~src ~dst
    | [ "jge"; target; dst; src ] -> parse_cond_jump Ge target ~src ~dst
    | [ "eq"; dst; src ] -> parse_set Eq ~src ~dst
    | [ "ne"; dst; src ] -> parse_set Ne ~src ~dst
    | [ "lt"; dst; src ] -> parse_set Lt ~src ~dst
    | [ "gt"; dst; src ] -> parse_set Gt ~src ~dst
    | [ "le"; dst; src ] -> parse_set Le ~src ~dst
    | [ "ge"; dst; src ] -> parse_set Ge ~src ~dst
    | [ "dump" ] -> Some Dump
    | _ -> None
end

module Segment = struct
  type t = Data | Text [@@deriving sexp, equal, hash]
end

module Address = struct
  type t = { segment : Segment.t; offset : int } [@@deriving sexp, equal, hash]
end

module Data = struct
  type t = Const of int | Label of string [@@deriving sexp, equal, hash]
end

type t = {
  data : Data.t list;
  insns : Instruction.t list;
  labels : (string, Address.t) Hashtbl.t;
}
[@@deriving fields]

let const_heap_start_label = "__reserved_heap_base"
let create ~data ~insns ~labels = { data; insns; labels }
let resolve_label t label = Hashtbl.find t.labels label

module Long = struct
  type t = Label of string | Number of int
end

module Declaration = struct
  type t = Long of Long.t | String of string
end

module Directive = struct
  type t = Text of int | Data of int | Init of Declaration.t

  let parse_exn s labels =
    match Str.bounded_split (Str.regexp "[ \t]+") s 2 with
    | [ ".text" ] -> Some (Text 0)
    | [ ".text"; subsection ] -> Some (Text (Int.of_string subsection))
    | [ ".data" ] -> Some (Data 0)
    | [ ".data"; subsection ] -> Some (Data (Int.of_string subsection))
    | [ ".long"; arg ] -> (
        match maybe_parse_number arg with
        | Some n -> Some (Init (Long (Number n)))
        | None ->
            if Hash_set.mem labels arg then Some (Init (Long (Label arg)))
            else failwith @@ "unknown argument for .long: " ^ arg)
    | [ ".string"; arg ] ->
        let inside_quotes = Str.regexp {|"\(.*\)"|} in
        if Str.string_match inside_quotes arg 0 then
          let unescaped = Scanf.unescaped @@ Str.matched_group 1 arg in
          let with_null_terminator = unescaped ^ "\x00" in
          Some (Init (String with_null_terminator))
        else failwith @@ "invalid argument for .string: " ^ arg
    | _ -> None
end

module Statement = struct
  type t =
    | Label of string
    | Directive of Directive.t
    | Instruction of Instruction.t

  let parse_exn s ~labels =
    match maybe_parse_label s with
    | Some label -> Label label
    | None -> (
        match Directive.parse_exn s labels with
        | Some directive -> Directive directive
        | None -> (
            match Instruction.maybe_parse_exn s ~labels with
            | Some i -> Instruction i
            | None -> failwith @@ "unknown statement: " ^ s))
end

module Section = struct
  type t = Text of int | Data of int
end

let get_all_labels lines =
  lines
  |> List.filter_map ~f:maybe_parse_label
  (* add elvm's magic heap base pointer *)
  |> List.append [ "_edata" ]
  |> Hash_set.of_list (module String)

(* elvm has incredibly weird subsection behavior which we have to parse in
   its strange way *)
let make_sections statements =
  let labels = Hashtbl.create (module String) in
  let data = Hashtbl.create (module Int) in
  let instructions = Hashtbl.create (module Int) in

  let add table key data =
    let deque =
      Hashtbl.find_or_add table key ~default:(fun () -> Deque.create ())
    in
    Deque.enqueue_back deque data
  in

  let current_section = ref None in
  List.iter statements ~f:(fun statement ->
      match statement with
      | Statement.Directive (Text subsection) ->
          current_section := Some (Section.Text subsection)
      | Directive (Data subsection) -> current_section := Some (Data subsection)
      | Directive (Init value) -> (
          match !current_section with
          | Some (Data sub) -> (
              match value with
              | Long (Number n) -> add data sub (Data.Const n)
              | Long (Label label) -> add data sub (Label label)
              | String s ->
                  String.iter s ~f:(fun c ->
                      add data sub (Const (Char.to_int c))))
          | _ -> failwith "cannot declare data outside .data")
      | Label label ->
          let length table sub =
            Hashtbl.find_or_add table sub ~default:(fun () -> Deque.create ())
            |> Deque.length
          in
          let value =
            match !current_section with
            | Some (Data sub) -> (Section.Data sub, length data sub)
            | Some (Text sub) -> (Text sub, length instructions sub)
            | None -> failwith "cannot declare label outside section"
          in
          Hashtbl.add_exn labels ~key:label ~data:value
      | Instruction i -> (
          match !current_section with
          | Some (Text sub) -> add instructions sub i
          | _ -> failwith "cannot declare instruction outside .text"));
  (labels, data, instructions)

let make_program statements =
  let labels, data, instructions = make_sections statements in

  (* squash a table of subsections down into a contiguous array.
     return a mapping from subsection -> offset into array *)
  let flatten table =
    (* sort for deterministic output *)
    let sorted_subsections =
      Hashtbl.to_alist table
      |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    in
    let flattened =
      List.map sorted_subsections ~f:snd
      |> List.concat_map ~f:(fun deque -> Deque.to_list deque)
    in
    let indices =
      List.folding_map sorted_subsections ~init:0 ~f:(fun index (key, deque) ->
          let length = Deque.length deque in
          let next_index = index + length in
          (next_index, (key, index)))
      |> Hashtbl.of_alist_exn (module Int)
    in
    (flattened, indices)
  in

  let data_segment, data_subsection_offsets = flatten data in
  let text_segment, text_subsection_offsets = flatten instructions in

  let make_address offset_lookup segment ~subsection ~offset =
    let offset = offset + Hashtbl.find_exn offset_lookup subsection in
    Address.{ segment; offset }
  in

  let segment_labels =
    Hashtbl.map labels ~f:(fun (section, offset) ->
        match section with
        | Data subsection ->
            make_address data_subsection_offsets Data ~subsection ~offset
        | Text subsection ->
            make_address text_subsection_offsets Text ~subsection ~offset)
  in

  (* add elvm's magic heap base pointer. _edata stores
     a pointer to the start of the heap. *)
  let data_len = List.length data_segment in
  Hashtbl.add_exn segment_labels ~key:"_edata"
    ~data:{ segment = Data; offset = data_len };
  Hashtbl.add_exn segment_labels ~key:const_heap_start_label
    ~data:{ segment = Data; offset = data_len + 1 };
  let data_segment = data_segment @ [ Label const_heap_start_label ] in

  create ~data:data_segment ~insns:text_segment ~labels:segment_labels

let filter_noops lines =
  let is_noop line =
    let is_prefix prefix = String.is_prefix line ~prefix in
    String.is_empty line || is_prefix "#" || is_prefix ".file"
    || is_prefix ".loc"
  in
  List.map lines ~f:String.strip
  |> List.filter ~f:(fun line -> not @@ is_noop line)

let parse_exn source =
  let lines = String.split_lines source |> filter_noops in
  let labels = get_all_labels lines in
  let statements = List.map lines ~f:(Statement.parse_exn ~labels) in
  make_program statements
