open Core
open Elvm

let () =
  Command_unix.run
    (Command.basic
       ~summary:"Optimize an elvm program."
       (let%map_open.Command file =
          flag "-file" ~doc:"FILE elvm program" (required Filename_unix.arg_type)
        in
        fun () ->
          In_channel.read_all file
          |> Eir.parse_exn
          |> Elvm.Lift.f
          |> [%sexp_of: Program.t]
          |> print_s))
;;
