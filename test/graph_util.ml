open! Core
open Async
open Elvm

let to_ascii graph to_string =
  Process.run_exn ~prog:"graph-easy" ~args:[] ~stdin:(Graph.to_dot graph to_string) ()
;;
