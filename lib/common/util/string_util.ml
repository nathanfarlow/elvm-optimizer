let indent_string s ~indent =
  s |> String.split ~on:'\n'
  |> List.map ~f:(fun s -> String.make (indent * 2) ' ' ^ s)
  |> String.concat ~sep:"\n"
