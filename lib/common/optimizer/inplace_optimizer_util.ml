open Core

let optimize_until_unchanging optim_func t =
  let rec loop t has_changed_before =
    let just_changed = optim_func t in
    if just_changed then loop t true else has_changed_before || just_changed
  in
  loop t false
