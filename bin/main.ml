(** [choose_file "target.ml"] returns "target.mli" if it exists,
    "target.ml" otherwise. *)
let choose_file target =
  let mli_target = target ^ "i" in
  if Sys.file_exists mli_target then mli_target else target

let _ =
  let target = choose_file Sys.argv.(1) in

  let deps = Toplevel.to_entries target in

  Format.(
    printf "@[<v>%a@]" (pp_print_list ~pp_sep:pp_print_space Entry.pp) deps)
