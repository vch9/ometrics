open Ometrics
open Ometrics.Change
open Cmdliner

(** [choose_file "target.ml"] returns "target.mli" if it exists,
    "target.ml" otherwise. *)
let choose_file target =
  let mli_target = target ^ "i" in
  if Sys.file_exists mli_target then mli_target else target

let find_undocumented_entries path =
  choose_file path |> Toplevel.to_entries |> List.sort compare
  |> List.filter Entry.is_not_documented

let rec conciliate before after =
  match (before, after) with
  | eb :: rstb, ea :: rsta -> (
      match compare ea eb with
      | 0 -> conciliate rstb rsta
      | x when x < 0 -> ea :: conciliate before rsta
      | _ -> conciliate rstb after)
  | _, rsta -> rsta

let conciliate_undocumented before after = function
  | Addition path -> (path, List.assoc path after)
  | Edition path ->
      (path, conciliate (List.assoc path before) (List.assoc path after))
  | Renaming (path, path') ->
      (path', conciliate (List.assoc path before) (List.assoc path' after))
  | _ -> assert false

(** [filter_duplicate_undocumented l] removes implementation files when
    the interface is present. The implementation file (.ml) must be _before_
    the interface one (.mli). *)
let rec filter_duplicate_undocumented = function
  | (path, _) :: (path', undoc) :: rst when String.equal path (path' ^ "i") ->
      (path, undoc) :: filter_duplicate_undocumented rst
  | (path, undoc) :: rst ->
      (choose_file path, undoc) :: filter_duplicate_undocumented rst
  | [] -> []

let conciliate_undocumented_all before after chs =
  List.map (conciliate_undocumented before after) chs
  |> filter_duplicate_undocumented

let check_mr path hash =
  let r = Git.open_repository ~path () in

  let h =
    match hash with
    | "" -> Git.find_last_merge_commit r
    | s -> Git.hash_from_string s
  in

  let changes = Git.get_changes r ~since:h |> List.filter is_ml_change in

  let before, after = Change.files_to_analyze changes in

  let before =
    Git.with_tmp_clone r ~hash:h (fun _r ->
        List.map (fun p -> (p, find_undocumented_entries p)) before)
  in

  let todo =
    Git.with_tmp_clone r (fun _r ->
        let after =
          List.map (fun p -> (p, find_undocumented_entries p)) after
        in

        conciliate_undocumented_all before after changes)
  in

  List.iter
    (fun (p, deps) ->
      Format.(
        if 0 < List.length deps then (
          printf "@[<v># `%s`@ @ @]" p;
          printf "@[<v>%a@ @ @]"
            (pp_print_list ~pp_sep:pp_print_space (fun fmt e ->
                 fprintf fmt "- `%a`" (Entry.pp ~with_mark:false) e))
            deps)))
    todo

let check path commit = Ok (check_mr path commit)

let name = "ometrics"

let version = "dev"

let check =
  let doc =
    "Check undocument function between current head and last merge commit."
  in
  let exits = Term.default_exits in

  let path =
    let doc = "Git project path." in
    Arg.(value & opt string "." & info [ "p"; "path" ] ~doc ~docv:"PATH")
  in

  let commit =
    let doc = "Base commit to check." in
    Arg.(value & opt string "" & info [ "h"; "hash" ] ~doc ~docv:"COMMIT")
  in
  ( Term.(term_result (const check $ path $ commit)),
    Term.info "check" ~version ~doc ~exits )

let default =
  let exits = Term.default_exits in
  (Term.(ret (const (`Help (`Pager, None)))), Term.info name ~version ~exits)

let cmds = [ check ]

let () = Term.(exit @@ eval_choice default cmds)
