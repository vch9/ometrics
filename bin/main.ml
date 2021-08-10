open Ometrics
open Ometrics.Change

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

let rec filter_duplicate_undocumented = function
  | (path, _) :: (path', undoc) :: rst when String.equal path (path' ^ "i") ->
      (path, undoc) :: filter_duplicate_undocumented rst
  | (path, undoc) :: rst ->
      (choose_file path, undoc) :: filter_duplicate_undocumented rst
  | [] -> []

let conciliate_undocumented_all before after chs =
  List.map (conciliate_undocumented before after) chs
  |> filter_duplicate_undocumented

let check_mr path =
  let r = Git.open_repository ~path () in

  let h =
    try Git.hash_from_string Sys.argv.(2)
    with _ -> Git.find_last_merge_commit r
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

let _ = check_mr @@ try Sys.argv.(1) with _ -> "."
