open Change

(** [choose_file "target.ml"] returns "target.mli" if it exists,
    "target.ml" otherwise. *)
let choose_file target =
  let mli_target = target ^ "i" in
  if Sys.file_exists mli_target then mli_target else target

let find_undocumented_entries path =
  choose_file path |> Toplevel.to_entries |> List.sort compare
  |> List.filter Entry.is_not_documented

(** [conciliate before after] removes undocumented entry that appears in both
    [before] and [after]. *)
let rec conciliate (before : Entry.t list) (after : Entry.t list) : Entry.t list
    =
  match (before, after) with
  | eb :: rstb, ea :: rsta -> (
      match compare ea eb with
      | 0 -> conciliate rstb rsta
      | x when x < 0 -> ea :: conciliate before rsta
      | _ -> conciliate rstb after)
  | _, rsta -> rsta

(** [conciliate_undocumented before after changes] retrives undocumented entries in after
    based on [changes] and [before].

    e.g.
    - if value "x" was undocumented before and the file has been edited and it is still
    not documented, we ignore the value.
*)
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

(** {2. Check } *)

let get_repo = function
  | `Git (git, branch) -> Git.clone_repository ?branch git
  | `Path path -> Git.open_repository ~path ()

let get_hash repo = function
  | "" -> (
      match Git.find_last_merge_commit repo with
      | None -> failwith "Last merge commit could not be found"
      | Some h -> h)
  | s -> Git.hash_from_string s

let check_mr r h =
  (* We fetch every changes since [h] in term of files *)
  let changes = Git.get_changes r ~since:h |> List.filter is_ml_change in

  (* We compute the files that were present before the changes and after *)
  let before, after = Change.files_to_analyze changes in

  (* We look for undocumented entries in files before the changes *)
  let before_undoc =
    Git.with_tmp_clone r ~hash:h (fun _r ->
        List.map (fun p -> (p, find_undocumented_entries p)) before)
  in

  (* We look for undocumented entries in files after the changes *)
  let after_undoc =
    Git.with_tmp_clone r (fun _r ->
        List.map (fun p -> (p, find_undocumented_entries p)) after)
  in

  (* We remove undocumented entries if they were also undocumented before *)
  let undocumented_entries =
    conciliate_undocumented_all before_undoc after_undoc changes
  in

  (* Finally, we print the undocumentend entries found *)
  List.iter
    (fun (p, deps) ->
      Format.(
        if 0 < List.length deps then (
          printf "@[<v># `%s`@ @ @]" p;
          printf "@[<v>%a@ @ @]"
            (pp_print_list ~pp_sep:pp_print_space (fun fmt e ->
                 fprintf fmt "- `%a`" (Entry.pp ~with_mark:false) e))
            deps)))
    undocumented_entries

let check_clone git branch commit =
  let branch = match branch with "" -> None | x -> Some x in
  let repo = get_repo @@ `Git (git, branch) in
  let hash = get_hash repo commit in
  Ok (check_mr repo hash)

let check path commit =
  let repo = get_repo @@ `Path path in
  let hash = get_hash repo commit in
  Ok (check_mr repo hash)
