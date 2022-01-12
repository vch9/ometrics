open Change
open Monad
open Report

(** [choose_file "target.ml"] returns "target.mli" if it exists,
    "target.ml" otherwise. *)
let choose_file target =
  let mli_target = target ^ "i" in
  if Sys.file_exists mli_target then mli_target else target

let find_entries path =
  choose_file path |> Toplevel.to_entries |> List.sort compare

(** [conciliate before after] removes entry that appears in both
    [before] and [after]. *)
let rec conciliate (before : Entry.t list) (after : Entry.t list) : Entry.t list
    =
  match (before, after) with
  | eb :: rstb, ea :: rsta -> (
      match Entry.compare ea eb with
      | 0 -> conciliate rstb rsta
      | x when x < 0 -> ea :: conciliate before rsta
      | _ -> conciliate rstb after)
  | _, rsta -> rsta

(** [conciliate before after changes] retrives entries in after
    based on [changes] and [before]. *)
let conciliate before after = function
  | Addition path -> (path, List.assoc path after)
  | Edition path ->
      (path, conciliate (List.assoc path before) (List.assoc path after))
  | Renaming (path, path') ->
      (path', conciliate (List.assoc path before) (List.assoc path' after))
  | _ -> assert false

(** [filter_duplicate l] removes implementation files when
    the interface is present. The implementation file (.ml) must be _before_
    the interface one (.mli). *)
let rec filter_duplicate = function
  | (path, _) :: (path', undoc) :: rst when String.equal path (path' ^ "i") ->
      (path, undoc) :: filter_duplicate rst
  | (path, undoc) :: rst -> (choose_file path, undoc) :: filter_duplicate rst
  | [] -> []

let conciliate_all before after chs = List.map (conciliate before after) chs

(** {2. Check } *)

let get_repo = function
  | `Git (git, branch) -> Git.clone_repository ?branch git
  | `Path path -> Git.open_repository ~path ()

let get_hash repo = function
  | "" -> Git.find_last_merge_commit repo
  | s -> return (Git.hash_from_string s)

let check_mr ?output ?git ?title ~clickable ~format r h exclude_files
    exclude_file_re exclude_entry_re : unit mresult =
  (* We fetch every changes since [h] in term of files *)
  Git.get_changes r ~since:h >>= fun changes ->
  let changes =
    changes |> List.filter is_ml_change
    |> List.filter (fun ch ->
           not (is_excluded exclude_files exclude_file_re ch))
  in

  (* We compute the files that were present before the changes and after *)
  let before, after = Change.files_to_analyze changes in
  let last_commit = ref None in

  (* We look for entries in files before the changes *)
  Git.with_tmp_clone r ~hash:h (fun _r ->
      return @@ List.map (fun p -> (p, find_entries p)) before)
  >>= fun before_undoc ->
  (* We look for entries in files after the changes *)
  Git.with_tmp_clone r (fun r ->
      Git.find_last_commit r >>= fun hash ->
      last_commit := Some hash;
      return @@ List.map (fun p -> (p, find_entries p)) after)
  >>= fun after_undoc ->
  (* We remove entries if they were also undocumented before *)
  let entries =
    conciliate_all before_undoc after_undoc changes
    |> List.filter (fun (_, deps) -> List.length deps > 0)
  in

  let entries =
    List.filter_map
      (fun (p, deps) ->
        let deps =
          List.fold_left
            (fun deps re -> Entry.exclude_entries_re deps re)
            deps exclude_entry_re
        in
        match deps with [] -> None | _ -> Some (p, deps))
      entries
  in

  return
  @@ report_full ?output ?title ~clickable ~format ?git
       (Option.get !last_commit) entries

let opt_string = function "" -> None | x -> Some x

let format markdown html gitlab =
  if markdown then `Markdown
  else if html then `Html
  else if gitlab then `Gitlab
  else `Classic

let check_clone git branch commit exclude_files exclude_file_re exclude_entry_re
    output clickable markdown html gitlab title =
  let output = opt_string output in
  let title = opt_string title in
  let format = format markdown html gitlab in
  run
    (let branch = match branch with "" -> None | x -> Some x in
     get_repo @@ `Git (git, branch) >>= fun repo ->
     get_hash repo commit >>= fun hash ->
     check_mr ~git repo hash exclude_files exclude_file_re exclude_entry_re
       ?output ~clickable ~format ?title)

let check path commit exclude_files exclude_file_re exclude_entry_re output
    markdown html gitlab =
  let output = opt_string output in
  let format = format markdown html gitlab in
  run
    ( get_repo (`Path path) >>= fun repo ->
      get_hash repo commit >>= fun hash ->
      check_mr repo hash exclude_files exclude_file_re exclude_entry_re ?output
        ~clickable:false ~format )
