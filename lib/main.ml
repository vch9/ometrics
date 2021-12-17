open Change
open Monad

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
  | "" -> Git.find_last_merge_commit repo
  | s -> return (Git.hash_from_string s)

let check_mr r h exclude_files exclude_re : string mresult =
  (* We fetch every changes since [h] in term of files *)
  Git.get_changes r ~since:h >>= fun changes ->
  let changes =
    changes |> List.filter is_ml_change
    |> List.filter (fun ch -> not (is_excluded exclude_files exclude_re ch))
  in

  (* We compute the files that were present before the changes and after *)
  let before, after = Change.files_to_analyze changes in

  (* We look for undocumented entries in files before the changes *)
  Git.with_tmp_clone r ~hash:h (fun _r ->
      return @@ List.map (fun p -> (p, find_undocumented_entries p)) before)
  >>= fun before_undoc ->
  (* We look for undocumented entries in files after the changes *)
  Git.with_tmp_clone r (fun _r ->
      return @@ List.map (fun p -> (p, find_undocumented_entries p)) after)
  >>= fun after_undoc ->
  (* We remove undocumented entries if they were also undocumented before *)
  let undocumented_entries =
    conciliate_undocumented_all before_undoc after_undoc changes
  in

  (* Finally, we print the undocumentend entries found *)
  return
  @@ List.fold_left
       (fun acc (p, deps) ->
         let open Format in
         let pp fmt (p, deps) =
           if 0 < List.length deps then (
             fprintf fmt "@[<v># `%s`@ @ @]" p;
             fprintf fmt "@[<v>%a@ @ @]"
               (pp_print_list ~pp_sep:pp_print_space (fun fmt e ->
                    fprintf fmt "- `%a`" (Entry.pp ~with_mark:false) e))
               deps)
         in
         match deps with [] -> acc | _ -> asprintf "%s%a" acc pp (p, deps))
       "" undocumented_entries

let check_clone_temp git branch commit exclude_files exclude_re =
  run_dry
    (let branch = match branch with "" -> None | x -> Some x in
     get_repo @@ `Git (git, branch) >>= fun repo ->
     get_hash repo commit >>= fun hash ->
     check_mr repo hash exclude_files exclude_re)

let check_clone git branch commit exclude_files exclude_re =
  run
    (let branch = match branch with "" -> None | x -> Some x in
     get_repo @@ `Git (git, branch) >>= fun repo ->
     get_hash repo commit >>= fun hash ->
     check_mr repo hash exclude_files exclude_re >>= fun msg ->
     return (print_endline msg))

let check path commit exclude_files exclude_re =
  run
    ( get_repo (`Path path) >>= fun repo ->
      get_hash repo commit >>= fun hash ->
      check_mr repo hash exclude_files exclude_re >>= fun msg ->
      return (print_endline msg) )
