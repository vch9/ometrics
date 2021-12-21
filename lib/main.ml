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
      match Entry.compare ea eb with
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

(** [link_prefix git hash] computes the link prefix based on [git] and [hash].

    Example:
    link_prefix "https://gitlab.com/nomadic-labs/tezos.git" "your_hash" it creates:
    https://gitlab.com/nomadic-labs/tezos/-/tree/your_hash
*)
let link_prefix git hash =
  let is_gitlab =
    let re = Str.regexp "https:\\/\\/gitlab\\.com\\/\\(.+\\)\\/tezos\\.git" in
    Str.string_match re git 0
  in
  if is_gitlab then
    let group = Str.matched_group 1 git in
    Some (Printf.sprintf "https://gitlab.com/%s/tezos/-/blob/%s/" group hash)
  else None

(** [report_full entries] creates a full report for [entries]. It list
    every unducommented entry found in [entries] with a clickable link
    to the line in question. In a markdown format. *)
let report_full fmt ?git ?report h entries =
  let ( >>= ) = Option.bind in
  let Git.(Hash h) = h in
  let with_link = git >>= fun git -> link_prefix git h in
  let fmt =
    Option.fold ~none:fmt
      ~some:(fun report ->
        let oc = open_out report in
        Format.formatter_of_out_channel oc)
      report
  in
  List.iter
    (fun (p, deps) ->
      Format.(
        if 0 < List.length deps then (
          fprintf fmt "@[<v># `%s`@ @ @]" p;
          fprintf fmt "@[<v>%a@ @ @]"
            (pp_print_list ~pp_sep:pp_print_space (fun fmt e ->
                 fprintf fmt "- `%a`" (Entry.pp ~with_mark:false ?with_link) e))
            deps)))
    entries

(** [report_partial entries] creates a partial report.Annot

    - If there is more than 5 files: go to recap.
    - If there is less than 5 files and more than 3 changes per file: go to recap
    - Else: {!report_full}
*)
let report_partial fmt ?git ?report h entries =
  let n = List.length entries in
  if n > 5 || List.exists (fun (_, l) -> List.length l > 3) entries then
    match report with
    | None ->
        Format.fprintf fmt
          "There is multiple undocumented entries, provide `--report [path]` \
           to store the report.";
        report_full fmt ?git ?report h entries
    | Some path ->
        let () = report_full fmt ?git ?report h entries in
        Format.fprintf fmt
          "There is multiple undocumentend entries, please see %s" path
  else report_full fmt ?git h entries

let get_repo = function
  | `Git (git, branch) -> Git.clone_repository ?branch git
  | `Path path -> Git.open_repository ~path ()

let get_hash repo = function
  | "" -> Git.find_last_merge_commit repo
  | s -> return (Git.hash_from_string s)

let check_mr ?output ?git ?report ~partial r h exclude_files exclude_re :
    unit mresult =
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
  let fmt =
    Option.fold ~none:Format.std_formatter
      ~some:(fun file ->
        let oc = open_out file in
        Format.formatter_of_out_channel oc)
      output
  in

  return
  @@
  if partial then report_partial fmt ?git ?report h undocumented_entries
  else report_full fmt ?git ?report h undocumented_entries

let opt_string = function "" -> None | x -> Some x

let check_clone git branch commit exclude_files exclude_re output partial report
    =
  let output = opt_string output and report = opt_string report in
  run
    (let branch = match branch with "" -> None | x -> Some x in
     get_repo @@ `Git (git, branch) >>= fun repo ->
     get_hash repo commit >>= fun hash ->
     check_mr ~git repo hash exclude_files exclude_re ?output ?report ~partial)

let check path commit exclude_files exclude_re output partial report =
  let output = opt_string output and report = opt_string report in
  run
    ( get_repo (`Path path) >>= fun repo ->
      get_hash repo commit >>= fun hash ->
      check_mr repo hash exclude_files exclude_re ?output ?report ~partial )
