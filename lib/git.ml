type change =
  | Addition of string
  | Deletion of string
  | Edition of string
  | Renaming of string * string

type changes = change list

let rec add_deletion path = function
  | (Addition path' | Renaming (_, path') | Edition path') :: rst
    when path = path' ->
      rst
  | x :: rst -> x :: add_deletion path rst
  | [] -> [ Deletion path ]

let rec add_renaming before after = function
  | Addition path :: rst when before = path -> Addition after :: rst
  | Edition path :: rst when path = before -> Renaming (before, after) :: rst
  | Renaming (origin, path) :: rst when path = before ->
      Renaming (origin, after) :: rst
  | x :: rst -> x :: add_renaming before after rst
  | [] -> [ Renaming (before, after) ]

let rec add_edition path = function
  | ((Addition path' | Renaming (_, path') | Edition path') as ch) :: rst
    when path = path' ->
      ch :: rst
  | x :: rst -> x :: add_edition path rst
  | [] -> [ Edition path ]

let add_change l = function
  | Deletion path -> add_deletion path l
  | Edition path -> add_edition path l
  | Renaming (before, after) -> add_renaming before after l
  | ch -> ch :: l

let merge_changes = List.fold_left add_change

let ignore _ = ()

let run : string -> unit =
 fun cmd ->
  let open Unix in
  let ch = open_process_full cmd [||] in
  let _, c = waitpid [] (process_full_pid ch) in
  if c = WEXITED 0 then () else assert false

let rec read_lines ch =
  try
    let x = input_line ch in
    x :: read_lines ch
  with End_of_file -> []

let run_lines : string -> string list =
 fun cmd ->
  let open Unix in
  let ((out, _, _) as ch) = open_process_full cmd [||] in
  let _, c = waitpid [] (process_full_pid ch) in
  if c = WEXITED 0 then read_lines out else assert false

let run_string : string -> string =
 fun cmd -> run_lines cmd |> String.concat "\n"

let rmrf path = run Format.(sprintf "rm -rf %s" path)

type hash = Hash of string

let hash_from_string str = Hash str

type repository = Repo of string

let open_repository : ?path:string -> unit -> repository =
 fun ?(path = ".") () ->
  Repo (run_string Format.(sprintf "git -C %s rev-parse --show-toplevel" path))

let root_of (Repo path) = path

let mktempdir () = run_string "mktemp -d"

let with_tmp_dir : (unit -> 'a) -> 'a =
 fun k ->
  let cwd = Sys.getcwd () in
  try
    let tmpd = mktempdir () in
    Sys.chdir tmpd;
    let res = k () in
    Sys.chdir cwd;
    ignore (rmrf tmpd);
    res
  with e ->
    Sys.chdir cwd;
    raise e

let with_tmp_clone : repository -> ?hash:hash -> (repository -> 'a) -> 'a =
 fun (Repo path) ?hash k ->
  with_tmp_dir (fun () ->
      run Format.(sprintf "git clone %s ." path);
      Option.fold hash ~none:() ~some:(fun (Hash h) ->
          (run @@ Format.(sprintf "git checkout %s" h));
          ());
      k @@ open_repository ())

let find_last_merge_commit : repository -> hash =
 fun (Repo r) ->
  Hash
    (run_string
       Format.(
         sprintf "git -C %s --no-pager log --merges -n1 --pretty=format:%%H" r))

let get_commits_after : repository -> hash -> hash list =
 fun (Repo r) (Hash h) ->
  run_lines
    Format.(sprintf "git -C %s rev-list %s..HEAD --topo-order --reverse" r h)
  |> List.map hash_from_string

let addition_from_string str =
  let rxp = Str.regexp "^[AC]\t*\\(.*\\)$" in
  if Str.string_match rxp str 0 then Some (Addition (Str.matched_group 1 str))
  else None

let deletion_from_string str =
  let rxp = Str.regexp "^D\t*\\(.*\\)$" in
  if Str.string_match rxp str 0 then Some (Deletion (Str.matched_group 1 str))
  else None

let edition_from_string str =
  let rxp = Str.regexp "^M\t*\\(.*\\)$" in
  if Str.string_match rxp str 0 then Some (Edition (Str.matched_group 1 str))
  else None

let renaming_from_string str =
  let rxp = Str.regexp "^R[0-9]*\t*\\(.*\\)\t*\\(.*\\)$" in
  if Str.string_match rxp str 0 then
    Some (Renaming (Str.matched_group 1 str, Str.matched_group 2 str))
  else None

let change_from_string str =
  match addition_from_string str with
  | Some c -> c
  | None -> (
      match deletion_from_string str with
      | Some c -> c
      | None -> (
          match edition_from_string str with
          | Some c -> c
          | None -> (
              match renaming_from_string str with
              | Some c -> c
              | None -> assert false)))

let changes_of : repository -> hash -> changes =
 fun (Repo r) (Hash h) ->
  let show_opts = "--stat=1000 --pretty=format:'' --name-status" in
  run_lines Format.(sprintf "git --no-pager -C %s show %s %s" r show_opts h)
  |> List.map change_from_string

let name_ext path =
  let base = Filename.basename path in
  let ext = Filename.extension base in
  let file = Filename.remove_extension base in
  (file, ext)

let is_ml_file path =
  let _, ext = name_ext path in
  ext = ".ml" || ext = ".mli"

let is_ml_change : change -> bool = function
  | Addition path | Edition path -> is_ml_file path
  | Renaming (before, after) -> is_ml_file before && is_ml_file after
  | _ -> false

let get_changes : repository -> since:hash -> changes =
 fun r ~since ->
  get_commits_after r since
  |> List.fold_left (fun cs h -> merge_changes cs (changes_of r h)) []

let files_to_analyze =
  let rec aux accb acca = function
    | Deletion _ :: rst -> aux accb acca rst
    | Addition path :: rst -> aux accb (path :: acca) rst
    | Edition path :: rst -> aux (path :: accb) (path :: acca) rst
    | Renaming (before, after) :: rst ->
        aux (before :: accb) (after :: acca) rst
    | [] -> (List.sort String.compare accb, List.sort String.compare acca)
  in
  aux [] []
