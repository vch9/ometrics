open Change

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

let changes_of : repository -> hash -> changes =
 fun (Repo r) (Hash h) ->
  let show_opts = "--stat=1000 --pretty=format:'' --name-status" in
  run_lines Format.(sprintf "git --no-pager -C %s show %s %s" r show_opts h)
  |> List.map change_from_string

let get_changes : repository -> since:hash -> changes =
 fun r ~since ->
  get_commits_after r since
  |> List.fold_left (fun cs h -> merge_changes cs (changes_of r h)) []
