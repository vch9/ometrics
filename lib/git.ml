open Change

(** {2. Utilities} *)

exception BadStatus of string

let rec read_lines ch =
  try
    let x = input_line ch in
    x :: read_lines ch
  with End_of_file -> []

let run_error_msg cmd status =
  let open Unix in
  let status =
    match status with
    | WEXITED d -> "WEXITED " ^ string_of_int d
    | WSIGNALED d -> "WSIGNALED " ^ string_of_int d
    | WSTOPPED d -> "WSTOPPED " ^ string_of_int d
  in
  Printf.sprintf "%S exited with status: %s" cmd status

let run : string -> unit =
 fun cmd ->
  let open Unix in
  let ch = open_process_full cmd [||] in
  let _, c = waitpid [] (process_full_pid ch) in
  if c = WEXITED 0 then () else raise (BadStatus (run_error_msg cmd c))

let run_lines : string -> string list =
 fun cmd ->
  let open Unix in
  let ((out, _, _) as ch) = open_process_full cmd [||] in
  let _, c = waitpid [] (process_full_pid ch) in
  if c = WEXITED 0 then read_lines out
  else raise (BadStatus (run_error_msg cmd c))

let run_string : string -> string =
 fun cmd -> run_lines cmd |> String.concat "\n"

let rmrf path = run Format.(sprintf "rm -rf %s" path)

(** {2. Types} *)

type hash = Hash of string

type repository = Repo of string

let hash_from_string str = Hash str

(** {2. Core functions} *)

let open_repository : ?path:string -> unit -> repository =
 fun ?(path = ".") () ->
  Repo (run_string Format.(sprintf "git -C %s rev-parse --show-toplevel" path))

let root_of (Repo path) = path

let mktempdir () = run_string "mktemp -d"

let with_tmp_dir ?(clean = true) : (unit -> 'a) -> 'a =
 fun k ->
  let cwd = Sys.getcwd () in
  try
    let tmpd = mktempdir () in
    Sys.chdir tmpd;
    let res = k () in
    Sys.chdir cwd;
    if clean then ignore (rmrf tmpd);
    res
  with e ->
    Sys.chdir cwd;
    raise e

let clone ?branch git =
  let branch =
    match branch with None -> "" | Some branch -> "--branch " ^ branch
  in
  run Format.(sprintf "git clone %s %s ." branch git)

let with_tmp_clone : repository -> ?hash:hash -> (repository -> 'a) -> 'a =
 fun (Repo path) ?hash k ->
  with_tmp_dir (fun () ->
      clone path;
      Option.fold hash ~none:() ~some:(fun (Hash h) ->
          (run @@ Format.(sprintf "git checkout %s" h));
          ());
      k @@ open_repository ())

let clone_repository : ?branch:string -> string -> repository =
 fun ?branch git ->
  with_tmp_dir ~clean:false (fun () ->
      let () = clone ?branch git in
      open_repository ())

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
