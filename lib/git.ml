open Change

(** {2. Utilities} *)

let rec read_lines ch =
  try
    let x = input_line ch in
    x :: read_lines ch
  with End_of_file -> []

let status_msg cmd status =
  let open Unix in
  let status =
    match status with
    | WEXITED n -> "WEXITED " ^ string_of_int n
    | WSIGNALED n -> "WSIGNALED " ^ string_of_int n
    | WSTOPPED n -> "WSTOPPED " ^ string_of_int n
  in
  Format.sprintf "%S exited with %s" cmd status

let run_lines : string -> string list =
 fun cmd ->
  let open Unix in
  let ((out, _, err) as ch) = open_process_full cmd [||] in
  let lines_out = read_lines out in
  let lines_err = read_lines err in
  let _, c = waitpid [] (process_full_pid ch) in
  if c = WEXITED 0 then lines_out
  else (
    Debug.dbg "Error on %s" cmd;
    let msg = lines_err |> String.concat "\n" in
    Debug.dbg "Error: %s" msg;
    failwith (status_msg cmd c))

let run : string -> unit = fun cmd -> run_lines cmd |> ignore

let run_string : string -> string =
 fun cmd ->
  let l = run_lines cmd in
  String.concat "\n" l

let rmrf path = run Format.(sprintf "rm -rf %s" path)

(** {2. Types} *)

type hash = Hash of string
type repository = Repo of string

let hash_from_string str = Hash str

(** {2. Core functions} *)

let open_repository : ?path:string -> unit -> repository =
 fun ?(path = ".") () ->
  let cmd = Format.(sprintf "git -C %s rev-parse --show-toplevel" path) in
  let r = run_string cmd in
  Repo r

let root_of (Repo path) = path
let mktempdir () = run_string "mktemp -d"

let with_tmp_dir ?(clean = true) : (unit -> 'a) -> 'a =
 fun k ->
  let cwd = Sys.getcwd () in
  let tmpd = mktempdir () in
  let () = Sys.chdir tmpd in
  let res = k () in
  let () = Sys.chdir cwd in
  let () = if clean then ignore (rmrf tmpd) in
  res

let clone ?branch git =
  let branch' =
    match branch with None -> "" | Some branch -> "--branch " ^ branch ^ " "
  in
  run Format.(sprintf "git clone %s%s ." branch' git)

let with_tmp_clone : repository -> ?hash:hash -> (repository -> 'a) -> 'a =
 fun (Repo path) ?hash k ->
  with_tmp_dir (fun () ->
      let () = clone path in
      let () =
        Option.iter
          (fun (Hash h) -> run Format.(sprintf "git checkout %s" h))
          hash
      in
      let r = open_repository () in
      k r)

let clone_repository : ?branch:string -> string -> repository =
 fun ?branch git ->
  with_tmp_dir ~clean:false (fun () ->
      let () = clone ?branch git in
      open_repository ())

let find_last_commit : repository -> hash =
 fun (Repo r) ->
  let cmd = Format.sprintf "git -C %s rev-parse HEAD" r in
  match run_string cmd with
  | "" -> failwith "Failed to find last commit"
  | h -> Hash h

let find_last_merge_commit : repository -> hash =
 fun (Repo r) ->
  let cmd =
    Format.(
      sprintf "git -C %s --no-pager log --merges -n1 --pretty=format:%%H" r)
  in
  match run_string cmd with
  | "" -> failwith "Failed to find last merge commit"
  | h -> Hash h

let get_commits_after : repository -> hash -> hash list =
 fun (Repo r) (Hash h) ->
  let commits =
    run_lines
      Format.(sprintf "git -C %s rev-list %s..HEAD --topo-order --reverse" r h)
  in
  match commits with
  | [] -> failwith (Format.sprintf "There is no commits after %S" h)
  | l -> List.map hash_from_string l

let changes_of : repository -> hash -> changes =
 fun (Repo r) (Hash h) ->
  let show_opts = "--stat=1000 --pretty=format:'' --name-status" in
  let changes =
    run_lines Format.(sprintf "git --no-pager -C %s show %s %s" r show_opts h)
  in
  List.map change_from_string changes

let get_changes : repository -> since:hash -> changes =
 fun r ~since ->
  let commits = get_commits_after r since in
  List.fold_left
    (fun cs h ->
      let changes = changes_of r h in
      merge_changes cs changes)
    [] commits
