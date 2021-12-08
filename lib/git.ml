open Change
open Monad

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

let run : string -> unit mresult =
 fun cmd ->
  let open Unix in
  let ch = open_process_full cmd [||] in
  let _, c = waitpid [] (process_full_pid ch) in
  if c = WEXITED 0 then return () else fail (__LOC__, status_msg cmd c)

let run_lines : string -> string list mresult =
 fun cmd ->
  let open Unix in
  let ((out, _, _) as ch) = open_process_full cmd [||] in
  let _, c = waitpid [] (process_full_pid ch) in
  if c = WEXITED 0 then return (read_lines out)
  else fail (__LOC__, status_msg cmd c)

let run_string : string -> string mresult =
 fun cmd -> run_lines cmd >>= fun l -> return (String.concat "\n" l)

let rmrf path =
  run Format.(sprintf "rm -rf %s" path)
  >>? (__LOC__, Format.sprintf "%S could not be removed" path)

(** {2. Types} *)

type hash = Hash of string

type repository = Repo of string

let hash_from_string str = Hash str

(** {2. Core functions} *)

let open_repository : ?path:string -> unit -> repository mresult =
 fun ?(path = ".") () ->
  let cmd = Format.(sprintf "git -C %s rev-parse --show-toplevel" path) in
  run_string cmd >>? (__LOC__, "Failed to open repository at " ^ path)
  >>= fun r -> return (Repo r)

let root_of (Repo path) = path

let mktempdir () = run_string "mktemp -d"

let with_tmp_dir ?(clean = true) : (unit -> 'a mresult) -> 'a mresult =
 fun k ->
  let cwd = Sys.getcwd () in
  mktempdir () >>= fun tmpd ->
  let () = Sys.chdir tmpd in
  k () >>= fun res ->
  let () = Sys.chdir cwd in
  let () = if clean then ignore (rmrf tmpd) in
  return res

let clone ?branch git =
  let branch' =
    match branch with None -> "" | Some branch -> "--branch " ^ branch ^ " "
  in
  run Format.(sprintf "git clone %s%s ." branch' git)
  >>? ( __LOC__,
        Format.sprintf "Failed to clone %S" git
        ^ Option.fold ~none:"" ~some:(fun b -> " with branch " ^ b) branch )

let with_tmp_clone :
    repository -> ?hash:hash -> (repository -> 'a mresult) -> 'a mresult =
 fun (Repo path) ?hash k ->
  with_tmp_dir (fun () ->
      clone path >>= fun () ->
      (match hash with
      | None -> return ()
      | Some (Hash h) ->
          (run @@ Format.(sprintf "git checkout %s" h))
          >>? (__LOC__, Format.sprintf "Failed to checkout %S " h))
      >>= fun () ->
      open_repository () >>= fun r -> k r)

let clone_repository : ?branch:string -> string -> repository mresult =
 fun ?branch git ->
  with_tmp_dir ~clean:false (fun () ->
      clone ?branch git >>= fun () -> open_repository ())

let find_last_merge_commit : repository -> hash mresult =
 fun (Repo r) ->
  let cmd =
    Format.(
      sprintf "git -C %s --no-pager log --merges -n1 --pretty=format:%%H" r)
  in
  run_string cmd >>= function
  | "" -> fail (__LOC__, "Failed to find last merge commit")
  | h -> return (Hash h)

let get_commits_after : repository -> hash -> hash list mresult =
 fun (Repo r) (Hash h) ->
  run_lines
    Format.(sprintf "git -C %s rev-list %s..HEAD --topo-order --reverse" r h)
  >>? (__LOC__, Format.sprintf "Failed to find commits after %S" h)
  >>= function
  | [] -> fail (__LOC__, Format.sprintf "There is no commits after %S" h)
  | l -> return (List.map hash_from_string l)

let changes_of : repository -> hash -> changes mresult =
 fun (Repo r) (Hash h) ->
  let show_opts = "--stat=1000 --pretty=format:'' --name-status" in
  run_lines Format.(sprintf "git --no-pager -C %s show %s %s" r show_opts h)
  >>? (__LOC__, Format.sprintf "Failed to find changes for commit %S" h)
  >>= fun l -> return (List.map change_from_string l)

let get_changes : repository -> since:hash -> changes mresult =
 fun r ~since ->
  get_commits_after r since >>= fun commits ->
  List.fold_left
    (fun cs h ->
      cs >>= fun cs ->
      changes_of r h >>= fun changes -> return (merge_changes cs changes))
    (return []) commits
