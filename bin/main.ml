open Ometrics.Main
open Cmdliner

let name = "ometrics"
let version = "dev"

let exclude_files =
  let doc =
    "Exclude files at $(i,PATH) from the merge request's analysis.\n\
    \    If $(i,PATH) ends with a path separator, it is treated as a directory \
     name."
  in
  Arg.(value & opt_all string [] & info [ "exclude-file" ] ~doc ~docv:"PATH")

let exclude_file_re =
  let doc = "Exclude files matching RE from the merge request's analysis." in
  Arg.(value & opt_all string [] & info [ "exclude-file-re" ] ~doc ~docv:"RE")

let exclude_entry_re =
  let doc = "Exclude entries matching RE from the merge request's analysis." in
  Arg.(value & opt_all string [] & info [ "exclude-entry-re" ] ~doc ~docv:"RE")

let output =
  let doc = "Output report to $(i,PATH)" in
  Arg.(value & opt string "" & info [ "o"; "output" ] ~doc ~docv:"OUTPUT")

let markdown =
  let doc = "Output in a markdown format" in
  Arg.(value & flag & info [ "markdown" ] ~doc ~docv:"MARKDOWN")

let html =
  let doc = "Output in a html format" in
  Arg.(value & flag & info [ "html" ] ~doc ~docv:"HTML")

let gitlab =
  let doc = "Output in GitLab code quality report" in
  Arg.(value & flag & info [ "gitlab" ] ~doc ~docv:"GITLAB")

let title =
  let doc = "Merge request title" in
  Arg.(value & opt string "" & info [ "t"; "title" ] ~doc ~docv:"TITLE")

module Check = struct
  let doc =
    "Check undocument function between current head and last merge commit."

  let commit =
    let doc = "Base commit to check." in
    Arg.(value & opt string "" & info [ "h"; "hash" ] ~doc ~docv:"COMMIT")

  let clickable =
    let doc = "Create clickable links towards entries on gitlab" in
    Arg.(value & flag & info [ "c"; "clickable" ] ~doc ~docv:"CLICKABLE")

  let check_open =
    let path =
      let doc = "Git project path." in
      Arg.(value & opt string "." & info [ "p"; "path" ] ~doc ~docv:"PATH")
    in
    let term =
      Term.(
        const check $ path $ commit $ exclude_files $ exclude_file_re
        $ exclude_entry_re $ output $ markdown $ html $ gitlab)
    in
    let info = Cmd.info "check" ~version ~doc ~exits:Cmd.Exit.defaults in
    Cmd.v info term

  let check_clone =
    let git =
      let doc = "Git project." in
      Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"GIT")
    in
    let branch =
      let doc = "Git project branch." in
      Arg.(value & opt string "" & info [ "b"; "branch" ] ~doc ~docv:"BRANCH")
    in
    let term =
      Term.(
        const check_clone $ git $ branch $ commit $ exclude_files
        $ exclude_file_re $ exclude_entry_re $ output $ clickable $ markdown
        $ html $ gitlab $ title)
    in
    let info = Cmd.info "check-clone" ~version ~doc ~exits:Cmd.Exit.defaults in
    Cmd.v info term

  let cmds = [ check_open; check_clone ]
end

let cmds =
  let help = Term.(ret (const (`Help (`Pager, None)))) in
  let info = Cmd.info "help" in

  Cmd.group ~default:help info Check.cmds

let () = Stdlib.exit @@ Cmd.eval cmds
