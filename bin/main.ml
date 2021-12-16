open Ometrics.Main
open Cmdliner

let name = "ometrics"
let version = "dev"

let exclude_files =
  let doc =
    "Exclude files at $(i,PATH) from the merge request's analysis.\n\
    \    If $(i,PATH) ends with a path seperator, it is treated as a directory \
     name."
  in
  Arg.(value & opt_all string [] & info [ "e"; "exclude" ] ~doc ~docv:"PATH")

let exclude_re =
  let doc = "Exclude files matching RE from the merge request's analysis." in
  Arg.(value & opt string "" & info [ "e-re"; "exclude-re" ] ~doc ~docv:"RE")

module Check = struct
  let doc =
    "Check undocument function between current head and last merge commit."

  let commit =
    let doc = "Base commit to check." in
    Arg.(value & opt string "" & info [ "h"; "hash" ] ~doc ~docv:"COMMIT")

  let check_open =
    let exits = Term.default_exits in
    let path =
      let doc = "Git project path." in
      Arg.(value & opt string "." & info [ "p"; "path" ] ~doc ~docv:"PATH")
    in
    ( Term.(const check $ path $ commit $ exclude_files $ exclude_re),
      Term.info "check" ~version ~doc ~exits )

  let check_clone =
    let exits = Term.default_exits in
    let git =
      let doc = "Git project." in
      Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"GIT")
    in
    let branch =
      let doc = "Git project branch." in
      Arg.(value & opt string "" & info [ "b"; "branch" ] ~doc ~docv:"BRANCH")
    in
    ( Term.(
        const check_clone $ git $ branch $ commit $ exclude_files $ exclude_re),
      Term.info "check-clone" ~version ~doc ~exits )

  let cmds = [ check_open; check_clone ]
end

let default =
  let exits = Term.default_exits in
  (Term.(ret (const (`Help (`Pager, None)))), Term.info name ~version ~exits)

let cmds = Check.cmds
let () = Term.(exit @@ eval_choice default cmds)
