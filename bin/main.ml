open Ometrics.Main
open Cmdliner

let name = "ometrics"

let version = "dev"

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
    ( Term.(term_result (const check $ path $ commit)),
      Term.info "check" ~version ~doc ~exits )

  let check_clone =
    let exits = Term.default_exits in
    let git =
      let doc = "Git project." in
      Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"GIT")
    in
    ( Term.(term_result (const check $ git $ commit)),
      Term.info "check" ~version ~doc ~exits )

  let cmds = [ check_open; check_clone ]
end

let default =
  let exits = Term.default_exits in
  (Term.(ret (const (`Help (`Pager, None)))), Term.info name ~version ~exits)

let cmds = Check.cmds

let () = Term.(exit @@ eval_choice default cmds)
