module Git = Ometrics__Git
module Change = Ometrics__Change
open Git
open Ometrics__Monad

let pp_repo fmt (Repo x) = Format.fprintf fmt "Report %s" x

let eq_repo = Alcotest.of_pp pp_repo

let pp_hash fmt (Hash x) = Format.fprintf fmt "Hash %s" x

let eq_hash = Alcotest.of_pp pp_hash

let name_project repo =
  String.split_on_char '/' repo |> fun l ->
  let n = List.length l in
  List.nth l (n - 1)

let open_current_repo () =
  let expected = "ometrics" in
  let (Repo actual) = run_dry @@ open_repository () |> Option.get in
  let actual = name_project actual in
  Alcotest.(check string) "open current repo opens ometrics" expected actual

let open_bad_repo () =
  let err = run_dry @@ open_repository ~path:"this/path/is/invalid" () in
  Alcotest.(check bool) "open bad repo fails" true (err = None)

let open_and_clean () =
  let pwd = ref "" in
  let kont _ = run_string "pwd" >>= fun res -> return (pwd := res) in
  let _ =
    run_dry (open_repository () >>= fun repo -> with_tmp_clone repo kont)
  in
  let actual = Sys.file_exists !pwd in
  Alcotest.(check bool) "with_tmp_clone cleans the repo" false actual

(** Gitlab project for testing purposes

    https://gitlab.com/vch9/ometrics-test *)
let git = "https://gitlab.com/vch9/ometrics-test.git"

let no_merge_commit () =
  let branch = "no-commits" in
  let actual =
    run_dry
      (clone_repository ~branch git >>= fun repo -> find_last_merge_commit repo)
  in
  Alcotest.(check (option eq_hash)) "no_merge_commit fails" None actual

let find_merge_commit () =
  let branch = "merge-commits" in
  let expected = Some (Hash "7b3d5b8c1c054d280637f4e11cd97135577d3fb5") in
  let actual =
    run_dry
      ( clone_repository ~branch git >>= fun repo ->
        let () = Printf.printf "repo: %s" (Git.root_of repo) in
        find_last_merge_commit repo )
  in
  Alcotest.(check (option eq_hash))
    "find_merge_commit should find" expected actual

let get_commits_after () =
  let branch = "merge-commits" in
  let expected =
    Some
      [
        Hash "ca65467249118435c2ecf4d784c121c0456ffbdc";
        Hash "73ef14ae0ef1277df03688f8ce7cb96b9a151e5a";
      ]
  in
  let actual =
    run_dry
      ( clone_repository ~branch git >>= fun repo ->
        find_last_merge_commit repo >>= fun hash -> get_commits_after repo hash
      )
  in
  Alcotest.(check (option (list eq_hash)))
    "get_commits_after gets [foo;bar]" expected actual

let get_changes () =
  let branch = "merge-commits" in
  let expected = Some Change.[ Edition "foo"; Edition "bar" ] in
  let actual =
    run_dry
      ( clone_repository ~branch git >>= fun repo ->
        find_last_merge_commit repo >>= fun hash ->
        Git.get_changes ~since:hash repo )
  in
  Alcotest.(check (option Test_change.eq_changes))
    "get_changes finds edit bar and foo" expected actual

let tests =
  ( "Git",
    Alcotest.
      [
        test_case "open_current_repo" `Quick open_current_repo;
        test_case "open_bad_repo" `Quick open_bad_repo;
        test_case "open_and_clean" `Quick open_and_clean;
        test_case "no_merge_commit" `Quick no_merge_commit;
        test_case "find_merge_commit" `Quick find_merge_commit
        (* test_case "get_commits_after" `Quick get_commits_after;
         * test_case "get_changes" `Quick get_changes; *);
      ] )
