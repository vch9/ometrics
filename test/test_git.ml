module Git = Ometrics__Git
module Change = Ometrics__Change
open Git

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
  let (Repo actual) = open_repository () in
  let actual = name_project actual in
  Alcotest.(check string) "open current repo opens ometrics" expected actual

let open_bad_repo () =
  try
    let _ = open_repository ~path:"this/path/is/invalid" () in
    Alcotest.fail "open bad repo shoud faild"
  with _ -> ()

let open_and_clean () =
  let pwd = ref "" in
  let cont _ =
    let res = run_string "pwd" in
    pwd := res
  in
  let _ =
    let repo = open_repository () in
    with_tmp_clone repo cont
  in
  let actual = Sys.file_exists !pwd in
  Alcotest.(check bool) "with_tmp_clone cleans the repo" false actual

(** Gitlab project for testing purposes

    https://gitlab.com/vch9/ometrics-test *)
let git = "https://gitlab.com/vch9/ometrics-test.git"

let no_merge_commit () =
  let branch = "no-commits" in
  try
    let repo = clone_repository ~branch git in
    let _ = find_last_merge_commit repo in
    Alcotest.fail "should fail with no merge commit"
  with _ -> ()

let find_merge_commit () =
  let branch = "merge-commits" in
  let expected = Hash "7b3d5b8c1c054d280637f4e11cd97135577d3fb5" in
  let actual =
    let repo = clone_repository ~branch git in
    find_last_merge_commit repo
  in
  Alcotest.(check eq_hash) "find_merge_commit should find" expected actual

let get_commits_after () =
  let branch = "merge-commits" in
  let expected =
    [
      Hash "ca65467249118435c2ecf4d784c121c0456ffbdc";
      Hash "73ef14ae0ef1277df03688f8ce7cb96b9a151e5a";
    ]
  in
  let actual =
    let repo = clone_repository ~branch git in
    let hash = find_last_merge_commit repo in
    get_commits_after repo hash
  in
  Alcotest.(check (list eq_hash))
    "get_commits_after gets [foo;bar]" expected actual

let get_changes () =
  let branch = "merge-commits" in
  let expected = Change.[ Edition "foo"; Edition "bar" ] in
  let actual =
    let repo = clone_repository ~branch git in
    let hash = find_last_merge_commit repo in
    Git.get_changes ~since:hash repo
  in
  Alcotest.(check Test_change.eq_changes)
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
