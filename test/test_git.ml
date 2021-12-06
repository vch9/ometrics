module Git = Ometrics__Git
open Git

let pp_repo fmt (Repo x) = Format.fprintf fmt "Report %s" x

let eq_repo = Alcotest.of_pp pp_repo

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
  let f () = open_repository ~path:"this/path/is/invalid/" () |> ignore in
  Alcotest.check_raises "open bad repo fails"
    (BadStatus
       "\"git -C this/path/is/invalid/ rev-parse --show-toplevel\" exited with \
        status: WEXITED 128")
    f

let open_and_clean () =
  let repo = open_repository () in
  let pwd = ref "" in
  let kont _ = pwd := run_string "pwd" in
  let _ = with_tmp_clone repo kont in
  let actual = Sys.file_exists !pwd in
  Alcotest.(check bool) "with_tmp_clone cleans the repo" false actual

let tests =
  ( "Git",
    Alcotest.
      [
        test_case "open_current_repo" `Quick open_current_repo;
        test_case "open_bad_repo" `Quick open_bad_repo;
        test_case "open_and_clean" `Quick open_and_clean;
      ] )
