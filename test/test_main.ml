open Ometrics

let test_prefix_good () =
  let expected =
    Some
      "https://gitlab.com/nomadic-labs/tezos/-/tree/4558f9fc10bc522a4c99bb54720a026a573c0aa5/"
  in
  let actual =
    Main.link_prefix "https://gitlab.com/nomadic-labs/tezos.git"
      "4558f9fc10bc522a4c99bb54720a026a573c0aa5"
  in
  Alcotest.(check (option string)) "create a valid prefix" expected actual

let tests =
  ("Main", Alcotest.[ test_case "test_prefix_good" `Quick test_prefix_good ])

let () =
  Alcotest.run "ometrics"
    [
      tests;
      Test_change.tests;
      Test_entry.tests;
      Test_git.tests;
      Test_implementation.tests;
      Test_interface.tests;
      Test_misc.tests;
      Test_toplevel.tests;
    ]
