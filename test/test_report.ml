open Ometrics

let test_prefix_good () =
  let expected =
    Some
      "https://gitlab.com/nomadic-labs/tezos/-/tree/4558f9fc10bc522a4c99bb54720a026a573c0aa5/"
  in
  let actual =
    Report.link_prefix "https://gitlab.com/nomadic-labs/tezos.git"
      "4558f9fc10bc522a4c99bb54720a026a573c0aa5"
  in
  Alcotest.(check (option string)) "create a valid prefix" expected actual

let tests =
  ("Main", Alcotest.[ test_case "test_prefix_good" `Quick test_prefix_good ])
