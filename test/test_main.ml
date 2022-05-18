let () =
  Alcotest.run "ometrics"
    [
      Test_change.tests;
      Test_documentation.tests;
      Test_git.tests;
      Test_report.tests;
    ]
