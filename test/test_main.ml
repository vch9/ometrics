let () =
  Alcotest.run "ometrics"
    [
      Test_change.tests;
      Test_entry.tests;
      Test_git.tests;
      Test_implementation.tests;
      Test_interface.tests;
      Test_misc.tests;
      Test_toplevel.tests;
      Test_report.tests;
    ]
