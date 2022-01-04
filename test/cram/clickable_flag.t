We first check with no flag
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8
  # `tezt/foo.ml`
  
  - `Value: Foo.tezt`
  
  # `src/test/test_main.ml`
  
  - `Value: Test_main.test_sum`
  
  # `src/main.ml`
  
  - `Value: Main.sum`
  


We then check with the clickable flag
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --clickable
  # `tezt/foo.ml`
  
  - `Value: [Foo.tezt](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/tezt/foo.ml#1)`
  
  # `src/test/test_main.ml`
  
  - `Value: [Test_main.test_sum](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/src/test/test_main.ml#1)`
  
  # `src/main.ml`
  
  - `Value: [Main.sum](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/src/main.ml#1)`
  
