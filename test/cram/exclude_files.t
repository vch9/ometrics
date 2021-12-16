We first check with no exclusions
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8
  # `tezt/foo.ml`
  
  - `Value: Foo.tezt`
  
  # `src/test/test_main.ml`
  
  - `Value: Test_main.test_sum`
  
  # `src/main.ml`
  
  - `Value: Main.sum`
  

We exclude tezt/foo.ml
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --exclude tezt/foo.ml
  # `src/test/test_main.ml`
  
  - `Value: Test_main.test_sum`
  
  # `src/main.ml`
  
  - `Value: Main.sum`
  


We exclude tezt/
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --exclude tezt/
  # `src/test/test_main.ml`
  
  - `Value: Test_main.test_sum`
  
  # `src/main.ml`
  
  - `Value: Main.sum`
  
We exclude test_ with regexp
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --exclude-re "test_.*"
  # `tezt/foo.ml`
  
  - `Value: Foo.tezt`
  
  # `src/main.ml`
  
  - `Value: Main.sum`
  
