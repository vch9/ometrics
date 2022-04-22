We first check with no exclusions
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8
  # `tezt/foo.ml`
  
  - `Value: tezt`
  
  # `src/test/test_main.ml`
  
  - `Value: test_sum`
  
  # `src/main.ml`
  
  - `Value: sum`
  

We exclude tezt/foo.ml
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --exclude-file tezt/foo.ml
  # `src/test/test_main.ml`
  
  - `Value: test_sum`
  
  # `src/main.ml`
  
  - `Value: sum`
  


We exclude tezt/
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --exclude-file tezt/
  # `src/test/test_main.ml`
  
  - `Value: test_sum`
  
  # `src/main.ml`
  
  - `Value: sum`
  
We exclude test_ with regexp
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --exclude-file-re "test_.*"
  # `tezt/foo.ml`
  
  - `Value: tezt`
  
  # `src/main.ml`
  
  - `Value: sum`
  
We now exclude entries with regexp
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --exclude-file-re "test_.*" --exclude-entry-re "sum"
  # `tezt/foo.ml`
  
  - `Value: tezt`
  
