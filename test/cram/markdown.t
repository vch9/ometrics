We first test without clickable links
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --markdown
  <details><summary markdown="span">⚠ There are changes which are not documented:</summary>
  
    * <details><summary markdown="span">tezt/foo.ml</summary>
  
      * `Value`: Foo.tezt
  
    * <details><summary markdown="span">src/test/test_main.ml</summary>
  
      * `Value`: Test_main.test_sum
  
    * <details><summary markdown="span">src/main.ml</summary>
  
      * `Value`: Main.sum
  
  </details>

We then check with clickable links
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --clickable --markdown
  <details><summary markdown="span">⚠ There are changes which are not documented:</summary>
  
    * <details><summary markdown="span">tezt/foo.ml</summary>
  
      * `Value`: [`Foo.tezt`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/tezt/foo.ml#1)
  
    * <details><summary markdown="span">src/test/test_main.ml</summary>
  
      * `Value`: [`Test_main.test_sum`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/src/test/test_main.ml#1)
  
    * <details><summary markdown="span">src/main.ml</summary>
  
      * `Value`: [`Main.sum`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/src/main.ml#1)
  
  </details>
