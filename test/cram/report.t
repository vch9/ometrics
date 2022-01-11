Markdown without clickable links
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --markdown
  <details><summary markdown="span">⚠ There are changes which are not documented:</summary>
  
    * <details><summary markdown="span">tezt/foo.ml</summary>
  
      * `Value`: Foo.tezt
  
    * <details><summary markdown="span">src/test/test_main.ml</summary>
  
      * `Value`: Test_main.test_sum
  
    * <details><summary markdown="span">src/main.ml</summary>
  
      * `Value`: Main.sum
  
  </details>

Markdown with clickables links
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --clickable --markdown
  <details><summary markdown="span">⚠ There are changes which are not documented:</summary>
  
    * <details><summary markdown="span">tezt/foo.ml</summary>
  
      * `Value`: [`Foo.tezt`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/tezt/foo.ml#1)
  
    * <details><summary markdown="span">src/test/test_main.ml</summary>
  
      * `Value`: [`Test_main.test_sum`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/src/test/test_main.ml#1)
  
    * <details><summary markdown="span">src/main.ml</summary>
  
      * `Value`: [`Main.sum`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/src/main.ml#1)
  
  </details>

HTML
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --html
  <html>
  <head>
  <title>4e083e743d9a4500958635867779fa9bcacbacda</title>
  </head>
  <body>
    <h2>Undocumented entries introduced since 4e083e743d9a4500958635867779fa9bcacbacda</h2>
  <h3>tezt/foo.ml</h3>
  <ol>
    <li>Value: Foo.tezt</li>
  </ol>
  <h3>src/test/test_main.ml</h3>
  <ol>
    <li>Value: Test_main.test_sum</li>
  </ol>
  <h3>src/main.ml</h3>
  <ol>
    <li>Value: Main.sum</li>
  </ol>
  <body>
  </html>

GitLab code quality:
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --gitlab
  [
    {
      "description" : "'tezt' is not documented.",
      "fingerprint" : "",
      "severity": "minor",
      "location": {
        "path": "tezt/foo.ml",
        "lines": {
          "begin": 1
        }
      }
    },
  
    {
      "description" : "'test_sum' is not documented.",
      "fingerprint" : "",
      "severity": "minor",
      "location": {
        "path": "src/test/test_main.ml",
        "lines": {
          "begin": 1
        }
      }
    },
  
    {
      "description" : "'sum' is not documented.",
      "fingerprint" : "",
      "severity": "minor",
      "location": {
        "path": "src/main.ml",
        "lines": {
          "begin": 1
        }
      }
    },
  ]
