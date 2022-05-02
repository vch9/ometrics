Markdown without clickable links
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --markdown
  <details><summary markdown="span">⚠ There are changes which are not documented:</summary>
  
    * <details><summary markdown="span">tezt/foo.ml</summary>
  
      * `Value`: tezt
  
    * <details><summary markdown="span">src/test/test_main.ml</summary>
  
      * `Value`: test_sum
  
    * <details><summary markdown="span">src/main.ml</summary>
  
      * `Value`: sum
  
  </details>

Markdown with clickables links
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --clickable --markdown
  <details><summary markdown="span">⚠ There are changes which are not documented:</summary>
  
    * <details><summary markdown="span">tezt/foo.ml</summary>
  
      * `Value`: [`tezt`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/tezt/foo.ml#1)
  
    * <details><summary markdown="span">src/test/test_main.ml</summary>
  
      * `Value`: [`test_sum`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/src/test/test_main.ml#1)
  
    * <details><summary markdown="span">src/main.ml</summary>
  
      * `Value`: [`sum`](https://gitlab.com/vch9/ometrics-test/-/tree/4e083e743d9a4500958635867779fa9bcacbacda/src/main.ml#1)
  
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
    <li>Value: tezt</li>
  </ol>
  <h3>src/test/test_main.ml</h3>
  <ol>
    <li>Value: test_sum</li>
  </ol>
  <h3>src/main.ml</h3>
  <ol>
    <li>Value: sum</li>
  </ol>
  <body>
  </html>

GitLab code quality:
  $ ometrics check-clone https://gitlab.com/vch9/ometrics-test.git --branch src-and-test --hash f9b705455789adea9258379e8e3113a61eda0ec8 --gitlab
  [
    {
      "description" : "'sum' is not documented.",
      "fingerprint" : "d678f950b4b5b189a89636053df4fa16f5fb3a22266a28c0687819d574c33e22",
      "severity": "minor",
      "location": {
        "path": "src/main.ml",
        "lines": {
          "begin": 1
        }
      }
    },
  
    {
      "description" : "'test_sum' is not documented.",
      "fingerprint" : "846135fc872acb14faa36aca7172e782215fe0c69a9aa956e822ee6f33e2c42b",
      "severity": "minor",
      "location": {
        "path": "src/test/test_main.ml",
        "lines": {
          "begin": 1
        }
      }
    },
  
    {
      "description" : "'tezt' is not documented.",
      "fingerprint" : "3710c4c6e29f28a3369c496b14a288924d9a8490a40bef851528ae20e3d313f4",
      "severity": "minor",
      "location": {
        "path": "tezt/foo.ml",
        "lines": {
          "begin": 1
        }
      }
    }
  ]
