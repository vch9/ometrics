module Git = Ometrics__Git

let () =
  let open Git in
  let _ = open_repository in
  ()

let tests = ("Git", [])
