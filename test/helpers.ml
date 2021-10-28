let qcheck_eq ?pp ?cmp ?eq expected actual =
  let pass =
    match (eq, cmp) with
    | Some eq, _ -> eq expected actual
    | None, Some cmp -> cmp expected actual = 0
    | None, None -> Stdlib.compare expected actual = 0
  in
  if pass then true
  else
    match pp with
    | None ->
        QCheck.Test.fail_reportf
          "@[<h 0>Values are not equal, but no pretty printer was provided.@]"
    | Some pp ->
        QCheck.Test.fail_reportf
          "@[<v 2>Equality check failed!@,expected:@,%a@,actual:@,%a@]" pp
          expected pp actual

let qcheck_eq' ?pp ?cmp ?eq ~expected ~actual () =
  qcheck_eq ?pp ?cmp ?eq expected actual
