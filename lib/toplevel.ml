let to_entries target =
  let parse lexbuf =
    (* Suppose that [target] is either a .ml or .mli file *)
    if Filename.extension target = ".ml" then
      `Implementation (Ppxlib.Parse.implementation lexbuf)
    else `Interface (Ppxlib.Parse.interface lexbuf)
  in
  let in_channel = open_in target in
  let lexbuf = Lexing.from_channel in_channel in
  let toplevel = parse lexbuf in
  match toplevel with
  | `Implementation str -> Implementation.to_entries ~path:target str
  | `Interface sigs -> Interface.to_entries ~path:target sigs
