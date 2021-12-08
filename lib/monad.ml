type location = string

and msg = string

type error = location * msg

type 'a mresult = ('a, error list) result

let return x = Ok x

let fail e = Error [ e ]

let pp_error fmt (loc, msg) = Format.fprintf fmt "\t%s:\n\t\t%s\n" loc msg

let pp_trace fmt trace =
  let open Format in
  let () = fprintf fmt "\x1b[31m" in
  let () = pp_print_string fmt "Error:" in
  let () = fprintf fmt "\x1b[0m" in
  pp_print_list pp_error fmt trace

let run (v : 'a mresult) =
  match v with
  | Ok _ -> ()
  | Error trace ->
      let trace = List.rev trace in
      let () = Printf.eprintf "%s" @@ Format.asprintf "%a" pp_trace trace in
      exit 1

let ( >>= ) v f = match v with Ok x -> f x | Error trace -> Error trace

let ( >>? ) (v : 'a mresult) (err : error) =
  match v with Error trace -> Error (err :: trace) | x -> x

(** Used for internal testing *)
let run_dry v = match v with Ok x -> Some x | Error _ -> None
