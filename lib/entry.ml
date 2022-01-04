type kind = (* ModuleType | Module | Functor | *) Type | Value

type t = {
  entry_kind : kind;
  entry_name : string;
  entry_documented : bool;
  entry_line : int;
  entry_file : string;
}

let compare x y =
  let entry_kind = compare x.entry_kind y.entry_kind in
  if entry_kind <> 0 then entry_kind else compare x.entry_name y.entry_name

type entry = t

let is_documented { entry_documented; _ } = entry_documented
let is_not_documented e = not (is_documented e)

let pp ?(with_mark = false) ?with_link fmt
    { entry_kind; entry_name; entry_documented; entry_line; entry_file } =
  let open Format in
  let pp_kind fmt = function
    (* | ModuleType -> pp_print_string fmt "ModuleType"
     * | Functor -> pp_print_string fmt "Functor"
     * | Module -> pp_print_string fmt "Module" *)
    | Type -> pp_print_string fmt "Type"
    | Value -> pp_print_string fmt "Value"
  in

  let name =
    match with_link with
    | None -> entry_name
    | Some prefix ->
        sprintf "[%s](%s%s#%d)" entry_name prefix entry_file entry_line
  in

  fprintf fmt "@[<h>%a: %s%a@]" pp_kind entry_kind name
    (fun fmt _ -> if with_mark && entry_documented then fprintf fmt " ✓")
    ()
