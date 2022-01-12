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

let pp_kind fmt kind =
  let open Format in
  match kind with
  (* | ModuleType -> pp_print_string fmt "ModuleType"
   * | Functor -> pp_print_string fmt "Functor"
   * | Module -> pp_print_string fmt "Module" *)
  | Type -> pp_print_string fmt "Type"
  | Value -> pp_print_string fmt "Value"

let pp ?(with_mark = false) fmt { entry_kind; entry_name; entry_documented; _ }
    =
  let open Format in
  fprintf fmt "@[<h>%a: %s%a@]" pp_kind entry_kind entry_name
    (fun fmt _ -> if with_mark && entry_documented then fprintf fmt " ✓")
    ()

let pp_markdown ?with_link fmt
    { entry_kind; entry_name; entry_line; entry_file; _ } =
  let open Format in
  let name =
    match with_link with
    | None -> entry_name
    | Some prefix ->
        sprintf "[`%s`](%s%s#%d)" entry_name prefix entry_file entry_line
  in
  fprintf fmt "`%a`: %s" pp_kind entry_kind name

let pp_html ?with_link fmt { entry_kind; entry_name; entry_line; entry_file; _ }
    =
  let open Format in
  let name =
    match with_link with
    | None -> entry_name
    | Some prefix ->
        sprintf "<a href=\"%s%s#%d\">%s</a>" prefix entry_file entry_line
          entry_name
  in
  fprintf fmt "%a: %s" pp_kind entry_kind name

let base_entry_name entry_name =
  if String.contains entry_name '.' then
    let x = Filename.extension entry_name in
    let n = String.length x in
    String.sub x 1 (n - 1)
  else entry_name

let is_excluded_entry_re { entry_name; _ } re =
  let x = base_entry_name entry_name in
  Str.string_match (Str.regexp re) x 0

let exclude_entries_re entries re =
  List.filter (fun entry -> not (is_excluded_entry_re entry re)) entries
