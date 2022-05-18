type kind = Toplevel | ModuleType | Module | Functor | Type | Value

type t = {
  entry_kind : kind;
  entry_name : string;
  entry_line : int;
  entry_file : string;
  entry_description : string;
}

let compare x y =
  compare
    (x.entry_kind, x.entry_name, x.entry_file, y.entry_description)
    (y.entry_kind, y.entry_name, y.entry_file, y.entry_description)

let pp_kind fmt kind =
  let open Format in
  match kind with
  | Toplevel -> pp_print_string fmt "Toplevel"
  | ModuleType -> pp_print_string fmt "ModuleType"
  | Functor -> pp_print_string fmt "Functor"
  | Module -> pp_print_string fmt "Module"
  | Type -> pp_print_string fmt "Type"
  | Value -> pp_print_string fmt "Value"

let pp fmt { entry_kind; entry_name; entry_description; _ } =
  let open Format in
  fprintf fmt "@[<h>%a: %s %s@]" pp_kind entry_kind entry_name entry_description

let pp_markdown ?with_link fmt
    { entry_kind; entry_name; entry_line; entry_file; entry_description } =
  let open Format in
  let name =
    match with_link with
    | None -> entry_name
    | Some prefix ->
        sprintf "[`%s`](%s%s#%d)" entry_name prefix entry_file entry_line
  in
  fprintf fmt "`%a`: %s %s" pp_kind entry_kind name entry_description

let pp_html ?with_link fmt
    { entry_kind; entry_name; entry_line; entry_file; entry_description } =
  let open Format in
  let name =
    match with_link with
    | None -> entry_name
    | Some prefix ->
        sprintf "<a href=\"%s%s#%d\">%s</a>" prefix entry_file entry_line
          entry_name
  in
  fprintf fmt "%a: %s %s" pp_kind entry_kind name entry_description

let base_entry_name entry_name =
  let l = String.split_on_char '.' entry_name in
  match l with
  | [ x ] -> x
  | xs -> (
      let n = List.length xs in
      match List.nth_opt xs (n - 1) with Some x -> x | None -> entry_name)

let is_excluded_entry_re { entry_name; _ } re =
  let x = base_entry_name entry_name in
  Str.string_match (Str.regexp re) x 0

let exclude_entries_re entries re =
  List.filter (fun entry -> not (is_excluded_entry_re entry re)) entries
