type t =
  | Addition of string
  | Deletion of string
  | Edition of string
  | Renaming of string * string

type change = t

type changes = change list

let is_ml_change : change -> bool =
  let name_ext path =
    let base = Filename.basename path in
    let ext = Filename.extension base in
    let file = Filename.remove_extension base in
    (file, ext)
  in

  let is_ml_file path =
    let _, ext = name_ext path in
    ext = ".ml" || ext = ".mli"
  in

  function
  | Addition path | Edition path -> is_ml_file path
  | Renaming (before, after) -> is_ml_file before && is_ml_file after
  | _ -> false

let rec add_deletion path = function
  | (Addition path' | Renaming (_, path') | Edition path') :: rst
    when path = path' ->
      rst
  | x :: rst -> x :: add_deletion path rst
  | [] -> [ Deletion path ]

let rec add_renaming before after = function
  | Addition path :: rst when before = path -> Addition after :: rst
  | Edition path :: rst when path = before -> Renaming (before, after) :: rst
  | Renaming (origin, path) :: rst when path = before ->
      Renaming (origin, after) :: rst
  | x :: rst -> x :: add_renaming before after rst
  | [] -> [ Renaming (before, after) ]

let rec add_edition path = function
  | ((Addition path' | Renaming (_, path') | Edition path') as ch) :: rst
    when path = path' ->
      ch :: rst
  | x :: rst -> x :: add_edition path rst
  | [] -> [ Edition path ]

let add_change l = function
  | Deletion path -> add_deletion path l
  | Edition path -> add_edition path l
  | Renaming (before, after) -> add_renaming before after l
  | ch -> ch :: l

let merge_changes = List.fold_left add_change

let files_to_analyze =
  let rec aux accb acca = function
    | Deletion _ :: rst -> aux accb acca rst
    | Addition path :: rst -> aux accb (path :: acca) rst
    | Edition path :: rst -> aux (path :: accb) (path :: acca) rst
    | Renaming (before, after) :: rst ->
        aux (before :: accb) (after :: acca) rst
    | [] -> (List.sort String.compare accb, List.sort String.compare acca)
  in
  aux [] []

let addition_from_string str =
  let rxp = Str.regexp "^[AC]\t*\\(.*\\)$" in
  if Str.string_match rxp str 0 then Some (Addition (Str.matched_group 1 str))
  else None

let deletion_from_string str =
  let rxp = Str.regexp "^D\t*\\(.*\\)$" in
  if Str.string_match rxp str 0 then Some (Deletion (Str.matched_group 1 str))
  else None

let edition_from_string str =
  let rxp = Str.regexp "^M\t*\\(.*\\)$" in
  if Str.string_match rxp str 0 then Some (Edition (Str.matched_group 1 str))
  else None

let renaming_from_string str =
  let rxp = Str.regexp "^R[0-9]*\t*\\(.*\\)\t*\\(.*\\)$" in
  if Str.string_match rxp str 0 then
    Some (Renaming (Str.matched_group 1 str, Str.matched_group 2 str))
  else None

let change_from_string str =
  match addition_from_string str with
  | Some c -> c
  | None -> (
      match deletion_from_string str with
      | Some c -> c
      | None -> (
          match edition_from_string str with
          | Some c -> c
          | None -> (
              match renaming_from_string str with
              | Some c -> c
              | None -> assert false)))
