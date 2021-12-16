type t =
  | Addition of string
  | Deletion of string
  | Edition of string
  | Renaming of string * string

let pp fmt = function
  | Addition s -> Format.fprintf fmt "Addition %s" s
  | Deletion s -> Format.fprintf fmt "Deletion %s" s
  | Edition s -> Format.fprintf fmt "Edition %s" s
  | Renaming (s, s') -> Format.fprintf fmt "Renaming %s -> %s" s s'

let eq x y =
  match (x, y) with
  | Addition s, Addition s' -> s = s'
  | Deletion s, Deletion s' -> s = s'
  | Edition s, Edition s' -> s = s'
  | Renaming (s1, s1'), Renaming (s2, s2') -> s1 = s2 && s1' = s2'
  | _ -> false

type change = t
type changes = change list

let name_ext path =
  let base = Filename.basename path in
  let ext = Filename.extension base in
  let file = Filename.remove_extension base in
  (file, ext)

let is_ml_file path =
  let _, ext = name_ext path in
  ext = ".ml" || ext = ".mli"

let is_ml_change : change -> bool = function
  | Addition path | Edition path -> is_ml_file path
  | Renaming (before, after) -> is_ml_file before && is_ml_file after
  | _ -> false

let is_excluded_files path : string -> bool =
 fun change ->
  let is_dir = String.get path (String.length path - 1) = '/' in
  if is_dir then
    let dir = String.sub path 0 (String.length path - 1) in
    String.length path >= String.length dir
    && dir = String.sub change 0 (String.length dir)
  else path = change

let is_excluded_re re : string -> bool =
 fun change ->
  if re = "" then false
  else
    let file, _ = name_ext change in
    let rxp = Str.regexp re in
    Str.string_match rxp file 0

let is_excluded exclude_files exclude_re : change -> bool =
 fun ch ->
  let is_excluded_files ch =
    List.exists (fun ef -> is_excluded_files ef ch) exclude_files
  in
  let is_excluded_re = is_excluded_re exclude_re in
  match ch with
  | Addition path | Edition path | Renaming (_, path) ->
      is_excluded_files path || is_excluded_re path
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

(** [uniqueness l] removes duplicates from [l]. *)
let rec uniqueness = function
  | x :: xs -> if List.mem x xs then uniqueness xs else x :: uniqueness xs
  | [] -> []

let merge_changes prev next = List.fold_left add_change prev next |> uniqueness

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
  let rxp = Str.regexp "^R[0-9]*\t*\\(.+\\)\t+\\(.+\\)$" in
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
              | None -> raise (Invalid_argument str))))
