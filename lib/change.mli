type t =
  | Addition of string
  | Deletion of string
  | Edition of string
  | Renaming of string * string

type change = t

type changes = change list

val change_from_string : string -> change

val is_ml_change : change -> bool

val files_to_analyze : changes -> string list * string list

val merge_changes : changes -> changes -> changes
