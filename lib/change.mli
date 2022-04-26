type t =
  | Addition of string
  | Deletion of string
  | Edition of string
  | Renaming of string * string

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool

type change = t
type changes = change list

val change_from_string : string -> change option
val is_ml_change : change -> bool
val is_ml_file : string -> bool
val is_excluded : string list -> string list -> change -> bool

val files_to_analyze : changes -> string list * string list
(** [files_to_analyze changes] computes the list of files to analyze
    before [changes] and after

    e.g.:
    {[ files_to_analyse [ Addition "foo" ] = ([], ["foo"]) ]} *)

val merge_changes : changes -> changes -> changes
