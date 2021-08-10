open Change

type hash

val hash_from_string : string -> hash

type repository

val open_repository : ?path:string -> unit -> repository
(** [open_repository ()] returns a [repository] if the current working
    directory is inside a git repository.

    Alternatively, the [path] optional argument can be used to change
    the reference directory.  Under the hood, it is used as an
    argument for the [-C] command-line argument of [git].

    If the current repository or [path] is not a git repository,
    raises [Not_a_git_repository]. *)

val root_of : repository -> string
(** [root_of r] returns the absolute path of the root of [r]. *)

val with_tmp_clone : repository -> ?hash:hash -> (repository -> 'a) -> 'a
(** [with_tmp_clone r k] clones the repository [r] in a temporary
    directory, and calls the continuation [k] with the resulting repository.

    The temporary clone is deleted once the continuation
    terminates. *)

val find_last_merge_commit : repository -> hash
(** [find_last_merge_commit r] returns the most recent merge commit

     TODO: failure case *)

val get_commits_after : repository -> hash -> hash list
(** [get_commits_after r h] returns the list of hashes of commits that have been
    applied of top of [h] up until the head of [r]. *)

val changes_of : repository -> hash -> changes

val get_changes : repository -> since:hash -> changes
