(** This file describes the error monad used in _Ometrics_ *)

type location = string
and msg = string

type error = location * msg
type 'a mresult = ('a, error list) result

val return : 'a -> 'a mresult
val fail : error -> 'a mresult

val run : 'a mresult -> unit
(** Run is called to extract and print errors in the monad *)

val ( >>= ) : 'a mresult -> ('a -> 'b mresult) -> 'b mresult

val ( >>? ) : 'a mresult -> error -> 'a mresult
(** Optionally adds an error to the [mresult] *)

(**/**)

val run_dry : 'a mresult -> 'a option
