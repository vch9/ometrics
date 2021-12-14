type kind = (* ModuleType | Module | Functor | *) Type | Value
type t = { entry_kind : kind; entry_name : string; entry_documented : bool }
type entry = t

let is_documented { entry_documented; _ } = entry_documented
let is_not_documented e = not (is_documented e)

let pp ?(with_mark = false) fmt { entry_kind; entry_name; entry_documented } =
  let open Format in
  let pp_kind fmt = function
    (* | ModuleType -> pp_print_string fmt "ModuleType"
     * | Functor -> pp_print_string fmt "Functor"
     * | Module -> pp_print_string fmt "Module" *)
    | Type -> pp_print_string fmt "Type"
    | Value -> pp_print_string fmt "Value"
  in

  Format.fprintf fmt "@[<h>%a: %s%a@]" pp_kind entry_kind entry_name
    (fun fmt _ -> if with_mark && entry_documented then fprintf fmt " ✓")
    ()
