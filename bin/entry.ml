type kind = ModuleType | Module | Functor | Type | Value

type t = { entry_kind : kind; entry_name : string; entry_documented : bool }

type entry = t

let pp_print_mark fmt = function
  | true -> Format.pp_print_string fmt "✓"
  | false -> ()

let pp fmt { entry_kind; entry_name; entry_documented } =
  let open Format in
  let pp_kind fmt = function
    | ModuleType -> pp_print_string fmt "ModuleType"
    | Functor -> pp_print_string fmt "Functor"
    | Module -> pp_print_string fmt "Module"
    | Type -> pp_print_string fmt "Type"
    | Value -> pp_print_string fmt "Value"
  in

  Format.fprintf fmt "@[<h>%a: %s%s%a@]" pp_kind entry_kind entry_name
    (if entry_documented then " " else "")
    pp_print_mark entry_documented
