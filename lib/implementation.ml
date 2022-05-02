let fully_qualified_name = Misc.fully_qualified_name
let is_documented = Misc.is_documented
let pattern_idents = Misc.pattern_idents
let line = Misc.line
let entries_of_type_declaration = Misc.entries_of_type_declaration

open Ppxlib

let entries_of_binding ~path ns binding : Entry.t list =
  let entry_documented = is_documented binding.pvb_attributes in
  List.map
    (fun ident ->
      Entry.
        {
          entry_name = fully_qualified_name ns ident;
          entry_kind = Value;
          entry_documented;
          entry_line = line binding.pvb_loc;
          entry_file = path;
        })
    (pattern_idents binding.pvb_pat)

let rec entries_of_module_binding ~path ns binding =
  Option.fold binding.pmb_name.txt ~none:[] ~some:(fun ident ->
      let entry_documented = is_documented binding.pmb_attributes in
      Entry.
        {
          entry_name = fully_qualified_name ns ident;
          entry_kind =
            (match binding.pmb_expr.pmod_desc with
            | Pmod_functor (_, _) -> Functor
            | _ -> Module);
          entry_documented;
          entry_line = line binding.pmb_loc;
          entry_file = path;
        }
      :: entries_of_module_expr ~path ns ident binding.pmb_expr)

and entries_of_module_expr ~path ns ident expr =
  match expr.pmod_desc with
  | Pmod_structure structure ->
      List.concat_map (entries_of_struct_item ~path (ident :: ns)) structure
  | Pmod_functor (_, expr) -> entries_of_module_expr ~path ns ident expr
  | _ -> []

and entries_of_struct_item ~path ns { pstr_desc; pstr_loc = _ } =
  match pstr_desc with
  | Pstr_value (_, bindings) ->
      List.concat_map (entries_of_binding ~path ns) bindings
  | Pstr_type (_, decls) ->
      List.concat_map (entries_of_type_declaration ~path ns) decls
  | Pstr_module binding -> entries_of_module_binding ~path ns binding
  | Pstr_recmodule bindings ->
      List.concat_map (entries_of_module_binding ~path ns) bindings
  | _ -> []

let to_entries ~path str = List.concat_map (entries_of_struct_item ~path []) str
