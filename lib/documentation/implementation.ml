let fully_qualified_name = Misc.fully_qualified_name
let is_documented = Misc.is_documented
let pattern_idents = Misc.pattern_idents
let line = Misc.line
let entries_of_type_declaration = Misc.entries_of_type_declaration
let mk_entry = Misc.mk_entry

open Ppxlib
open Analysis

let entries_of_binding ~path ns binding : Entry.t option list =
  List.map
    (fun ident ->
      mk_entry
        (fully_qualified_name ns ident)
        Value binding.pvb_loc path
        (is_documented binding.pvb_attributes))
    (pattern_idents binding.pvb_pat)

let rec entries_of_module_binding ~path ns binding =
  Option.fold binding.pmb_name.txt ~none:[] ~some:(fun ident ->
      mk_entry
        (fully_qualified_name ns ident)
        (match binding.pmb_expr.pmod_desc with
        | Pmod_functor (_, _) -> Functor
        | _ -> Module)
        binding.pmb_loc path
        (is_documented binding.pmb_attributes)
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
  | Pstr_modtype decl ->
      Interface.entries_of_module_type_declaration ~path ns decl
  | _ -> []

let toplevel ~path strs =
  match strs with
  | { pstr_desc = Pstr_attribute attribute; _ } :: _ -> (
      (* I'm not 100% sure about this method to get the toplevel module name,
         I'd rather have an undetected undocumented entry than an exception. *)
      try
        let entry_name =
          Filename.basename path |> Filename.chop_extension
          |> String.capitalize_ascii
        in

        mk_entry entry_name Toplevel Location.none path
          (attribute.attr_name.txt = "ocaml.text")
      with _ -> None)
  | _ -> None

let to_entries ~path str =
  toplevel ~path str :: List.concat_map (entries_of_struct_item ~path []) str
  |> List.filter_map (fun x -> x)
