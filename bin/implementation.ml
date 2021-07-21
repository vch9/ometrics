open Typedtree
open Entry
open Misc

let entries_of_value_binding ns comments binding =
  let entry_documented = is_documented comments binding.vb_loc in
  List.map
    (fun ident ->
      {
        entry_name = String.concat "." @@ List.rev (ident :: ns);
        entry_kind = Value;
        entry_documented;
      })
    (pattern_idents binding.vb_pat)

let entries_of_type_declaration ns comments decl =
  [
    {
      entry_name = fully_qualified_name ns decl.typ_id;
      entry_kind = Type;
      entry_documented = is_documented comments decl.typ_loc;
    };
  ]

let rec entries_of_module_binding ns comments binding =
  Option.fold binding.mb_id ~none:[] ~some:(fun ident ->
      {
        entry_name = fully_qualified_name ns ident;
        entry_kind =
          (match binding.mb_expr.mod_desc with
          | Tmod_functor (_, _) -> Functor
          | _ -> Module);
        entry_documented = is_documented comments binding.mb_loc;
      }
      :: entries_of_module_expr ns comments ident binding.mb_expr)

and entries_of_module_expr ns comments ident expr =
  match expr.mod_desc with
  | Tmod_structure structure ->
      List.concat_map
        (entries_of_struct_item (Ident.name ident :: ns) comments)
        structure.str_items
  | Tmod_functor (_, expr) -> entries_of_module_expr ns comments ident expr
  | _ -> []

and entries_of_struct_item ns comments str_item : entry list =
  match str_item.str_desc with
  | Tstr_value (_, bindings) ->
      List.concat_map (entries_of_value_binding ns comments) bindings
  | Tstr_type (_, decls) ->
      List.concat_map (entries_of_type_declaration ns comments) decls
  | Tstr_module binding -> entries_of_module_binding ns comments binding
  | Tstr_recmodule bindings ->
      List.concat_map (entries_of_module_binding ns comments) bindings
  | _ -> []

let to_entries ns comments s =
  List.concat_map (entries_of_struct_item ns comments) s.str_items
