let fully_qualified_name = Misc.fully_qualified_name

let is_documented = Misc.is_documented

let pattern_idents = Misc.pattern_idents

let line = Misc.line

let entries_of_type_declaration = Misc.entries_of_type_declaration

open Ppxlib

let entries_of_sig_value_description ~path ns vd =
  let entry_documented = is_documented vd.pval_attributes in
  [
    Entry.
      {
        entry_name = fully_qualified_name ns vd.pval_name.txt;
        entry_kind = Value;
        entry_documented;
        entry_line = line vd.pval_loc;
        entry_file = path;
      };
  ]

let rec entries_of_module_type ~path ns ident md =
  match md.pmty_desc with
  | Pmty_signature signature ->
      List.concat_map (entries_of_sig_item ~path (ident :: ns)) signature
  | Pmty_functor (_, mtyp) -> entries_of_module_type ~path ns ident mtyp
  | _ -> []

and entries_of_sig_item ~path ns sign : 'a list =
  match sign.psig_desc with
  | Psig_value vd -> entries_of_sig_value_description ~path ns vd
  | Psig_type (_, decls) ->
      List.concat_map (entries_of_type_declaration ~path ns) decls
  | Psig_module
      { pmd_name = { txt = Some ident; _ }; pmd_type; pmd_attributes; _ } ->
      let entry_documented = is_documented pmd_attributes in
      Entry.
        {
          entry_name = fully_qualified_name ns ident;
          entry_kind =
            (match pmd_type.pmty_desc with
            | Pmty_functor (_, _) -> Functor
            | _ -> Module);
          entry_documented;
          entry_line = line pmd_type.pmty_loc;
          entry_file = path;
        }
      :: entries_of_module_type ~path ns ident pmd_type
  | _ -> []

let to_entries ~path sigs = List.concat_map (entries_of_sig_item ~path []) sigs
