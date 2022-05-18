let fully_qualified_name = Misc.fully_qualified_name
let is_documented = Misc.is_documented
let pattern_idents = Misc.pattern_idents
let entries_of_type_declaration = Misc.entries_of_type_declaration
let mk_entry = Misc.mk_entry

open Ppxlib
open Analysis

let entries_of_sig_value_description ~path ns vd =
  mk_entry
    (fully_qualified_name ns vd.pval_name.txt)
    Value vd.pval_loc path
    (is_documented vd.pval_attributes)

let rec entries_of_module_type ~path ns ident md =
  match md.pmty_desc with
  | Pmty_signature signature ->
      List.concat_map (entries_of_sig_item ~path (ident :: ns)) signature
  | Pmty_functor (_, mtyp) -> entries_of_module_type ~path ns ident mtyp
  | _ -> []

and entries_of_sig_item ~path ns sign =
  match sign.psig_desc with
  | Psig_value vd -> [ entries_of_sig_value_description ~path ns vd ]
  | Psig_type (_, decls) ->
      List.concat_map (entries_of_type_declaration ~path ns) decls
  | Psig_module
      { pmd_name = { txt = Some ident; _ }; pmd_type; pmd_attributes; _ } ->
      mk_entry
        (fully_qualified_name ns ident)
        (match pmd_type.pmty_desc with
        | Pmty_functor (_, _) -> Functor
        | _ -> Module)
        pmd_type.pmty_loc path
        (is_documented pmd_attributes)
      :: entries_of_module_type ~path ns ident pmd_type
  | Psig_modtype decl -> entries_of_module_type_declaration ~path ns decl
  | _ -> []

and entries_of_module_type_declaration ~path ns decl : Entry.t option list =
  let xs =
    match decl.pmtd_type with
    | Some { pmty_desc = Pmty_signature signature; _ } ->
        List.concat_map
          (entries_of_sig_item ~path (decl.pmtd_name.txt :: ns))
          signature
    | _ -> []
  in
  mk_entry
    (fully_qualified_name ns decl.pmtd_name.txt)
    ModuleType decl.pmtd_loc path
    (is_documented decl.pmtd_attributes)
  :: xs

let toplevel ~path strs =
  match strs with
  | { psig_desc = Psig_attribute attribute; _ } :: _ -> (
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

let to_entries ~path sigs =
  toplevel ~path sigs :: List.concat_map (entries_of_sig_item ~path []) sigs
  |> List.filter_map (fun x -> x)
