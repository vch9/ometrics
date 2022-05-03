let fully_qualified_name ns ident = String.concat "." @@ List.rev (ident :: ns)

let is_attribute_doc attribute =
  let open Ppxlib in
  let attr_name = attribute.attr_name.txt in
  attr_name = "ocaml.doc"

let is_documented attributes = List.exists is_attribute_doc attributes

(** [pattern_idents ns loc path] returns the list of fresh identifiers
    introduced by [vb] *)
let rec pattern_idents pat : string list =
  let open Ppxlib in
  match pat.ppat_desc with
  | Ppat_var x -> [ x.txt ]
  | Ppat_alias (pat, id) -> id.txt :: pattern_idents pat
  | Ppat_array pats | Ppat_tuple pats -> List.concat_map pattern_idents pats
  | Ppat_construct (_, Some (_, pat))
  | Ppat_variant (_, Some pat)
  | Ppat_lazy pat
  | Ppat_exception pat ->
      pattern_idents pat
  | Ppat_record (fields, _) ->
      List.concat_map (fun (_, pat) -> pattern_idents pat) fields
  | Ppat_or (pat1, pat2) ->
      List.append (pattern_idents pat1) (pattern_idents pat2)
  | _ -> []

let line loc = loc.Warnings.loc_start.Lexing.pos_lnum

let entries_of_type_declaration ~path ns decl : Entry.t list =
  let open Ppxlib in
  let entry_documented = is_documented decl.ptype_attributes in
  [
    {
      entry_name = fully_qualified_name ns decl.ptype_name.txt;
      entry_kind = Type;
      entry_documented;
      entry_line = line decl.ptype_loc;
      entry_file = path;
    };
  ]
