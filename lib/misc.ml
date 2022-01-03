open Typedtree
open Merlin_analysis

let fully_qualified_name ns ident =
  String.concat "." @@ List.rev (Ident.name ident :: ns)

let is_documented comments loc =
  Ocamldoc.associate_comment comments loc Location.none |> fst |> Option.is_some

(** [pattern_idents ns loc path] returns the list of fresh identifiers
    introduced by [vb] *)
let rec pattern_idents : type k. k general_pattern -> string list =
 fun pat ->
  match pat.pat_desc with
  | Tpat_var (id, _) -> [ Ident.name id ]
  | Tpat_alias (pat, id, _) -> Ident.name id :: pattern_idents pat
  | Tpat_array pats | Tpat_construct (_, _, pats, _) | Tpat_tuple pats ->
      List.concat_map pattern_idents pats
  | Tpat_variant (_, Some pat, _) | Tpat_lazy pat | Tpat_exception pat ->
      pattern_idents pat
  | Tpat_record (fields, _) ->
      List.concat_map (fun (_, _, pat) -> pattern_idents pat) fields
  | Tpat_or (pat1, pat2, _) ->
      List.append (pattern_idents pat1) (pattern_idents pat2)
  | _ -> []
