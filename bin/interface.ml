open Entry
open Misc
open Typedtree
open Types

let entries_of_sig_value ns comments ident descr =
  [
    {
      entry_name = fully_qualified_name ns ident;
      entry_kind = Value;
      entry_documented = is_documented comments descr.val_loc;
    };
  ]

let entries_of_sig_type ns comments ident decl =
  [
    {
      entry_name = fully_qualified_name ns ident;
      entry_kind = Type;
      entry_documented = is_documented comments decl.type_loc;
    };
  ]

let rec entries_of_sig_module ns comments ident decl =
  {
    entry_name = fully_qualified_name ns ident;
    entry_kind =
      (match decl.md_type with Mty_functor (_, _) -> Functor | _ -> Module);
    entry_documented = is_documented comments decl.md_loc;
  }
  :: entries_of_module_type ns comments ident decl.md_type

and entries_of_module_type ns comments ident = function
  | Mty_signature signature ->
      List.concat_map
        (entries_of_sig_item (Ident.name ident :: ns) comments)
        signature
  | Mty_functor (_, mtyp) -> entries_of_module_type ns comments ident mtyp
  | _ -> []

and entries_of_sig_item ns comments : Types.signature_item -> entry list =
  function
  | Sig_value (ident, descr, Exported) ->
      entries_of_sig_value ns comments ident descr
  | Sig_type (ident, decl, _, Exported) ->
      entries_of_sig_type ns comments ident decl
  | Sig_module (ident, _, decl, _, Exported) ->
      entries_of_sig_module ns comments ident decl
  | _ -> []

let to_entries ns comments i =
  List.concat_map (entries_of_sig_item ns comments) i.sig_type
