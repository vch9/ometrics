open Typedtree
open Types
open Merlin_analysis

let pp_print_mark fmt = function
  | true -> Format.pp_print_string fmt "✓"
  | false -> ()

type kind = ModuleType | Module | Type | Value

type entry = { entry_kind : kind; entry_name : string; entry_documented : bool }

let pp_entry fmt { entry_kind; entry_name; entry_documented } =
  let open Format in
  let pp_kind fmt = function
    | ModuleType -> pp_print_string fmt "ModuleType"
    | Module -> pp_print_string fmt "Module"
    | Type -> pp_print_string fmt "Type"
    | Value -> pp_print_string fmt "Value"
  in

  Format.fprintf fmt "@[<h>%a: %s %a@]" pp_kind entry_kind entry_name
    pp_print_mark entry_documented

let fully_qualified_name ns ident =
  String.concat "." @@ List.rev (Ident.name ident :: ns)

let make_config path =
  let mconfig = Mconfig.initial in
  let path = Merlin_utils.Misc.canonicalize_filename path in
  let filename = Filename.basename path in
  let directory = Filename.dirname path in
  let mconfig =
    {
      mconfig with
      query = { mconfig.query with verbosity = 1; filename; directory };
    }
  in
  Mconfig.get_external_config path mconfig

let merlin_config target =
  let basename = Filename.basename target in
  let dirname = Filename.dirname target in

  let init = Mconfig.get_external_config target Mconfig.initial in
  {
    init with
    query = { init.query with directory = dirname; filename = basename };
  }

let merlin_source target =
  let file_channel = open_in target in
  let file_size = in_channel_length file_channel in
  let file_content = really_input_string file_channel file_size in
  close_in file_channel;
  Msource.make file_content

(** Tada *)
let is_documented comments loc =
  Ocamldoc.associate_comment comments loc Location.none |> fst |> Option.is_some

(** [pattern_idents ns loc path] returns the list of fresh identifiers
    introduced by [vb] *)
let rec pattern_idents : type k. k general_pattern -> string list =
 fun pat ->
  match pat.pat_desc with
  | Tpat_var (id, _) -> [ Ident.name id ]
  | Tpat_alias (pat, id, _) -> Ident.name id :: pattern_idents pat
  | Tpat_array pats | Tpat_construct (_, _, pats) | Tpat_tuple pats ->
      List.concat_map pattern_idents pats
  | Tpat_variant (_, Some pat, _) | Tpat_lazy pat | Tpat_exception pat ->
      pattern_idents pat
  | Tpat_record (fields, _) ->
      List.concat_map (fun (_, _, pat) -> pattern_idents pat) fields
  | Tpat_or (pat1, pat2, _) ->
      List.append (pattern_idents pat1) (pattern_idents pat2)
  | _ -> []

(* --------------------------------------------------------------------------- *)

let inspect_value_binding ns comments binding =
  let entry_documented = is_documented comments binding.vb_loc in
  List.map
    (fun ident ->
      {
        entry_name = String.concat "." @@ List.rev (ident :: ns);
        entry_kind = Value;
        entry_documented;
      })
    (pattern_idents binding.vb_pat)

let inspect_type_declaration ns comments decl =
  [
    {
      entry_name = fully_qualified_name ns decl.typ_id;
      entry_kind = Type;
      entry_documented = is_documented comments decl.typ_loc;
    };
  ]

let inspect_struct_item ns comments str_item : entry list =
  match str_item.str_desc with
  | Tstr_value (_, bindings) ->
      List.concat_map (inspect_value_binding ns comments) bindings
  | Tstr_type (_, decls) ->
      List.concat_map (inspect_type_declaration ns comments) decls
  | _ -> []

(* --------------------------------------------------------------------------- *)

let inspect_sig_value ns comments ident descr =
  [
    {
      entry_name = fully_qualified_name ns ident;
      entry_kind = Value;
      entry_documented = is_documented comments descr.val_loc;
    };
  ]

let inspect_sig_type ns comments ident decl =
  [
    {
      entry_name = fully_qualified_name ns ident;
      entry_kind = Type;
      entry_documented = is_documented comments decl.type_loc;
    };
  ]

let inspect_sig_item ns comments : Types.signature_item -> entry list = function
  | Sig_value (ident, descr, Exported) ->
      inspect_sig_value ns comments ident descr
  | Sig_type (ident, decl, _, Exported) ->
      inspect_sig_type ns comments ident decl
  | _ -> []

(* --------------------------------------------------------------------------- *)

let inspect_typedtree ns comments = function
  | `Implementation s ->
      List.concat_map (inspect_struct_item ns comments) s.str_items
  | `Interface i -> List.concat_map (inspect_sig_item ns comments) i.sig_type

(** [choose_file "target.ml"] returns "target.mli" if it exists,
    "target.ml" otherwise. *)
let choose_file target =
  let mli_target = target ^ "i" in
  if Sys.file_exists mli_target then mli_target else target

let toplevel_entry comments name =
  {
    entry_kind = Module;
    entry_name = name;
    entry_documented = is_documented comments Location.none;
  }

let _ =
  let target = choose_file Sys.argv.(1) in

  let mconfig = make_config target in
  let msource = merlin_source target in

  let unit = Mconfig.unitname mconfig in

  let pipeline = Mpipeline.make mconfig msource in

  let deps : entry list =
    Mpipeline.with_pipeline pipeline (fun _ ->
        let typing = Mpipeline.typer_result pipeline in
        let typedtree = Mtyper.get_typedtree typing in
        let comments = Mpipeline.reader_comments pipeline in
        toplevel_entry comments unit
        :: inspect_typedtree [ unit ] comments typedtree)
  in

  Format.(
    printf "@[<v>%a@]" (pp_print_list ~pp_sep:pp_print_space pp_entry) deps)
