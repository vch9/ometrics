open Entry
open Misc

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

let entries_of_typedtree ns comments = function
  | `Implementation s -> Implementation.to_entries ns comments s
  | `Interface i -> Interface.to_entries ns comments i

let toplevel_entry comments name =
  {
    entry_kind = Module;
    entry_name = name;
    entry_documented = is_documented comments Location.none;
  }

let to_entries target =
  let mconfig = make_config target in
  let msource = merlin_source target in

  let unit = Mconfig.unitname mconfig in

  let pipeline = Mpipeline.make mconfig msource in

  Mpipeline.with_pipeline pipeline (fun _ ->
      let typing = Mpipeline.typer_result pipeline in
      let typedtree = Mtyper.get_typedtree typing in
      let comments = Mpipeline.reader_comments pipeline in
      toplevel_entry comments unit
      :: entries_of_typedtree [ unit ] comments typedtree)
