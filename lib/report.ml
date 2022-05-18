open Analysis

let default_msg =
  "ometrics has not found any code quality issues in your changes."

(** [link_prefix git hash] computes the link prefix based on [git] and [hash].

    Example:
    link_prefix "https://gitlab.com/nomadic-labs/tezos.git" "your_hash" it creates:
    https://gitlab.com/nomadic-labs/tezos/-/tree/your_hash
*)
let link_prefix git hash =
  let is_gitlab =
    let re =
      Str.regexp "https:\\/\\/gitlab\\.com\\/\\(.+\\)\\/\\(.+\\)\\.git"
    in
    Str.string_match re git 0
  in
  if is_gitlab then
    let group = Str.matched_group 1 git in
    let project = Str.matched_group 2 git in
    Some
      (Printf.sprintf "https://gitlab.com/%s/%s/-/tree/%s/" group project hash)
  else None

let with_link clickable git hash =
  let ( >>= ) = Option.bind in
  (if clickable then git else None) >>= fun git -> link_prefix git hash

(** {2. Markdown } *)

let intro = "⚠ There are code quality issues:"

let report_file_markdown ?with_link fmt (p, deps) =
  let open Format in
  match deps with
  | [] -> ()
  | deps ->
      fprintf fmt "  * <details><summary markdown=\"span\">%s</summary>\n\n" p;
      List.iter
        (fun entry ->
          pp_print_string fmt "    * ";
          Entry.pp_markdown fmt ?with_link entry;
          pp_print_string fmt "\n")
        deps;
      pp_print_string fmt "\n"

let report_markdown ~clickable fmt git hash entries =
  let open Format in
  match entries with
  | [] -> pp_print_string fmt default_msg
  | entries ->
      let Git.(Hash hash) = hash in
      let with_link = with_link clickable git hash in
      fprintf fmt "<details><summary markdown=\"span\">%s</summary>\n\n" intro;
      List.iter (report_file_markdown ?with_link fmt) entries;
      pp_print_string fmt "</details>\n"

(** {2. HTML} *)

let pp_file_html fmt ?with_link (p, deps) =
  let open Format in
  fprintf fmt {|<h3>%s</h3>
<ol>
%a
</ol>
|} p
    (pp_print_list ~pp_sep:pp_print_newline (fun fmt ->
         fprintf fmt "  <li>%a</li>" (Entry.pp_html ?with_link)))
    deps

let report_html ~clickable ?title fmt git hash entries =
  let Git.(Hash hash) = hash in
  let with_link = with_link clickable git hash in
  let open Format in
  Format.fprintf fmt
    {|<html>
<head>
<title>%s</title>
</head>
<body>
  <h2>%s</h2>
|}
    (Option.value ~default:hash title)
    (match entries with
    | [] -> default_msg
    | _ -> Format.sprintf "Code quality issues introduced since %s" hash);
  List.iter (pp_file_html fmt ?with_link) entries;
  pp_print_string fmt "<body>\n</html>\n"

(** {2. Classic} *)

let report fmt entries =
  match entries with
  | [] -> Format.pp_print_string fmt default_msg
  | entries ->
      List.iter
        (fun (p, deps) ->
          Format.(
            if 0 < List.length deps then (
              fprintf fmt "@[<v># `%s`@ @ @]" p;
              fprintf fmt "@[<v>%a@ @ @]"
                (pp_print_list ~pp_sep:pp_print_space (fun fmt e ->
                     fprintf fmt "- `%a`" Entry.pp e))
                deps)))
        entries

(** {2. GitLab } *)

let pp_gitlab_entry ?(comma = false) fmt path
    Entry.{ entry_name; entry_line; entry_description; _ } =
  let entry_name = Entry.base_entry_name entry_name in

  let hash =
    let ctx = Digestif.SHA256.empty in
    let bytes = Marshal.to_bytes (entry_name, path, entry_line) [] in
    let ctx = Digestif.SHA256.feed_bytes ctx bytes in
    Digestif.SHA256.get ctx
  in

  Format.fprintf fmt
    {|
  {
    "description" : "'%s' %s.",
    "fingerprint" : "%a",
    "severity": "minor",
    "location": {
      "path": "%s",
      "lines": {
        "begin": %d
      }
    }
  }%s
|}
    entry_name entry_description Digestif.SHA256.pp hash path entry_line
    (if comma then "," else "")

let report_gitlab fmt entries =
  let open Format in
  let flat_entries =
    List.fold_left
      (fun acc (p, deps) -> List.map (fun dep -> (p, dep)) deps @ acc)
      [] entries
  in

  match flat_entries with
  | [] -> pp_print_string fmt "[]"
  | _ ->
      let rec aux fmt xs =
        match xs with
        | [ (p, dep) ] -> pp_gitlab_entry fmt p dep
        | (p, dep) :: xs ->
            pp_gitlab_entry ~comma:true fmt p dep;
            aux fmt xs
        | [] -> assert false
      in
      pp_print_string fmt "[";
      aux fmt flat_entries;
      pp_print_string fmt "]"

(** {2. Entrypoint} *)

let report_full ~format ~clickable ?title ?output ?git hash entries =
  let fmt =
    match output with
    | None -> Format.std_formatter
    | Some file ->
        let oc = open_out file in
        Format.formatter_of_out_channel oc
  in
  let entries =
    List.filter_map
      (fun (p, deps) -> match deps with [] -> None | deps -> Some (p, deps))
      entries
  in
  let () =
    match format with
    | `Markdown -> report_markdown ~clickable fmt git hash entries
    | `Html -> report_html ~clickable ?title fmt git hash entries
    | `Gitlab -> report_gitlab fmt entries
    | `Classic -> report fmt entries
  in
  Format.pp_print_flush fmt ()
