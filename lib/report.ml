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

let intro = "⚠ There are changes which are not documented:"

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

let report fmt entries =
  List.iter
    (fun (p, deps) ->
      Format.(
        if 0 < List.length deps then (
          fprintf fmt "@[<v># `%s`@ @ @]" p;
          fprintf fmt "@[<v>%a@ @ @]"
            (pp_print_list ~pp_sep:pp_print_space (fun fmt e ->
                 fprintf fmt "- `%a`" (Entry.pp ~with_mark:false) e))
            deps)))
    entries

let with_link clickable git hash =
  let ( >>= ) = Option.bind in
  (if clickable then git else None) >>= fun git -> link_prefix git hash

(** [report_markdown ~clickable fmt git hash entries] creates a report for
    [entries] in a markdown format. It list every unducommented entry found in
    [entries] with a clickable link (iff [clickable] is true) to the line in
    question. *)
let report_markdown ~clickable fmt git hash entries =
  let open Format in
  let Git.(Hash hash) = hash in
  let with_link = with_link clickable git hash in
  fprintf fmt "<details><summary markdown=\"span\">%s</summary>\n\n" intro;
  List.iter (report_file_markdown ?with_link fmt) entries;
  pp_print_string fmt "</details>\n"

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
    (Format.sprintf "Undocumented entries introduced since %s" hash);
  List.iter (pp_file_html fmt ?with_link) entries;
  pp_print_string fmt "<body>\n</html>\n"

let report_full ~markdown ~html ~clickable ?title ?output ?git hash entries =
  (* Finally, we print the undocumented entries found *)
  let fmt =
    Option.fold ~none:Format.std_formatter
      ~some:(fun file ->
        let oc = open_out file in
        Format.formatter_of_out_channel oc)
      output
  in
  if markdown then report_markdown ~clickable fmt git hash entries
  else if html then report_html ~clickable ?title fmt git hash entries
  else report fmt entries
