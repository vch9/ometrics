open QCheck2
module Change = Ometrics__Change

let eq = Alcotest.of_pp Change.pp

let gen_addition = Gen.(oneof [ pure "A"; pure "C" ])

let gen_deletion = Gen.pure "D"

let gen_edition = Gen.pure "M"

let gen_renaming = Gen.(map (fun x -> Printf.sprintf "R%d" x) (0 -- 100))

let gen_printable =
  let gen_int = Gen.(5 -- 20) in
  let gen_char = Gen.char_range 'a' 'z' in
  Gen.string_size ~gen:gen_char gen_int

let gen_ml_file =
  let open Gen in
  oneof
    [
      map (fun x -> x ^ ".ml") gen_printable;
      map (fun x -> x ^ ".mli") gen_printable;
    ]

let gen_change =
  let open Gen in
  let open Change in
  oneof
    [
      map (fun x -> Addition x) gen_ml_file;
      map (fun x -> Deletion x) gen_ml_file;
      map (fun x -> Edition x) gen_ml_file;
      map (fun (x, y) -> Renaming (x, y)) (pair gen_ml_file gen_ml_file);
    ]

let print_change = Format.asprintf "%a" Change.pp

let eq_list = ( = )

let pp_list = Format.pp_print_list Change.pp

let test_change_from_string_addition_any =
  Test.make ~name:"change A|C any => Addition any"
    ~print:Print.(pair string string)
    Gen.(pair gen_printable gen_addition)
    (fun (file, mode) ->
      let s = Printf.sprintf "%s\t%s" mode file in

      let expected = Change.(Addition file) in
      let actual = Change.change_from_string s in

      Helpers.qcheck_eq ~eq:Change.eq ~pp:Change.pp expected actual)
  |> QCheck_alcotest.to_alcotest

let test_change_from_string_deletion_any =
  Test.make ~name:"change D any => Deletion any"
    ~print:Print.(pair string string)
    Gen.(pair gen_printable gen_deletion)
    (fun (file, mode) ->
      let s = Printf.sprintf "%s\t%s" mode file in

      let expected = Change.(Deletion file) in
      let actual = Change.change_from_string s in

      Helpers.qcheck_eq ~eq:Change.eq ~pp:Change.pp expected actual)
  |> QCheck_alcotest.to_alcotest

let test_change_from_string_edition_any =
  Test.make ~name:"change M any => Edition any"
    ~print:Print.(pair string string)
    Gen.(pair gen_printable gen_edition)
    (fun (file, mode) ->
      let s = Printf.sprintf "%s\t%s" mode file in

      let expected = Change.(Edition file) in
      let actual = Change.change_from_string s in

      Helpers.qcheck_eq ~eq:Change.eq ~pp:Change.pp expected actual)
  |> QCheck_alcotest.to_alcotest

let test_change_from_string_renaming_any =
  Test.make ~name:"change R src dst => Renaming (src, dst)"
    ~print:Print.(triple string string string)
    Gen.(triple gen_printable gen_printable gen_renaming)
    (fun (src, dst, mode) ->
      let s = Printf.sprintf "%s\t%s\t%s" mode src dst in

      let expected = Change.(Renaming (src, dst)) in
      let actual = Change.change_from_string s in

      Helpers.qcheck_eq ~eq:Change.eq ~pp:Change.pp expected actual)
  |> QCheck_alcotest.to_alcotest

let test_change_from_string_fail =
  Test.make ~name:"change _ should raise an error" ~print:Print.string
    Gen.string (fun s ->
      try
        let s = "_" ^ s in
        let _ = Change.change_from_string s in
        false
      with
      | Invalid_argument _ -> true
      | _ -> false)
  |> QCheck_alcotest.to_alcotest

let test_is_ml_change =
  Test.make ~name:"is_ml_change on .ml and .mli" ~print:print_change gen_change
    (fun change ->
      match change with
      | Deletion _ -> true
      | change -> Change.is_ml_change change)
  |> QCheck_alcotest.to_alcotest

let test_merge_same_list =
  Test.make ~name:"merge [x] [x] = [x]" ~print:print_change gen_change
    (fun change ->
      let prev = [ change ] in
      let next = [ change ] in
      let expected = [ change ] in
      let actual = Change.merge_changes prev next in

      Helpers.qcheck_eq ~pp:pp_list ~eq:eq_list expected actual)
  |> QCheck_alcotest.to_alcotest

let tests =
  ( "Change",
    [
      test_change_from_string_addition_any;
      test_change_from_string_deletion_any;
      test_change_from_string_edition_any;
      (* test_change_from_string_renaming_any; *)
      test_change_from_string_fail;
      test_is_ml_change;
      test_merge_same_list;
    ] )
