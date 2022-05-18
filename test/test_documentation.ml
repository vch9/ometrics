open Analysis.Entry
open Documentation

module type INPUT = sig
  val modname : string
  val modpath : string
end

module Make (I : INPUT) = struct
  let computed_entries = Toplevel.to_entries I.modpath

  let toplevel =
    List.find_opt
      (fun { entry_kind; entry_name; _ } ->
        entry_kind = Toplevel && entry_name = I.modname)
      computed_entries
    |> Option.is_some

  let find_entry kind name =
    List.find_opt
      (fun { entry_kind; entry_name; _ } ->
        entry_kind = kind && entry_name = name)
      computed_entries
    |> Option.is_some

  let assert_documented kind name =
    Alcotest.(check bool) "documented entry" false (find_entry kind name)

  let assert_undocumented kind name =
    Alcotest.(check bool) "undocumented entry" true (find_entry kind name)
end

module ML = Make (struct
  let modname = "Example"
  let modpath = "example.ml"
end)

module MLI = Make (struct
  let modname = "Example"
  let modpath = "example.mli"
end)

(** {1 Tests} *)

(** {2 Types} *)

let ml_t_is_undocumented () = ML.assert_undocumented Type "t"
let ml_u_is_documented () = ML.assert_documented Type "u"
let ml_x_is_documented () = ML.assert_documented Value "x"
let mli_t_is_documented () = MLI.assert_documented Type "t"
let mli_u_is_undocumented () = MLI.assert_undocumented Type "u"

(** {2 Values} *)

let ml_foo_is_documented () = ML.assert_documented Value "foo"
let mli_foo_is_undocumented () = ML.assert_documented Value "foo"

(** {2 Modules} *)

(** {3 ML} *)

(* Fails because it is not considered as a [ocaml.doc] attribute *)
(* let ml_module_B_is_documented () = ML.assert_documented Module "B" *)

let ml_toplevel_is_documented () =
  Alcotest.(check bool) "toplevel" false ML.toplevel

let ml_module_A_is_documented () = ML.assert_documented Module "A"
let ml_module_A_x_documented () = ML.assert_documented Value "A.x"
let ml_module_B_x_undocumented () = ML.assert_undocumented Value "B.x"
let ml_module_C_is_undocumented () = ML.assert_undocumented Module "C"
let ml_module_Foo_x_documented () = ML.assert_documented Value "Foo.x"

(** {3 MLI} *)

(* Fails because it is not considered as a [ocaml.doc] attribute *)
(* let mli_module_B_is_documented () = MLI.assert_documented Module "B" *)

let mli_toplevel_is_documented () =
  Alcotest.(check bool) "toplevel" false MLI.toplevel

let mli_module_A_is_documented () = MLI.assert_documented Module "A"
let mli_module_C_is_undocumented () = MLI.assert_undocumented Module "C"
let mli_module_Foo_x_is_undocumented () = MLI.assert_undocumented Value "Foo.x"
let mli_module_C_x_is_documented () = MLI.assert_undocumented Value "Foo.x"

let tests =
  ( "Entry",
    Alcotest.
      [
        (* Implementation *)
        test_case "ml-toplevel-is-documented" `Quick ml_toplevel_is_documented;
        test_case "ml-t-is-undocumented" `Quick ml_t_is_undocumented;
        test_case "ml-u-is-documented" `Quick ml_u_is_documented;
        test_case "ml-x-is-documented" `Quick ml_x_is_documented;
        test_case "ml-foo-is-documented" `Quick ml_foo_is_documented;
        test_case "ml-Foo-x-is-documented" `Quick ml_module_Foo_x_documented;
        test_case "ml-A-is-documented" `Quick ml_module_A_is_documented;
        test_case "ml-A-x-is-documented" `Quick ml_module_A_x_documented;
        test_case "ml-B-x-is-undocumented" `Quick ml_module_B_x_undocumented;
        test_case "ml-C-is-undocumented" `Quick ml_module_C_is_undocumented;
        (* Interface *)
        test_case "mli-toplevel-is-documented" `Quick mli_toplevel_is_documented;
        test_case "mli-t-is-documented" `Quick mli_t_is_documented;
        test_case "mli-u-is-undocumented" `Quick mli_u_is_undocumented;
        test_case "mli-foo-is-undocumented" `Quick mli_foo_is_undocumented;
        test_case "mli-Foo-x-is-undocumented" `Quick
          mli_module_Foo_x_is_undocumented;
        test_case "mli-A-is-documented" `Quick mli_module_A_is_documented;
        test_case "mli-C-is-undocumented" `Quick mli_module_C_is_undocumented;
        test_case "mli-C-x-is-documented" `Quick mli_module_C_x_is_documented
        (* Failing tests *)
        (* test_case "ml-B-is-documented" `Quick ml_module_B_is_documented; *)
        (* test_case "mli-B-is-documented" `Quick mli_module_B_is_documented; *);
      ] )
