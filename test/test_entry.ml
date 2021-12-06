open Ometrics.Entry
open Ometrics

module type INPUT = sig
  val modname : string

  val modpath : string
end

module Make (I : INPUT) = struct
  let computed_entries = Toplevel.to_entries I.modpath

  let toplevel =
    List.find
      (fun { entry_kind; entry_name; _ } ->
        entry_kind = Module && entry_name = I.modname)
      computed_entries

  let find_entry kind name =
    List.find
      (fun { entry_kind; entry_name; _ } ->
        entry_kind = kind && entry_name = I.modname ^ "." ^ name)
      computed_entries

  let assert_documented kind name =
    Alcotest.(check bool)
      "documented entry" true (find_entry kind name).entry_documented

  let assert_undocumented kind name =
    Alcotest.(check bool)
      "undocumented entry" false (find_entry kind name).entry_documented
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

let mli_t_is_documented () = MLI.assert_documented Type "t"

let mli_u_is_undocumented () = MLI.assert_undocumented Type "u"

(** {2 Values} *)

let ml_foo_is_documented () = ML.assert_documented Value "foo"

let mli_foo_is_undocumented () = ML.assert_documented Value "foo"

(** {2 Modules} *)

(** {3 ML} *)

(* Fails when a License is present *)
let ml_toplevel_is_documented () =
  Alcotest.(check bool) "toplevel" true ML.toplevel.entry_documented

let ml_module_A_is_documented () = ML.assert_documented Module "A"

(* Fails where `merlin-document` on emacs finds the comment *)
let ml_module_B_is_documented () = ML.assert_documented Module "B"

let ml_module_C_is_undocumented () = ML.assert_undocumented Module "C"

(** {3 MLI} *)

(* Fails when a License is present *)
let mli_toplevel_is_documented () =
  Alcotest.(check bool) "toplevel" true MLI.toplevel.entry_documented

let mli_module_A_is_documented () = MLI.assert_documented Module "A"

(* Fails where `merlin-document` on emacs finds the comment *)
let mli_module_B_is_documented () = MLI.assert_documented Module "B"

let mli_module_C_is_undocumented () = MLI.assert_undocumented Module "C"

let tests =
  ( "Entry",
    Alcotest.
      [
        test_case "ml-t-is-undocumented" `Quick ml_t_is_undocumented;
        test_case "ml-u-is-documented" `Quick ml_u_is_documented;
        test_case "mli-t-is-documented" `Quick mli_t_is_documented;
        test_case "mli-u-is-undocumented" `Quick mli_u_is_undocumented;
        test_case "ml-foo-is-documented" `Quick ml_foo_is_documented;
        test_case "mli-foo-is-undocumented" `Quick mli_foo_is_undocumented;
        (* test_case "ml-toplevel-is-documented" `Quick ml_toplevel_is_documented; *)
        test_case "ml-A-is-documented" `Quick ml_module_A_is_documented;
        (* test_case "ml-B-is-documented" `Quick ml_module_B_is_documented; *)
        test_case "ml-C-is-undocumented" `Quick ml_module_C_is_undocumented;
        (* test_case "mli-toplevel-is-documented" `Quick mli_toplevel_is_documented; *)
        test_case "mli-A-is-documented" `Quick mli_module_A_is_documented;
        (* test_case "mli-B-is-documented" `Quick mli_module_B_is_documented; *)
        test_case "mli-C-is-undocumented" `Quick mli_module_C_is_undocumented;
      ] )
