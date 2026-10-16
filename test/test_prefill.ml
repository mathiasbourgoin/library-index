(* Deterministic pre-fill with provenance (CHECK-35, AC-35, FR-110..FR-112): the pre-fill
   order, auto values replaced by a refresh and never curated ones, annotate flipping a
   field to curated, exports carrying curated values only, and the no-LLM source scan. *)

open Library_index
open Test_support.Support
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

let opt = Alcotest.(option string)

let order () =
  let readme =
    "# T\n\nA readme paragraph long enough to be a summary of the library.\n"
  in
  Alcotest.check opt "description first" (Some "Desc")
    (Prefill.what_of ~description:(Some "Desc") ~synopsis:(Some "Syn")
       ~readme:(Some readme));
  Alcotest.check opt "then the synopsis" (Some "Syn")
    (Prefill.what_of ~description:(Some "  ") ~synopsis:(Some "Syn") ~readme:(Some readme));
  Alcotest.check opt "then the README's first paragraph"
    (Some "A readme paragraph long enough to be a summary of the library.")
    (Prefill.what_of ~description:None ~synopsis:None ~readme:(Some readme));
  Alcotest.check opt "nothing known" None
    (Prefill.what_of ~description:None ~synopsis:None ~readme:None);
  Alcotest.check opt "interest template" (Some "Own OCaml code (Libraries)")
    (Prefill.interest_of ~ownership:"own" ~language:(Some "OCaml")
       ~domain:(Some "Libraries"));
  Alcotest.check opt "third-party, language only" (Some "Third-party Go code")
    (Prefill.interest_of ~ownership:"external" ~language:(Some "Go") ~domain:None);
  Alcotest.check opt "empty when nothing is known" None
    (Prefill.interest_of ~ownership:"participant" ~language:None ~domain:None)

let get ctx key = ok_or_fail (Ops.get ctx (`Assoc [ ("repo", `String key) ]))
let field ctx key f = to_str (member f (get ctx key))
let prov ctx key f = to_str (member f (member "provenance" (get ctx key)))

let refresh ctx forge key =
  let o =
    ok_or_fail
      (Ops.refresh ctx (Fake_forge.env forge) ~allow_full:false
         (`Assoc [ ("repo", `String key) ]))
  in
  Alcotest.(check bool) "refresh complete" true o.complete

let curated_entry (ctx : Ctx.t) key =
  member key (Yojson.Safe.from_file (Filename.concat ctx.export_dir "curated.json"))

let auto_and_curated () =
  let ctx = empty_ctx () in
  let fn = "example-user/gizmo" in
  let key = "github:" ^ fn in
  ignore (F.insert_repo ctx ~owner:"example-user" "gizmo");
  let forge = Fake_forge.create () in
  let describe d =
    Fake_forge.json forge ("repos/" ^ fn) (Fake_forge.gh_repo ~description:d fn)
  in
  describe "First description";
  refresh ctx forge key;
  Alcotest.(check string)
    "pre-filled from the description" "First description" (field ctx key "what");
  Alcotest.(check string) "marked auto" "auto" (prov ctx key "what");
  Alcotest.(check string) "interest template" "Own OCaml code" (field ctx key "interest");
  Alcotest.(check string) "interest auto" "auto" (prov ctx key "interest");
  (* A refresh bringing a new description replaces an auto value. *)
  describe "Second description";
  refresh ctx forge key;
  Alcotest.(check string) "auto overwritten" "Second description" (field ctx key "what");
  (* Exports carry curated values only. *)
  ignore (Export.export ctx ~dir:ctx.export_dir);
  Alcotest.(check bool)
    "auto what not exported" true
    (member "what" (curated_entry ctx key) = `Null);
  (* annotate flips the field to curated; refresh never overwrites it again. *)
  ignore
    (ok_or_fail
       (Ops.annotate ctx
          (`Assoc
             [
               ("repo", `String key); ("fields", `Assoc [ ("what", `String "My words") ]);
             ])));
  Alcotest.(check string) "annotate makes it curated" "curated" (prov ctx key "what");
  describe "Third description";
  refresh ctx forge key;
  Alcotest.(check string) "curated never overwritten" "My words" (field ctx key "what");
  Alcotest.(check string) "interest still auto" "auto" (prov ctx key "interest");
  let e = curated_entry ctx key in
  Alcotest.(check string) "curated what exported" "My words" (to_str (member "what" e));
  Alcotest.(check bool) "auto interest not exported" true (member "interest" e = `Null);
  (* An auto value whose source disappears is cleared, not left stale. *)
  Db.run ctx.db "UPDATE repos SET language = NULL WHERE key = ?" [ Db.text key ];
  ignore
    (Db.with_txn ctx.db (fun () ->
         Ok (Prefill.apply ctx.db ~now:"2026-10-04T00:00:00Z" ~key ())));
  Alcotest.(check string)
    "nothing known: auto interest cleared" "null" (field ctx key "interest")

(* Imported curated.json values are curated: a refresh leaves them, the export round-trips
   them unchanged (MIG-1 relies on this). *)
let imported () =
  let ctx = fresh_ctx () in
  let before = Yojson.Safe.from_file (Filename.concat fixtures_dir "curated.json") in
  let key =
    match before with
    | `Assoc m ->
        List.find_map
          (fun (k, v) ->
            match member "what" v with `String _ when k <> "_doc" -> Some k | _ -> None)
          m
    | _ -> None
  in
  let key = match key with Some k -> k | None -> Alcotest.fail "fixture has no what" in
  Alcotest.(check string) "imported what is curated" "curated" (prov ctx key "what");
  ignore
    (Db.with_txn ctx.db (fun () ->
         Ok (Prefill.apply ctx.db ~now:"2026-10-04T00:00:00Z" ())));
  ignore (Export.export ctx ~dir:ctx.export_dir);
  let after = Yojson.Safe.from_file (Filename.concat ctx.export_dir "curated.json") in
  Alcotest.(check bool)
    "export unchanged by the pre-fill" true
    (Yojson.Safe.equal before after)

let no_llm () =
  let files = source_files () in
  Alcotest.(check bool) "scanned lib/ and bin/" true (List.length files > 60);
  Alcotest.(check (list string))
    "no model API host, endpoint or SDK" [] (model_api_hits files);
  (* The scan does detect a marker. *)
  let dir = temp_dir () in
  let probe = Filename.concat dir "probe.ml" in
  F.write_file probe "let url = \"https://api.openai.com/v1/chat/completions\"\n";
  Alcotest.(check bool) "detector works" true (model_api_hits [ probe ] <> [])

let () =
  run "prefill"
    [
      ( "prefill",
        [
          Alcotest.test_case "AC-35 order" `Quick order;
          Alcotest.test_case "AC-35 auto vs curated" `Quick auto_and_curated;
          Alcotest.test_case "AC-35 imported" `Quick imported;
        ] );
      ("no-llm", [ Alcotest.test_case "AC-35 source scan" `Quick no_llm ]);
    ]
