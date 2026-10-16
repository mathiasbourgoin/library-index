(* Documents store (S12, spec US-9, FR-080..FR-085): chunking, local markdown ingestion
   (CHECK-21), the .mli fallback (CHECK-22), tier/campaign gating and incremental
   re-reads (CHECK-23), and API-sourced documents through the fake forge (CHECK-26). *)

open Library_index
open Test_support.Support
module Fake_forge = Test_support.Fake_forge
module F = Test_support.Doc_fixture

let titles l = List.map (fun (c : Chunk.t) -> c.title) l
let contains s sub = Re.execp (Re.compile (Re.str sub)) s
let strs = Alcotest.(list string)

(* ------------------------------------------------------------ pure chunking *)

let markdown_chunks () =
  let md =
    "[![ci](badge)](link)\n\n\
     # Demo\n\n\
     Intro text.\n\n\
     ## Installing\n\n\
     Run it.\n\n\
     ```sh\n\
     # not a heading\n\
     ## nor this\n\
     ```\n\n\
     ### From source\n\n\
     Clone it.\n\n\
     #### Deep heading\n\n\
     still in from source\n\n\
     ## Usage ##\n"
  in
  let cs = Chunk.markdown ~default_title:"T" md in
  Alcotest.check strs "heading paths (fences ignored, h4 kept in body, empty dropped)"
    [ "T"; "Demo"; "Demo > Installing"; "Demo > Installing > From source" ]
    (titles cs);
  let installing = List.nth cs 2 in
  Alcotest.(check bool)
    "fence kept in its section" true
    (contains installing.text "# not a heading");
  Alcotest.(check bool)
    "h4 inside h3" true
    (contains (List.nth cs 3).text "#### Deep heading");
  Alcotest.(check (option string)) "first h1" (Some "Demo") (Chunk.markdown_title md);
  let para c = String.make 900 c in
  let long = String.concat "\n\n" [ para 'a'; para 'b'; para 'c' ] in
  let parts = Chunk.split_long ~title:"X" long in
  Alcotest.check strs "paragraph packing" [ "X"; "X (2)" ] (titles parts);
  let huge =
    Chunk.split_long ~title:"H" (String.concat "" (List.init 1500 (fun _ -> "é.")))
  in
  Alcotest.check strs "hard split" [ "H"; "H (2)" ] (titles huge);
  List.iter
    (fun (c : Chunk.t) ->
      Alcotest.(check bool) "≤ 2000 code points" true (Utf8.length c.text <= 2000);
      Alcotest.(check bool) "valid UTF-8" true (String.is_valid_utf_8 c.text))
    (parts @ huge);
  Alcotest.check strs "blank → nothing" [] (titles (Chunk.plain ~title:"P" " \n "))

let mli_chunks () =
  let cs = Chunk.mli ~module_name:"Itf" F.itf_mli in
  Alcotest.check strs "one chunk per top-level item"
    [
      "Itf (module)";
      "val parse_file";
      "type t";
      "module Sub";
      "exception Bad";
      "external fast";
      "val ( >>= )";
      "type box";
    ]
    (titles cs);
  let text t = (List.find (fun (c : Chunk.t) -> c.title = t) cs).text in
  Alcotest.(check bool)
    "doc before" true
    (contains (text "val parse_file") "Parse an ITF");
  Alcotest.(check bool) "doc after" true (contains (text "type t") "A parsed trace.");
  Alcotest.(check bool)
    "doc after (exception)" true
    (contains (text "exception Bad") "Raised on bad input.");
  Alcotest.(check bool)
    "nested item stays in its module" true
    (contains (text "module Sub") "val inner");
  Alcotest.(check bool) "module doc" true (contains (text "Itf (module)") "Apalache");
  Alcotest.(check bool) "and continues the item" true (contains (text "type box") "other");
  Alcotest.(check bool)
    "doc not duplicated" false
    (contains (text "type t") "Parse an ITF")

let selection () =
  let sel l =
    List.map (fun (p, k) -> (p, Doc_select.kind_name k)) (Doc_select.select l)
  in
  let pair = Alcotest.(list (pair string string)) in
  Alcotest.check pair "README, docs/**, other root md; exclusions"
    [
      ("README.md", "readme");
      ("docs/guide.md", "doc");
      ("docs/sub/deep.md", "doc");
      ("AGENTS.md", "doc");
    ]
    (sel
       [
         "README.md";
         "readme.org";
         "docs/guide.md";
         "docs/sub/deep.md";
         "CHANGELOG.md";
         "license.md";
         "CONTRIBUTING.md";
         "Code_Of_Conduct.md";
         "AGENTS.md";
         "src/a.mli";
         "docs/img.png";
         "../x.md";
         "/abs.md";
       ]);
  Alcotest.check pair "interface fallback when no markdown beyond README"
    [
      ("README.md", "readme");
      ("crates/y/src/lib.rs", "api");
      ("index.d.ts", "api");
      ("lib/x.ml", "api");
      ("pkg/mod.pyi", "api");
      ("src/a.mli", "api");
      ("src/lib.rs", "api");
    ]
    (sel
       [
         "README.md";
         "dune-project";
         "package.json";
         "Cargo.toml";
         "pyproject.toml";
         "src/a.mli";
         "test/t.mli";
         "_build/default/src/a.mli";
         "vendor/v.mli";
         "examples/e.mli";
         "tests/x.mli";
         "_opam/lib/o.mli";
         "index.d.ts";
         "pkg/mod.pyi";
         "src/lib.rs";
         "lib/x.ml";
         "crates/y/src/lib.rs";
       ]);
  Alcotest.check pair "no detected language: README only (FR-120)"
    [ ("README.md", "readme") ]
    (sel [ "README.md"; "main.cbl"; "copy.cpy"; "notes.txt" ]);
  let many = List.init 250 (fun i -> Printf.sprintf "docs/%03d.md" i) in
  Alcotest.(check int)
    "at most 200 files" 200
    (List.length (Doc_select.select ("README.md" :: many)))

(* ------------------------------------------------------------ CHECK-21 *)

let markdown_ingest () =
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let dir = Filename.concat dev "demo" in
  F.git_checkout dir
    [
      ("README.md", "# Demo\n\nA demo library for tests.\n");
      ("docs/guide.md", F.guide_md);
      ("CHANGELOG.md", "## 1.0\n\nInstalling notes.\n");
      ("LICENSE.md", "MIT, installing allowed\n");
      ("src/demo.mli", "val run : unit -> unit\n");
    ];
  let outside = Filename.concat dev "outside.md" in
  F.write_file outside "## Outside\n\nprivate text outside the checkout\n";
  Unix.symlink outside (Filename.concat dir "docs/link.md");
  F.git_add dir;
  let key = F.insert_repo ctx ~owner:"example-user" ~local_dir:dir "demo" in
  let s = F.ingest ctx (F.mixed_env (Fake_forge.create ())) in
  Alcotest.(check (list string)) "no gap" [] s.gaps;
  Alcotest.(check int) "read locally" 1 s.local;
  Alcotest.(check (list (triple string string string)))
    "selected documents (symlink never followed, exclusions, no .mli fallback)"
    [ ("README.md", "readme", "local"); ("docs/guide.md", "doc", "local") ]
    (F.docs ctx key);
  Alcotest.check strs "heading chunks" [ "Installing"; "Usage" ]
    (F.chunk_titles ctx key "docs/guide.md");
  F.docs_integrity ctx;
  let q args = ok_or_fail (Ops.query_docs ctx (`Assoc args)) in
  let r = q [ ("libraryId", `String key); ("query", `String "installing") ] in
  let top = match member "excerpts" r with `List (e :: _) -> e | _ -> `Null in
  Alcotest.(check string) "top title" "Installing" (to_str (member "title" top));
  Alcotest.(check string) "top path" "docs/guide.md" (to_str (member "path" top));
  Alcotest.(check string) "top kind" "doc" (to_str (member "kind" top));
  Alcotest.(check string) "top source" "local" (to_str (member "source" top));
  Alcotest.(check string)
    "excerpt uri" "libindex://doc/github/example-user/demo/docs%2Fguide.md"
    (to_str (member "uri" top));
  Alcotest.(check string) "library id" key (to_str (member "id" (member "library" r)));
  let r' =
    q [ ("libraryId", `String "/example-user/demo"); ("query", `String "installing") ]
  in
  Alcotest.(check bool)
    "/owner/name accepted" true
    (member "excerpts" r' = member "excerpts" r);
  let other =
    q [ ("libraryId", `String key); ("query", `String "configuration record") ]
  in
  Alcotest.(check string)
    "only that library, ranked" "Usage"
    (to_str
       (member "title"
          (match member "excerpts" other with `List (e :: _) -> e | _ -> `Null)));
  (match
     Ops.query_docs ctx (`Assoc [ ("libraryId", `String "nope"); ("query", `String "x") ])
   with
  | Error e ->
      Alcotest.(check bool)
        "unknown id names resolve-library-id" true
        (contains e "resolve-library-id")
  | Ok _ -> Alcotest.fail "unknown id accepted");
  (* FR-088: curated rules come first. *)
  ignore
    (ok_or_fail
       (Ops.annotate ctx
          (`Assoc
             [
               ("repo", `String key);
               ("fields", `Assoc [ ("rules", `String "Use Demo.run only.") ]);
             ])));
  let r = q [ ("libraryId", `String key); ("query", `String "installing") ] in
  (match r with
  | `Assoc ((k1, _) :: (k2, v) :: _) ->
      Alcotest.(check (pair string string))
        "rules right after library" ("library", "rules") (k1, k2);
      Alcotest.(check string) "rules text" "Use Demo.run only." (to_str v)
  | _ -> Alcotest.fail "unexpected shape");
  Alcotest.(check bool)
    "rules exported" true
    (contains
       (read_file (Filename.concat ctx.export_dir "curated.json"))
       "Use Demo.run only.");
  (* End to end: `refresh --repo` (same Ops path as the MCP tool) ingests the documents
     of a Tier-1 row, then query-docs finds them. *)
  let dir2 = Filename.concat dev "demo2" in
  F.git_checkout dir2 [ ("README.md", "# Demo2\n\nx\n"); ("docs/guide.md", F.guide_md) ];
  let key2 = F.insert_repo ctx ~owner:"example-user" ~tier:"2" ~local_dir:dir2 "demo2" in
  let forge = Fake_forge.create () in
  Fake_forge.json forge "repos/example-user/demo2"
    (Fake_forge.gh_repo "example-user/demo2");
  let o =
    ok_or_fail
      (Ops.refresh ctx (F.mixed_env forge) ~allow_full:false
         (`Assoc [ ("repo", `String key2) ]))
  in
  Alcotest.(check bool) "refresh complete" true o.complete;
  Alcotest.(check int)
    "owner row became tier 1, 2 documents written" 2
    (match member "written" (member "docs" o.json) with `Int n -> n | _ -> -1);
  let r = q [ ("libraryId", `String key2); ("query", `String "installing") ] in
  Alcotest.(check string)
    "query-docs after refresh" "Installing"
    (to_str
       (member "title"
          (match member "excerpts" r with `List (e :: _) -> e | _ -> `Null)))

(* ------------------------------------------------------------ CHECK-22 *)

let mli_fallback () =
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let lib = Filename.concat dev "itf" in
  F.git_checkout lib
    [
      ("README.md", "# Itf\n\nTrace replay.\n");
      ("src/itf.mli", F.itf_mli);
      ("test/helpers.mli", "val helper : int\n");
      ("vendor/dep/dep.mli", "val dep : int\n");
      ("src/impl.ml", "let x = 1\n");
    ];
  let key = F.insert_repo ctx ~owner:"example-user" ~local_dir:lib "quint-bridge" in
  let documented = Filename.concat dev "documented" in
  F.git_checkout documented
    [
      ("README.md", "# D\n\nx\n");
      ("docs/a.md", "## A\n\ntext\n");
      ("src/b.mli", "val b : int\n");
    ];
  let key2 = F.insert_repo ctx ~owner:"example-user" ~local_dir:documented "documented" in
  let s = F.ingest ctx (F.mixed_env (Fake_forge.create ())) in
  Alcotest.(check (list string)) "no gap" [] s.gaps;
  Alcotest.(check (list (triple string string string)))
    "README + .mli fallback (+ .ml without .mli), test/ and vendor/ excluded"
    [
      ("README.md", "readme", "local");
      ("src/impl.ml", "api", "local");
      ("src/itf.mli", "api", "local");
    ]
    (F.docs ctx key);
  Alcotest.check strs "one chunk per item, titled kind qualified_path (FR-120)"
    [
      "module Itf";
      "val Itf.parse_file";
      "type Itf.t";
      "module Itf.Sub";
      "exception Itf.Bad";
      "external Itf.fast";
      "val Itf.( >>= )";
      "type Itf.box";
    ]
    (F.chunk_titles ctx key "src/itf.mli");
  Alcotest.(check bool)
    "doc comment carried" true
    (contains
       (F.chunk_text ctx key "src/itf.mli" "val Itf.parse_file")
       "Parse an ITF trace file");
  Alcotest.(check (list (triple string string string)))
    "no fallback when markdown docs exist"
    [ ("README.md", "readme", "local"); ("docs/a.md", "doc", "local") ]
    (F.docs ctx key2);
  let r =
    ok_or_fail
      (Ops.query_docs ctx
         (`Assoc
            [
              ("libraryId", `String key);
              ("query", `String "parse trace file");
              ("kind", `String "api");
            ]))
  in
  let top = match member "excerpts" r with `List (e :: _) -> e | _ -> `Null in
  Alcotest.(check string)
    "item found by its doc" "val Itf.parse_file"
    (to_str (member "title" top))

(* ------------------------------------------------------------ CHECK-23 *)

let doc_count (ctx : Ctx.t) key = List.length (F.docs ctx key)

let chunk_ids (ctx : Ctx.t) key =
  Db.query ctx.db
    {|SELECT c.id FROM doc_chunks c JOIN documents d ON d.id = c.doc_id
JOIN repos r ON r.id = d.repo_id WHERE r.key = ? ORDER BY c.id|}
    [ Db.text key ]
    (fun r -> Option.value (Db.to_int_opt r.(0)) ~default:0)

let gating () =
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let mk name =
    let d = Filename.concat dev name in
    F.git_checkout d
      [ ("README.md", "# " ^ name ^ "\n\nhello\n"); ("docs/x.md", "## X\n\nx\n") ];
    d
  in
  let t1 = F.insert_repo ctx ~owner:"example-user" ~local_dir:(mk "one") "one" in
  let t2 = F.insert_repo ctx ~owner:"someorg" ~tier:"2" ~local_dir:(mk "two") "two" in
  let camp =
    F.insert_repo ctx ~owner:"acme-campaigns" ~campaign:true ~local_dir:(mk "camp") "camp"
  in
  let env = F.mixed_env (Fake_forge.create ()) in
  let s = F.ingest ctx env in
  Alcotest.(check int) "only the Tier-1 non-campaign row is eligible" 1 s.repos;
  Alcotest.(check int) "tier 1 → documents" 2 (doc_count ctx t1);
  Alcotest.(check int) "tier 2 → none" 0 (doc_count ctx t2);
  Alcotest.(check int) "campaign → none" 0 (doc_count ctx camp);
  (* Incremental: unchanged content is not re-chunked. *)
  let ids = chunk_ids ctx t1 in
  let s = F.ingest ctx env in
  Alcotest.(check (pair int int))
    "second run: 0 written, 2 unchanged" (0, 2) (s.written, s.unchanged);
  Alcotest.(check (list int)) "chunks untouched" ids (chunk_ids ctx t1);
  F.write_file (Filename.concat dev "one/docs/x.md") "## X\n\nchanged\n";
  let s = F.ingest ctx env in
  Alcotest.(check (pair int int)) "changed file re-chunked" (1, 1) (s.written, s.unchanged);
  (* A row that becomes a campaign row, or leaves tier 1, loses its documents (EC-15). *)
  Db.run ctx.db "UPDATE repos SET campaign = 1 WHERE key = ?" [ Db.text t1 ];
  let s = F.ingest ctx env in
  Alcotest.(check int) "campaign purge" 0 (doc_count ctx t1);
  Alcotest.(check int) "purge counted" 2 s.removed;
  Db.run ctx.db "UPDATE repos SET campaign = 0, tier = '1' WHERE key = ?" [ Db.text t2 ];
  ignore (F.ingest ctx env);
  Alcotest.(check int) "promoted to tier 1 → ingested" 2 (doc_count ctx t2);
  Db.run ctx.db "UPDATE repos SET tier = '2' WHERE key = ?" [ Db.text t2 ];
  ignore (F.ingest ctx env);
  Alcotest.(check int) "demoted → purged" 0 (doc_count ctx t2);
  (* EC-15 through the refresh write path itself. *)
  Db.run ctx.db "UPDATE repos SET campaign = 0 WHERE key = ?" [ Db.text t1 ];
  ignore (F.ingest ctx env);
  Alcotest.(check int) "restored" 2 (doc_count ctx t1);
  ignore (Doc_store.purge_key ctx.db t1);
  Alcotest.(check int) "purge_key" 0 (doc_count ctx t1);
  F.docs_integrity ctx

(* ------------------------------------------------------------ CHECK-26 *)

let api_docs () =
  let ctx = empty_ctx () in
  let fn = "example-user/remote" in
  let key = F.insert_repo ctx ~owner:"example-user" "remote" in
  let forge = Fake_forge.create () in
  let entries =
    [
      ("README.md", "r1"); ("docs/a.md", "a1"); ("LICENSE.md", "l1"); ("src/x.mli", "x1");
    ]
  in
  Fake_forge.json forge (F.tree_path fn) (F.gh_tree entries);
  Fake_forge.ok forge (F.file_path fn "README.md") "# Remote\n\nFrom the API.\n";
  let big = String.concat "" (List.init 30_000 (fun _ -> "## Big\n\nsome text ")) in
  Fake_forge.ok forge (F.file_path fn "docs/a.md") big;
  Fake_forge.ok forge (F.file_path fn "src/x.mli") "(** The x. *)\nval x : int\n";
  let env = Fake_forge.env forge in
  let s = F.ingest ctx env in
  Alcotest.(check (list string)) "no gap" [] s.gaps;
  (* S13: the interface file is scanned for API items although the documents are
     markdown (no api document). *)
  Alcotest.(check int) "scanned API file" 1 s.scanned;
  Alcotest.(check (list (triple string string string)))
    "API documents (LICENSE excluded, no fallback)"
    [ ("README.md", "readme", "api"); ("docs/a.md", "doc", "api") ]
    (F.docs ctx key);
  List.iter Fake_forge.assert_get (Fake_forge.calls forge);
  Alcotest.(check bool)
    "raw media type" true
    (List.exists
       (fun a -> Array.mem "Accept: application/vnd.github.raw+json" a)
       (Fake_forge.calls forge));
  let stored =
    Option.join
      (Db.query_one ctx.db
         "SELECT length(d.text) FROM documents d JOIN repos r ON r.id = d.repo_id WHERE \
          r.key = ? AND d.path = 'docs/a.md'"
         [ Db.text key ]
         (fun r -> Db.to_int_opt r.(0)))
  in
  Alcotest.(check bool)
    "256 KiB cap" true
    (match stored with Some n -> n <= Doc_store.file_cap | None -> false);
  let marked =
    Db.query_one ctx.db "SELECT docs_pushed_at FROM repos WHERE key = ?"
      [ Db.text key ]
      (fun r -> Db.to_string_opt r.(0))
  in
  Alcotest.(check (option (option string)))
    "docs_pushed_at stamped" (Some (Some "2026-10-01T00:00:00Z")) marked;
  (* Not due: same pushed_at → no tree call. *)
  Fake_forge.reset_log forge;
  let s = F.ingest ctx env in
  Alcotest.(check int) "not due" 1 s.not_due;
  Alcotest.(check bool) "no tree call" false (Fake_forge.called forge (F.tree_path fn));
  (* New push: tree again, unchanged blobs not refetched. *)
  Db.run ctx.db "UPDATE repos SET pushed_at = '2026-10-02T00:00:00Z' WHERE key = ?"
    [ Db.text key ];
  Fake_forge.reset_log forge;
  ignore (F.ingest ctx env);
  Alcotest.(check bool) "tree listed" true (Fake_forge.called forge (F.tree_path fn));
  Alcotest.(check int) "unchanged blobs not fetched" 0 (F.file_calls forge);
  (* --full refetches. *)
  Fake_forge.reset_log forge;
  ignore (F.ingest ~full:true ctx env);
  Alcotest.(check int)
    "full refetches (2 documents, 1 scanned file)" 3 (F.file_calls forge);
  (* A failed fetch keeps the previous document, records a gap, does not stamp. *)
  Fake_forge.json forge (F.tree_path fn)
    (F.gh_tree [ ("README.md", "r2"); ("docs/a.md", "a1") ]);
  Fake_forge.fail forge (F.file_path fn "README.md") `Network;
  Db.run ctx.db "UPDATE repos SET pushed_at = '2026-10-03T00:00:00Z' WHERE key = ?"
    [ Db.text key ];
  let s = F.ingest ctx env in
  Alcotest.(check (list string))
    "classified gap"
    [ key ^ ": docs docs-file failed (network); previous documents kept" ]
    s.gaps;
  Alcotest.(check int) "previous README kept" 2 (doc_count ctx key);
  let marked =
    Db.query_one ctx.db "SELECT docs_pushed_at FROM repos WHERE key = ?"
      [ Db.text key ]
      (fun r -> Db.to_string_opt r.(0))
  in
  Alcotest.(check (option (option string)))
    "not stamped" (Some (Some "2026-10-02T00:00:00Z")) marked;
  (* Fetch cap: 100 docs → 60 fetched this run, the rest on the next one. *)
  let ctx = empty_ctx () in
  let fn = "example-user/many" in
  let key = F.insert_repo ctx ~owner:"example-user" "many" in
  let forge = Fake_forge.create () in
  let names = List.init 100 (fun i -> Printf.sprintf "docs/%03d.md" i) in
  Fake_forge.json forge (F.tree_path fn)
    (F.gh_tree (List.map (fun p -> (p, "b" ^ p)) names));
  List.iter
    (fun p -> Fake_forge.ok forge (F.file_path fn p) ("## " ^ p ^ "\n\ntext\n"))
    names;
  let env = Fake_forge.env forge in
  ignore (F.ingest ctx env);
  Alcotest.(check int) "60 file fetches" 60 (F.file_calls forge);
  Alcotest.(check int) "60 stored" 60 (doc_count ctx key);
  Fake_forge.reset_log forge;
  ignore (F.ingest ctx env);
  Alcotest.(check int) "remaining 40 next run" 40 (F.file_calls forge);
  Alcotest.(check int) "100 stored" 100 (doc_count ctx key);
  (* GitLab: paginated tree, files API with an encoded path and ref. *)
  let ctx = empty_ctx () in
  let key = F.insert_repo ctx ~host:"gitlab" ~gl_id:42 ~owner:"acme" "glib" in
  let forge = Fake_forge.create () in
  Fake_forge.ok forge "projects/42/repository/tree?recursive=true&per_page=100"
    {|[{"path":"README.md","type":"blob","id":"r"}][{"path":"docs/guide.md","type":"blob","id":"g"}]|};
  Fake_forge.ok forge "projects/42/repository/files/README%2Emd/raw?ref=main"
    "# G\n\nhi\n";
  Fake_forge.ok forge "projects/42/repository/files/docs%2Fguide%2Emd/raw?ref=main"
    F.guide_md;
  ignore (F.ingest ctx (Fake_forge.env forge));
  List.iter Fake_forge.assert_get (Fake_forge.calls forge);
  Alcotest.(check (list (triple string string string)))
    "GitLab documents"
    [ ("README.md", "readme", "api"); ("docs/guide.md", "doc", "api") ]
    (F.docs ctx key);
  F.docs_integrity ctx

let () =
  run "docs"
    [
      ( "chunking",
        [
          Alcotest.test_case "markdown" `Quick markdown_chunks;
          Alcotest.test_case "mli" `Quick mli_chunks;
          Alcotest.test_case "selection" `Quick selection;
        ] );
      ("markdown-ingest", [ Alcotest.test_case "AC-21" `Quick markdown_ingest ]);
      ("mli-fallback", [ Alcotest.test_case "AC-22" `Quick mli_fallback ]);
      ("gating", [ Alcotest.test_case "AC-23" `Quick gating ]);
      ("api-docs", [ Alcotest.test_case "AC-26" `Quick api_docs ]);
    ]
