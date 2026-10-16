(* Curation: CHECK-11 (AC-11) — record_usage, annotate, validation, no write on error,
   exports updated atomically, export_dirty retry (AM-10). Temp dirs only. *)

open Library_index
open Test_support.Support

let contains s sub = Re.execp (Re.compile (Re.str sub)) s

let annotate ctx repo fields =
  Ops.annotate ctx (`Assoc [ ("repo", `String repo); ("fields", `Assoc fields) ])

let usage ?note ctx repo consumer =
  Ops.record_usage ctx
    (`Assoc
       ([ ("repo", `String repo); ("consumer", `String consumer) ]
       @ match note with Some n -> [ ("note", `String n) ] | None -> []))

let exported (ctx : Ctx.t) file =
  ok_or_fail (Curated_io.read_json (Filename.concat ctx.export_dir file))

let curated_entry ctx key = member key (exported ctx Export.curated_file)
let get ctx k = ok_or_fail (Repo_view.get ctx k)

let usage_rows (ctx : Ctx.t) =
  Db.query ctx.db "SELECT count(*) FROM usage" [] (fun r -> Db.to_int_opt r.(0))

(* Everything a write could touch: the repos rows, usage, and the export dir listing. *)
let snapshot (ctx : Ctx.t) =
  let rows =
    Db.query ctx.db
      "SELECT key || '|' || coalesce(what,'') || coalesce(interest,'') || \
       coalesce(domain,'') || coalesce(consumers,'') || coalesce(status_override,'') || \
       coalesce(maintained_side,'') || coalesce(notes,'') || coalesce(extra,'') FROM \
       repos ORDER BY key"
      [] (fun r -> Option.value ~default:"" (Db.to_string_opt r.(0)))
  in
  let files = List.sort compare (Array.to_list (Sys.readdir ctx.export_dir)) in
  (rows, usage_rows ctx, files)

let expect_error_no_write ctx label f =
  let before = snapshot ctx in
  match f () with
  | Ok _ -> Alcotest.failf "%s: accepted" label
  | Error e ->
      Alcotest.(check bool) (label ^ ": nothing written") true (snapshot ctx = before);
      e

let record_usage () =
  let ctx = fresh_ctx () in
  let k = "github:example-org/acme-mcp" in
  let j = ok_or_fail (usage ctx k "library-index" ~note:"MCP server") in
  Alcotest.(check string)
    "consumers (set append)" {|["notes-app","library-index"]|}
    (Yojson.Safe.to_string (member "consumers" j));
  Alcotest.(check string) "one usage row" "1" (to_str (member "usage_count" j));
  let e = curated_entry ctx k in
  Alcotest.(check string)
    "curated.json consumers" {|["notes-app","library-index"]|}
    (Yojson.Safe.to_string (member "consumers" e));
  (match member "usage" e with
  | `List [ u ] ->
      Alcotest.(check string)
        "usage consumer" "library-index"
        (to_str (member "consumer" u));
      Alcotest.(check string) "usage note" "MCP server" (to_str (member "note" u));
      Alcotest.(check string)
        "usage at = injected clock" "2026-10-03T19:58:30Z"
        (to_str (member "at" u))
  | _ -> Alcotest.fail "curated.json usage missing");
  Alcotest.(check bool)
    "other fields untouched" true
    (to_str (member "domain" e) = "OCaml libraries" && member "what" e <> `Null);
  (* EC-17: a duplicate call appends history; consumers stays a set. *)
  let j = ok_or_fail (usage ctx k "library-index" ~note:"MCP server") in
  Alcotest.(check string) "second row appended" "2" (to_str (member "usage_count" j));
  Alcotest.(check string)
    "consumers unchanged" {|["notes-app","library-index"]|}
    (Yojson.Safe.to_string (member "consumers" j));
  (* A stub with no consumers gets a fresh list; get shows the usage. *)
  ignore (ok_or_fail (usage ctx "sample-lib" "x-project"));
  Alcotest.(check string)
    "get usage" "x-project"
    (match member "usage" (get ctx "local:sample-lib") with
    | `List (u :: _) -> to_str (member "consumer" u)
    | _ -> "");
  (* US-5.3 and validation: nothing written. *)
  let e = expect_error_no_write ctx "ambiguous" (fun () -> usage ctx "acme-mcp" "y") in
  Alcotest.(check bool) "ambiguous lists keys" true (contains e "gitlab:acme/acme-mcp");
  ignore (expect_error_no_write ctx "unknown" (fun () -> usage ctx "nope" "y"));
  ignore (expect_error_no_write ctx "empty consumer" (fun () -> usage ctx k "  "));
  ignore
    (expect_error_no_write ctx "long note" (fun () ->
         usage ctx k "y" ~note:(String.make 4001 'n')));
  ignore
    (expect_error_no_write ctx "missing consumer" (fun () ->
         Ops.record_usage ctx (`Assoc [ ("repo", `String k) ])));
  ignore
    (expect_error_no_write ctx "whitehat row" (fun () -> usage ctx "proof-checker" "y"));
  (* Redaction on this write path. *)
  let token = "glpat-" ^ String.make 24 'Z' in
  ignore (ok_or_fail (usage ctx k ("svc " ^ token) ~note:("note " ^ token)));
  Alcotest.(check bool)
    "no token in curated.json" false
    (contains (read_file (Filename.concat ctx.export_dir Export.curated_file)) token);
  fts_integrity ctx

let field_json ctx key f = Yojson.Safe.to_string (member f (get ctx key))

let annotate_fields () =
  let ctx = fresh_ctx () in
  let k = "local:sample-lib" in
  let before = get ctx k in
  let j =
    ok_or_fail
      (annotate ctx k [ ("status_override", `String "active"); ("notes", `String "x") ])
  in
  Alcotest.(check string)
    "updated list" {|["status_override","notes"]|}
    (Yojson.Safe.to_string (member "updated" j));
  let after = get ctx k in
  let changed =
    match (before, after) with
    | `Assoc b, `Assoc a ->
        List.filter_map
          (fun (f, v) ->
            if Yojson.Safe.equal v (member f (`Assoc b)) then None else Some f)
          a
    | _ -> []
  in
  Alcotest.(check (list string))
    "exactly those fields changed (plus effective status and provenance)"
    [ "notes"; "provenance"; "status"; "status_override" ]
    (List.sort compare changed);
  List.iter
    (fun f ->
      Alcotest.(check string)
        ("annotated " ^ f ^ " is curated (FR-112)")
        "curated"
        (to_str (member f (member "provenance" after))))
    [ "status_override"; "notes" ];
  Alcotest.(check string)
    "effective status = override" "active"
    (to_str (member "status" after));
  let e = curated_entry ctx k in
  Alcotest.(check string) "exported notes" "x" (to_str (member "notes" e));
  Alcotest.(check string)
    "exported override" "active"
    (to_str (member "status_override" e));
  (* null clears; export omits the key. *)
  ignore (ok_or_fail (annotate ctx k [ ("notes", `Null); ("domain", `Null) ]));
  Alcotest.(check string) "cleared in DB" "null" (field_json ctx k "notes");
  (match curated_entry ctx k with
  | `Assoc m ->
      Alcotest.(check bool)
        "export omits cleared keys" false
        (List.mem_assoc "notes" m || List.mem_assoc "domain" m)
  | _ -> Alcotest.fail "entry missing");
  (* Canonicalisation of closed values; consumers replace + dedup. *)
  ignore
    (ok_or_fail
       (annotate ctx k
          [
            ("domain", `String "ocaml LIBRARIES");
            ("consumers", `List [ `String "a"; `String "b"; `String "a" ]);
          ]));
  Alcotest.(check string)
    "domain canonical" {|"OCaml libraries"|} (field_json ctx k "domain");
  Alcotest.(check string)
    "consumers replaced, dedup" {|["a","b"]|}
    (field_json ctx k "consumers");
  (* 4000 code points accepted (multi-byte), 4001 rejected. *)
  let long n = String.concat "" (List.init n (fun _ -> "é")) in
  ignore (ok_or_fail (annotate ctx k [ ("what", `String (long 4000)) ]));
  ignore
    (expect_error_no_write ctx "4001 chars" (fun () ->
         annotate ctx k [ ("what", `String (long 4001)) ]));
  (* Search sees the new curated text (FTS triggers). *)
  ignore (ok_or_fail (annotate ctx k [ ("notes", `String "zebrafishword") ]));
  Alcotest.(check string)
    "search finds annotation" {|["local:sample-lib"]|}
    (Yojson.Safe.to_string
       (`List
          (match
             member "hits"
               (ok_or_fail
                  (Ops.search ctx (`Assoc [ ("query", `String "zebrafishword") ])))
           with
          | `List l -> List.map (member "key") l
          | _ -> [])));
  fts_integrity ctx

let annotate_rejects () =
  let ctx = fresh_ctx () in
  let k = "local:sample-lib" in
  let e =
    expect_error_no_write ctx "url" (fun () ->
        annotate ctx k [ ("url", `String "https://x") ])
  in
  Alcotest.(check string)
    "lists allowed fields"
    "field 'url' cannot be annotated; allowed fields: what, interest, domain, consumers, \
     status_override, maintained_side, notes, rules"
    e;
  let e =
    expect_error_no_write ctx "mixed valid + invalid" (fun () ->
        annotate ctx k [ ("notes", `String "ok"); ("status_override", `String "great") ])
  in
  Alcotest.(check string)
    "lists allowed values"
    "invalid status_override 'great'; allowed values: active, dormant, stale, archived, \
     unknown"
    e;
  Alcotest.(check string)
    "EC-18" "no fields to annotate"
    (expect_error_no_write ctx "empty" (fun () -> annotate ctx k []));
  ignore
    (expect_error_no_write ctx "bad domain" (fun () ->
         annotate ctx k [ ("domain", `String "Gardening") ]));
  ignore
    (expect_error_no_write ctx "consumers not array" (fun () ->
         annotate ctx k [ ("consumers", `String "a") ]));
  ignore
    (expect_error_no_write ctx "consumers non-string" (fun () ->
         annotate ctx k [ ("consumers", `List [ `Int 1 ]) ]));
  ignore
    (expect_error_no_write ctx "number value" (fun () ->
         annotate ctx k [ ("notes", `Int 3) ]));
  ignore
    (expect_error_no_write ctx "duplicate field" (fun () ->
         annotate ctx k [ ("notes", `String "a"); ("notes", `String "b") ]));
  ignore
    (expect_error_no_write ctx "unknown repo" (fun () ->
         annotate ctx "nope" [ ("notes", `String "a") ]));
  ignore
    (expect_error_no_write ctx "ambiguous repo" (fun () ->
         annotate ctx "acme-mcp" [ ("notes", `String "a") ]));
  ignore
    (expect_error_no_write ctx "fields missing" (fun () ->
         Ops.annotate ctx (`Assoc [ ("repo", `String k) ])));
  ignore
    (expect_error_no_write ctx "whitehat domain" (fun () ->
         annotate ctx "proof-checker" [ ("domain", `String "Other") ]))

let exports () =
  let ctx = fresh_ctx () in
  (* A non-whitehat write rewrites curated.json only. *)
  ignore (ok_or_fail (annotate ctx "sample-lib" [ ("notes", `String "n") ]));
  let files () = List.sort compare (Array.to_list (Sys.readdir ctx.export_dir)) in
  Alcotest.(check (list string))
    "curated only, no temp leftovers"
    [ ".export.lock"; "curated.json"; "t.db" ]
    (List.filter (fun f -> not (contains f "t.db-")) (files ()));
  (* A whitehat write rewrites whitehat_tools.json with reuse = interest, order kept. *)
  ignore (ok_or_fail (annotate ctx "proof-checker" [ ("interest", `String "reusable") ]));
  (match member "tools" (exported ctx Export.whitehat_file) with
  | `List (t :: _ as l) ->
      Alcotest.(check int) "6 tools" 6 (List.length l);
      Alcotest.(check string)
        "first tool kept first" "proof-checker/"
        (to_str (member "path" t));
      Alcotest.(check string) "reuse updated" "reusable" (to_str (member "reuse" t))
  | _ -> Alcotest.fail "whitehat export missing");
  (* AM-10: export failure → error, DB updated, dirty flag; next write retries both files. *)
  let broken = { ctx with export_dir = Filename.concat ctx.export_dir "missing-dir" } in
  (match annotate broken "sample-lib" [ ("notes", `String "while-broken") ] with
  | Ok _ -> Alcotest.fail "export into a missing dir succeeded"
  | Error e ->
      Alcotest.(check bool)
        "DB updated, export failed" true
        (contains e "DB updated, export failed"));
  Alcotest.(check string)
    "DB was updated" {|"while-broken"|}
    (field_json ctx "local:sample-lib" "notes");
  Alcotest.(check bool) "dirty" true (Db.export_dirty ctx.db);
  Alcotest.(check bool)
    "get flags export_dirty" true
    (member "export_dirty" (get ctx "sample-lib") = `Bool true);
  Unix.mkdir broken.export_dir 0o755;
  ignore (ok_or_fail (usage broken "sample-lib" "retry"));
  Alcotest.(check bool) "dirty cleared" false (Db.export_dirty ctx.db);
  List.iter
    (fun f ->
      Alcotest.(check bool)
        ("retry wrote " ^ f) true
        (Sys.file_exists (Filename.concat broken.export_dir f)))
    [ Export.curated_file; Export.whitehat_file ];
  Alcotest.(check string)
    "retried export carries the earlier write" "while-broken"
    (to_str (member "notes" (curated_entry broken "local:sample-lib")));
  (* The full export still equals the inputs on every untouched key. *)
  let original =
    ok_or_fail (Curated_io.read_json (Filename.concat fixtures_dir "curated.json"))
  in
  let now = exported broken Export.curated_file in
  (match original with
  | `Assoc m ->
      List.iter
        (fun (key, v) ->
          if key <> "local:sample-lib" then
            Alcotest.(check bool)
              (key ^ " unchanged") true
              (Yojson.Safe.equal v (member key now)))
        m
  | _ -> ());
  Alcotest.(check bool)
    "lock file present" true
    (Sys.file_exists (Filename.concat broken.export_dir ".export.lock"))

(* C-23: exports are serialised by lockf on .export.lock. A forked child holds the lock
   for 400 ms; an export started meanwhile must wait for it. *)
let export_lock () =
  let ctx = fresh_ctx () in
  let lock = Filename.concat ctx.export_dir ".export.lock" in
  let r, w = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
      let fd = Unix.openfile lock [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
      Unix.lockf fd Unix.F_LOCK 0;
      ignore (Unix.write_substring w "x" 0 1);
      Unix.sleepf 0.4;
      Unix._exit 0
  | pid ->
      let buf = Bytes.create 1 in
      ignore (Unix.read r buf 0 1);
      let t0 = Unix.gettimeofday () in
      ok_or_fail (Export.export ctx ~dir:ctx.export_dir);
      let waited = Unix.gettimeofday () -. t0 in
      ignore (Unix.waitpid [] pid);
      Unix.close r;
      Unix.close w;
      Alcotest.(check bool)
        (Printf.sprintf "export waited for the lock (%.2fs)" waited)
        true (waited >= 0.3)

(* Review round 1: the export snapshot is read under the lock. A child holds the lock,
   then commits a newer curated value through its own connection while the parent's
   export waits; the export must contain it (a snapshot taken before the lock would not,
   and would overwrite the newer file once the lock is released). *)
let export_snapshot () =
  let ctx = fresh_ctx () in
  let lock = Filename.concat ctx.export_dir Export.lock_file in
  let r, w = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
      let fd = Unix.openfile lock [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
      Unix.lockf fd Unix.F_LOCK 0;
      ignore (Unix.write_substring w "x" 0 1);
      Unix.sleepf 0.3;
      let db = Db.create ctx.db_path in
      Db.run db "UPDATE repos SET notes = 'written-while-waiting' WHERE key = ?"
        [ Db.text "local:sample-lib" ];
      Db.close db;
      Unix._exit 0
  | pid ->
      let buf = Bytes.create 1 in
      ignore (Unix.read r buf 0 1);
      ok_or_fail (Export.export ctx ~dir:ctx.export_dir);
      ignore (Unix.waitpid [] pid);
      Unix.close r;
      Unix.close w;
      Alcotest.(check string)
        "export has the commit made while it waited" "written-while-waiting"
        (to_str
           (member "notes" (member "local:sample-lib" (exported ctx Export.curated_file))));
      Alcotest.(check bool) "not dirty" false (Db.export_dirty ctx.db)

let () =
  run "curate"
    [
      ( "record-usage",
        [ Alcotest.test_case "append, set semantics, errors" `Quick record_usage ] );
      ( "annotate",
        [ Alcotest.test_case "set, clear, canonicalise, cap" `Quick annotate_fields ] );
      ( "annotate-rejects",
        [ Alcotest.test_case "validation, no write on error" `Quick annotate_rejects ] );
      ( "exports",
        [ Alcotest.test_case "atomic exports, export_dirty retry" `Quick exports ] );
      ("export-lock", [ Alcotest.test_case "lockf serialises exports" `Quick export_lock ]);
      ( "export-snapshot",
        [ Alcotest.test_case "snapshot read under the lock" `Quick export_snapshot ] );
    ]
