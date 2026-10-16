(* Unit cases of the minimal-freeze invariants (spec "Minimal-freeze": INV-C, INV-K, INV-M,
   INV-D) and of the round-3 MEDIUM/LOW fixes. The operation-sequence harnesses
   (checks/confidentiality-closure.js, curated-no-loss.js, migration-concurrency.js,
   single-campaign-decision.js) cover the classes; these cases pin each mechanism. Names
   are fictional. *)

open Library_index
open Test_support.Support
open Test_support.Hardening_fixture
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

let count ctx sql key = Option.value ~default:"?" (scalar ctx sql key)

(* ------------------------------------------------------------ INV-C *)

(* Review round 3 HIGH tool/lib/source_reg.ml:294: a campaign prefix registered after a
   row was stored (its source's listing fails) reclassifies the stored row at once. *)
let prefix_after_rows () =
  let ctx = data_ctx () in
  let dev = temp_dir () in
  let dir = Filename.concat dev "later-secret" in
  F.git_checkout dir [ ("README.md", "# Later\n\nThis holds " ^ sentinel ^ " notes.\n") ];
  commit dir ~author:"Ada Example";
  ignore
    (Sys.command
       ("git -C " ^ Filename.quote dir
      ^ " remote add origin git@gitlab.com:acme-later/later-secret.git"));
  let forge = Fake_forge.create () in
  let env = F.mixed_env forge in
  ignore
    (ok_or_fail
       (Ops.register_source ctx env
          (`Assoc [ ("kind", `String "local-root"); ("name", `String dev) ])));
  let key = "local:later-secret" in
  Alcotest.(check bool)
    "authentic: README served before" true
    (contains (Yojson.Safe.to_string (get ctx key)) sentinel);
  Import_raw.replace_gaps ctx.db [ key ^ ": readme failed (network); previous data kept" ];
  (* The group's listing fails (404): only the configuration change can reclassify. *)
  ignore
    (Ops.register_source ctx env
       (`Assoc
          [
            ("kind", `String "gitlab-group");
            ("name", `String "acme-later");
            ("campaign_prefix", `Bool true);
          ]));
  Alcotest.(check (option string))
    "campaign now" (Some "1")
    (scalar ctx "SELECT campaign FROM repos WHERE key = ?" key);
  Alcotest.(check (option string))
    "README dropped" None
    (scalar ctx "SELECT readme FROM repos WHERE key = ?" key);
  Alcotest.(check (option string))
    "synopsis dropped" None
    (scalar ctx "SELECT synopsis FROM repos WHERE key = ?" key);
  Alcotest.(check bool)
    "get withholds" false
    (contains (Yojson.Safe.to_string (get ctx key)) sentinel);
  Alcotest.(check string)
    "documents purged" "0"
    (count ctx
       "SELECT count(*) FROM documents WHERE repo_id IN (SELECT id FROM repos WHERE key \
        = ?)"
       key);
  Alcotest.(check string)
    "gap masked" "0"
    (count ctx "SELECT count(*) FROM gaps WHERE text LIKE '%' || ? || '%'" "later-secret")

let upsert ctx row = ignore (ok_or_fail (Import_raw.upsert_fetched ~id ctx.Ctx.db row))

(* Stored gap lines are masked by whole token: a row's key, full name and name. *)
let gap_masking () =
  let ctx = empty_ctx () in
  upsert ctx
    (`Assoc
       [
         ("key", `String "local:camp-one");
         ("host", `String "local");
         ("owner", `String "(local)");
         ("name", `String "camp-one");
         ("full_name", `String "camp-one");
       ]);
  Import_raw.replace_gaps ctx.db
    [
      "local:camp-one: readme failed (network); previous data kept";
      "camp-one: last-commit failed (network)";
      "camp-ones: readme failed (network)";
    ];
  Db.with_txn ctx.db (fun () -> Ok (Campaign.mark ctx.db id "local:camp-one"))
  |> ok_or_fail;
  let gaps =
    Db.query ctx.db "SELECT text FROM gaps ORDER BY ord" [] (fun r ->
        Db.to_string_opt r.(0))
  in
  Alcotest.(check (list (option string)))
    "masked by token"
    [
      Some "a campaign repository: readme failed (network); previous data kept";
      Some "a campaign repository: last-commit failed (network)";
      Some "camp-ones: readme failed (network)";
    ]
    gaps;
  (* A line written later is masked on write. *)
  Import_raw.replace_gaps ctx.db [ "camp-one: readme failed" ];
  Alcotest.(check (list (option string)))
    "masked on write"
    [ Some "a campaign repository: readme failed" ]
    (Db.query ctx.db "SELECT text FROM gaps" [] (fun r -> Db.to_string_opt r.(0)));
  Alcotest.(check string)
    "gap name of a stored campaign row" "a campaign repository"
    (Campaign.gap_name ctx.db ~id "local:camp-one");
  Alcotest.(check string)
    "gap name of another row" "local:other"
    (Campaign.gap_name ctx.db ~id "local:other")

(* A gap stored before its row existed is masked when a fetch stores the row as campaign
   (found by checks/confidentiality-closure.js, seed-10). *)
let gap_before_row () =
  let ctx = empty_ctx () in
  Import_raw.replace_gaps ctx.db [ "delta-kit: readme failed (network)" ];
  Db.with_txn ctx.db (fun () ->
      Ok
        (upsert ctx
           (`Assoc
              [
                ("key", `String "github:shadow-org/delta-kit");
                ("host", `String "github");
                ("owner", `String "shadow-org");
                ("name", `String "delta-kit");
                ("full_name", `String "shadow-org/delta-kit");
                ("campaign", `Bool true);
              ])))
  |> ok_or_fail;
  Alcotest.(check (list (option string)))
    "masked"
    [ Some "a campaign repository: readme failed (network)" ]
    (Db.query ctx.db "SELECT text FROM gaps" [] (fun r -> Db.to_string_opt r.(0)))

(* ------------------------------------------------------------ INV-K *)

let key = "github:example-collab/bench-tool"

let set_dirty_values (ctx : Ctx.t) =
  Db.run ctx.db
    "UPDATE repos SET what = 'db-newer-what', notes = 'db-only-notes' WHERE key = ?"
    [ Db.text key ];
  Db.set_meta ctx.db "export_dirty" "1"

let import_dirty () =
  let ctx = fresh_ctx () in
  set_dirty_values ctx;
  (match Init.import ~id ctx.db ~from_dir:fixtures_dir () with
  | Ok _ -> Alcotest.fail "a dirty import was accepted"
  | Error e ->
      Alcotest.(check bool) "names --prefer-file" true (contains e "--prefer-file");
      Alcotest.(check bool) "no destructive advice" false (contains e "init --force"));
  Alcotest.(check (option string))
    "DB value kept" (Some "db-newer-what")
    (scalar ctx "SELECT what FROM repos WHERE key = ?" key);
  ignore (ok_or_fail (Init.import ~id ctx.db ~from_dir:fixtures_dir ~prefer_file:true ()));
  Alcotest.(check (option string))
    "the file wins" (Some "Benchmark runner that compares OCaml executables")
    (scalar ctx "SELECT what FROM repos WHERE key = ?" key);
  Alcotest.(check (option string))
    "a field the file lacks is kept" (Some "db-only-notes")
    (scalar ctx "SELECT notes FROM repos WHERE key = ?" key)

let init_force_dirty () =
  let ctx = fresh_ctx () in
  set_dirty_values ctx;
  Db.close ctx.db;
  let init ?discard_unexported () =
    Init.init ~id ~db_path:ctx.db_path ~from_dir:fixtures_dir ~force:true
      ?discard_unexported ()
  in
  (match init () with
  | Ok _ -> Alcotest.fail "init --force deleted unexported curated data"
  | Error e ->
      Alcotest.(check bool) "names the override" true (contains e "--discard-unexported");
      Alcotest.(check bool) "no destructive advice" false (contains e "init --force"));
  let db = ok_or_fail (Db.open_existing ctx.db_path) in
  Alcotest.(check bool) "still dirty" true (Db.export_dirty db);
  Db.close db;
  ignore (ok_or_fail (init ~discard_unexported:true ()));
  let db = ok_or_fail (Db.open_existing ctx.db_path) in
  Alcotest.(check bool) "recreated clean" false (Db.export_dirty db);
  Db.close db

(* ------------------------------------------------------------ INV-M *)

let v3 () =
  let ctx = fresh_ctx () in
  Db.run ctx.db "UPDATE repos SET notes = 'kept' WHERE key = ?" [ Db.text key ];
  Db.exec ctx.db "ALTER TABLE repos DROP COLUMN created_by_registration";
  Db.set_meta ctx.db "schema_version" "3";
  Db.close ctx.db;
  ctx.db_path

let migration_semantics () =
  let path = v3 () in
  let db = ok_or_fail (Db.open_existing path) in
  Alcotest.(check (option string))
    "version 4" (Some "4")
    (Db.get_meta db "schema_version");
  let row sql =
    Option.join (Db.query_one db sql [ Db.text key ] (fun r -> Db.to_string_opt r.(0)))
  in
  Alcotest.(check (option string))
    "pre-existing row not registration-created" (Some "0")
    (row "SELECT created_by_registration FROM repos WHERE key = ?");
  Alcotest.(check (option string))
    "curated value intact" (Some "kept")
    (row "SELECT notes FROM repos WHERE key = ?");
  Db.close db;
  (* A second open finds version 4 and migrates nothing. *)
  Db.close (ok_or_fail (Db.open_existing path))

(* A migrator that meets a held write lock reports busy/retry after the busy timeout, never
   a foreign file or init --force; the version it read outside the lock is not trusted. *)
let migration_busy () =
  let path = v3 () in
  let holder = Sqlite3.db_open path in
  Db.exec holder "BEGIN IMMEDIATE";
  let r = Db.open_existing path in
  Db.exec holder "ROLLBACK";
  ignore (Sqlite3.db_close holder);
  (match r with
  | Ok _ -> Alcotest.fail "migrated under a held lock"
  | Error e ->
      Alcotest.(check bool) "busy" true (contains e "busy");
      Alcotest.(check bool) "retry" true (contains e "retry");
      Alcotest.(check bool) "no destructive advice" false (contains e "init --force"));
  Db.close (ok_or_fail (Db.open_existing path))

let foreign_dirty () =
  let ctx = fresh_ctx () in
  Db.set_meta ctx.db "schema_version" "2";
  Db.close ctx.db;
  (match Db.open_existing ctx.db_path with
  | Ok _ -> Alcotest.fail "opened a schema-2 file"
  | Error e ->
      Alcotest.(check bool) "advises init --force" true (contains e "init --force"));
  let db = Sqlite3.db_open ctx.db_path in
  Db.set_meta db "export_dirty" "1";
  ignore (Sqlite3.db_close db);
  match Db.open_existing ctx.db_path with
  | Ok _ -> Alcotest.fail "opened a schema-2 file"
  | Error e ->
      Alcotest.(check bool)
        "never advises init --force while dirty" false (contains e "init --force")

(* ------------------------------------------------------------ round-3 MEDIUM / LOW *)

(* db.ml:75: a failing COMMIT (a deferred foreign key) is rolled back, not left open. *)
let commit_failure () =
  let dir = temp_dir () in
  let db = Db.create (Filename.concat dir "c.db") in
  Db.exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
  Db.exec db "CREATE TABLE c (pid INTEGER REFERENCES p(id) DEFERRABLE INITIALLY DEFERRED)";
  (match
     Db.with_txn db (fun () ->
         Db.exec db "INSERT INTO c(pid) VALUES (42)";
         Ok ())
   with
  | Ok () -> Alcotest.fail "the violating commit succeeded"
  | Error () -> Alcotest.fail "unexpected Error"
  | exception Db.Db_error _ -> ());
  Alcotest.(check bool)
    "no transaction left open" true
    (Sqlite3.exec db "BEGIN" = Sqlite3.Rc.OK);
  Db.exec db "ROLLBACK";
  Alcotest.(check (option string))
    "nothing written" (Some "0")
    (Option.join
       (Db.query_one db "SELECT count(*) FROM c" [] (fun r -> Db.to_string_opt r.(0))));
  Db.close db

(* db.ml:69: a read-only data directory is a classified error, not an exception. *)
let readonly_dir () =
  if Unix.geteuid () = 0 then Alcotest.skip ()
  else
    let ctx = empty_ctx () in
    Db.close ctx.db;
    Unix.chmod ctx.data_dir 0o500;
    let r =
      Fun.protect
        ~finally:(fun () -> Unix.chmod ctx.data_dir 0o700)
        (fun () -> Db.open_existing ctx.db_path)
    in
    match r with
    | Ok db ->
        (* A platform where WAL needs no new file: the open is fine. *)
        Db.close db
    | Error e -> Alcotest.(check bool) "classified" true (contains e "READONLY")

(* config_edit.ml:250: a dangling symlink is written through, never replaced. *)
let dangling_symlink () =
  let dir = temp_dir () in
  let real = Filename.concat dir "real" in
  Unix.mkdir real 0o700;
  let link = Filename.concat dir "sources.toml" in
  Unix.symlink "real/sources.toml" link;
  ignore (ok_or_fail (Config_edit.write_atomic link "[owner]\n"));
  Alcotest.(check bool) "link kept" true ((Unix.lstat link).st_kind = Unix.S_LNK);
  Alcotest.(check string)
    "target written" "[owner]\n"
    (read_file (Filename.concat real "sources.toml"));
  Alcotest.(check int) "0600" 0o600 (Unix.stat link).st_perm

(* target.ml:79: a refused target never echoes a query-string or userinfo credential. *)
let target_echo () =
  let call _ = Error `Not_found in
  List.iter
    (fun t ->
      match Target.parse ~id ~call ~home:None t with
      | Ok _ -> Alcotest.failf "accepted %s" t
      | Error e ->
          Alcotest.(check bool) ("no credential: " ^ e) false (contains e "SEKRIT"))
    [
      "/nonexistent-dir?access_token=SEKRIT1";
      "/nonexistent-dir#private_token=SEKRIT2";
      "https://gitlab.com/solo?private_token=SEKRIT3";
      "ftp://user:SEKRIT4@example.org/x";
    ]

(* comment_db.ml:173: arch-index items are deleted through Api_store, scan items kept. *)
let api_delete_source () =
  let ctx = empty_ctx () in
  upsert ctx
    (`Assoc
       [
         ("key", `String "local:lib-a");
         ("host", `String "local");
         ("owner", `String "(local)");
         ("name", `String "lib-a");
         ("full_name", `String "lib-a");
       ]);
  let repo_id =
    match
      Db.query_one ctx.db "SELECT id FROM repos WHERE key = 'local:lib-a'" [] (fun r ->
          Db.to_int_opt r.(0))
    with
    | Some (Some i) -> i
    | _ -> Alcotest.fail "no row"
  in
  let item name : Api_store.row =
    {
      module_path = "A";
      name;
      qualified_path = "A." ^ name;
      kind = "value";
      signature = None;
      doc = None;
      file = "a.mli";
      line = None;
    }
  in
  ignore (Api_store.insert ctx.db ~repo_id ~source:"arch-index" (item "x"));
  ignore (Api_store.insert ctx.db ~repo_id ~source:"scan" (item "y"));
  Alcotest.(check int)
    "one deleted" 1
    (Api_store.delete_source ctx.db ~repo_id ~source:"arch-index");
  Alcotest.(check (option string))
    "scan item kept" (Some "1")
    (Option.join
       (Db.query_one ctx.db "SELECT count(*) FROM api_items" [] (fun r ->
            Db.to_string_opt r.(0))))

let () =
  run "freeze"
    [
      ( "INV-C",
        [
          Alcotest.test_case "prefix after rows" `Quick prefix_after_rows;
          Alcotest.test_case "gap masking" `Quick gap_masking;
          Alcotest.test_case "gap before row" `Quick gap_before_row;
        ] );
      ( "INV-K",
        [
          Alcotest.test_case "dirty import" `Quick import_dirty;
          Alcotest.test_case "dirty init --force" `Quick init_force_dirty;
        ] );
      ( "INV-M",
        [
          Alcotest.test_case "migration semantics" `Quick migration_semantics;
          Alcotest.test_case "migration busy" `Slow migration_busy;
          Alcotest.test_case "foreign and dirty" `Quick foreign_dirty;
        ] );
      ( "round-3",
        [
          Alcotest.test_case "commit failure" `Quick commit_failure;
          Alcotest.test_case "read-only dir" `Quick readonly_dir;
          Alcotest.test_case "dangling symlink" `Quick dangling_symlink;
          Alcotest.test_case "target echo" `Quick target_echo;
          Alcotest.test_case "api delete_source" `Quick api_delete_source;
        ] );
    ]
