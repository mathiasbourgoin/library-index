(* Review round 3 hardening, part 1 (CHECK-38, CHECK-40): registration classifies with the
   whole configuration (campaign prefixes of every source, every remote of a checkout,
   configured local checkouts) and keeps the stored flags of a re-registered source;
   unregistration never deletes a row it did not create or that someone curated, and keeps
   the DB and sources.toml consistent. Names are fictional; gh/glab are the fake forge, git
   is the real read-only binary on temp checkouts. Part 2: test_hardening_misc. *)

open Library_index
open Test_support.Support
open Test_support.Hardening_fixture
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

(* ------------------------------------------------------------ HIGH 1: identity *)

let source_identity () =
  let base = Identity.of_config config in
  let src : Config.source =
    {
      kind = Local_root;
      name = "/tmp/x";
      host = None;
      own = false;
      probe = false;
      include_subgroups = true;
      confidential = Not_confidential;
      campaign_prefix = false;
    }
  in
  let id = Identity.for_source base src in
  Alcotest.(check (list string)) "listing: this root only" [ "/tmp/x" ] id.local_roots;
  Alcotest.(check bool) "no other listing" true (id.gh_user = None && id.gl_groups = []);
  Alcotest.(check (list string))
    "campaign prefixes of the whole configuration" base.campaign_prefixes
    id.campaign_prefixes;
  Alcotest.(check bool) "author kept" true (id.author_regex = base.author_regex)

let register_source_campaign () =
  let ctx = data_ctx () in
  let dev = campaign_checkout () in
  let forge = Fake_forge.create () in
  let o =
    ok_or_fail
      (Ops.register_source ctx (F.mixed_env forge)
         (`Assoc [ ("kind", `String "local-root"); ("name", `String dev) ]))
  in
  ignore o;
  let key = "local:secret-campaign" in
  Alcotest.(check (option string))
    "campaign flag" (Some "1")
    (scalar ctx "SELECT campaign FROM repos WHERE key = ?" key);
  Alcotest.(check (option string))
    "no README" None
    (scalar ctx "SELECT readme FROM repos WHERE key = ?" key);
  Alcotest.(check bool)
    "get withholds" false
    (contains (Yojson.Safe.to_string (get ctx key)) sentinel);
  Alcotest.(check (option string))
    "no document" (Some "0")
    (scalar ctx
       "SELECT count(*) FROM documents WHERE repo_id IN (SELECT id FROM repos WHERE key \
        = ?)"
       key)

(* register-repo of a checkout whose campaign remote is not its origin: the campaign rule
   reads every remote of the checkout, as a full refresh does. *)
let register_repo_campaign_remote () =
  let ctx = data_ctx () in
  let dev = temp_dir () in
  let dir = Filename.concat dev "side-project" in
  F.git_checkout dir [ ("README.md", "# Side\n\n" ^ sentinel ^ " notes.\n") ];
  commit dir ~author:"Ada Example";
  List.iter
    (fun (n, u) ->
      ignore
        (Sys.command
           (Printf.sprintf "git -C %s remote add %s %s" (Filename.quote dir) n u)))
    [
      ("origin", "/srv/git/side-project.git");
      ("up", "git@gitlab.com:acme-campaigns/side-project.git");
    ];
  let o =
    ok_or_fail
      (Ops.register_repo ctx
         (F.mixed_env (Fake_forge.create ()))
         (`Assoc [ ("target", `String dir) ]))
  in
  let key = to_str (member "key" o.json) in
  Alcotest.(check (option string))
    "campaign flag" (Some "1")
    (scalar ctx "SELECT campaign FROM repos WHERE key = ?" key);
  Alcotest.(check bool)
    "nothing of the checkout stored" false
    (contains (Yojson.Safe.to_string (get ctx key)) sentinel)

(* Registering a known row with campaign=true withholds it at once, even when the fetch
   fails: no document, no README, description withheld. *)
let register_campaign_flag_immediate () =
  let ctx = data_ctx () in
  let key = F.insert_repo ctx ~owner:"outside-dev" ~what:"x" "known" in
  Db.run ctx.db "UPDATE repos SET description = ?, readme = ? WHERE key = ?"
    [ Db.text sentinel; Db.text sentinel; Db.text key ];
  F.add_doc ctx key "docs/guide.md" ("# Guide\n\n" ^ sentinel ^ " details.\n");
  let forge = Fake_forge.create () in
  Fake_forge.fail forge "repos/outside-dev/known" `Network;
  ignore
    (Ops.register_repo ctx (F.mixed_env forge)
       (`Assoc
          [
            ("target", `String "https://github.com/outside-dev/known");
            ("campaign", `Bool true);
          ]));
  Alcotest.(check bool)
    "nothing served" false
    (contains (Yojson.Safe.to_string (get ctx key)) sentinel);
  Alcotest.(check (option string))
    "documents purged" (Some "0")
    (scalar ctx
       "SELECT count(*) FROM documents WHERE repo_id IN (SELECT id FROM repos WHERE key \
        = ?)"
       key)

(* A forge repository listed by register-source is a campaign repository when a local
   checkout of it (under a configured local root) has a campaign remote, as in a full
   refresh; and a row already flagged campaign stays so. *)
let register_source_checkout_remote () =
  let dev = temp_dir () in
  let dir = Filename.concat dev "fork-tool" in
  F.git_checkout dir [ ("README.md", "# fork\n") ];
  commit dir ~author:"Ada Example";
  List.iter
    (fun (n, u) ->
      ignore
        (Sys.command
           (Printf.sprintf "git -C %s remote add %s %s" (Filename.quote dir) n u)))
    [
      ("origin", "git@github.com:gadget-guild/fork-tool.git");
      ("up", "git@gitlab.com:acme-campaigns/fork-tool.git");
    ];
  let config =
    read_file (fixture "sources.test.toml")
    |> Re.replace_string
         (Re.compile (Re.str "name = \"~/work\""))
         ~by:(Printf.sprintf "name = %S" dev)
  in
  let ctx = data_ctx ~config () in
  let forge = Fake_forge.create () in
  let fn = "gadget-guild/fork-tool" in
  Fake_forge.json forge "orgs/gadget-guild" (`Assoc [ ("login", `String "gadget-guild") ]);
  Fake_forge.json forge "orgs/gadget-guild/repos?type=all&per_page=100"
    (`List [ Fake_forge.gh_repo ~description:sentinel fn ]);
  Fake_forge.ok forge ("repos/" ^ fn ^ "/readme") ("# t\n\n" ^ sentinel ^ "\n");
  ignore
    (ok_or_fail
       (Ops.register_source ctx (F.mixed_env forge)
          (`Assoc [ ("kind", `String "github-org"); ("name", `String "gadget-guild") ])));
  let key = "github:" ^ fn in
  Alcotest.(check (option string))
    "campaign through the checkout's remote" (Some "1")
    (scalar ctx "SELECT campaign FROM repos WHERE key = ?" key);
  Alcotest.(check bool)
    "nothing stored" false
    (contains (Yojson.Safe.to_string (get ctx key)) sentinel);
  Alcotest.(check bool)
    "local path recorded" true
    (contains (Yojson.Safe.to_string (get ctx key)) dir);
  (* register-repo of the forge URL alone reaches the same decision. *)
  let ctx = data_ctx ~config () in
  Fake_forge.json forge ("repos/" ^ fn) (Fake_forge.gh_repo ~description:sentinel fn);
  ignore
    (ok_or_fail
       (Ops.register_repo ctx (F.mixed_env forge)
          (`Assoc [ ("target", `String ("https://github.com/" ^ fn)) ])));
  Alcotest.(check (option string))
    "register-repo: campaign" (Some "1")
    (scalar ctx "SELECT campaign FROM repos WHERE key = ?" key);
  Alcotest.(check bool)
    "register-repo: nothing stored" false
    (contains (Yojson.Safe.to_string (get ctx key)) sentinel)

(* ------------------------------------------------------------ HIGH 2: source merge *)

let campaign_source_config =
  "[owner]\n\
   author_regex = \"example\"\n\n\
   [[source]]\n\
   kind = \"github-org\"\n\
   name = \"acme-campaigns\"\n\
   # owner note: never publish anything of this org\n\
   confidential = \"generic\"\n\
   campaign_prefix = true\n"

let source_merge () =
  let ctx = data_ctx ~config:campaign_source_config () in
  let forge = Fake_forge.create () in
  let fn = "acme-campaigns/target" in
  Fake_forge.json forge "orgs/acme-campaigns"
    (`Assoc [ ("login", `String "acme-campaigns") ]);
  Fake_forge.json forge "orgs/acme-campaigns/repos?type=all&per_page=100"
    (`List [ Fake_forge.gh_repo ~description:sentinel fn ]);
  Fake_forge.ok forge ("repos/" ^ fn ^ "/readme") ("# t\n\n" ^ sentinel ^ "\n");
  ignore
    (ok_or_fail
       (Ops.register_source ctx (F.mixed_env forge)
          (`Assoc [ ("kind", `String "github-org"); ("name", `String "acme-campaigns") ])));
  let text = config_text ctx in
  Alcotest.(check bool)
    "campaign_prefix kept" true
    (contains text "campaign_prefix = true");
  Alcotest.(check bool)
    "confidential kept" true
    (contains text "confidential = \"generic\"");
  Alcotest.(check bool) "comment kept" true (contains text "# owner note");
  let j = Yojson.Safe.to_string (get ctx ("github:" ^ fn)) in
  Alcotest.(check bool) "nothing of the campaign stored" false (contains j sentinel);
  (* An explicit value still changes a flag. *)
  ignore
    (ok_or_fail
       (Ops.register_source ctx (F.mixed_env forge)
          (`Assoc
             [
               ("kind", `String "github-org");
               ("name", `String "acme-campaigns");
               ("probe", `Bool true);
             ])));
  let text = config_text ctx in
  Alcotest.(check bool) "probe set" true (contains text "probe = true");
  Alcotest.(check bool)
    "still a campaign prefix" true
    (contains text "campaign_prefix = true")

let local_root_missing () =
  let ctx = data_ctx () in
  let before = config_text ctx in
  match
    Ops.register_source ctx
      (F.mixed_env (Fake_forge.create ()))
      (`Assoc [ ("kind", `String "local-root"); ("name", `String "/nonexistent/dir/x") ])
  with
  | Ok _ -> Alcotest.fail "a missing local root was registered"
  | Error e ->
      Alcotest.(check bool) "actionable" true (contains e "not an existing directory");
      Alcotest.(check string) "config unchanged" before (config_text ctx)

let source_deadline () =
  let ctx = data_ctx () in
  let forge = Fake_forge.create () in
  Fake_forge.json forge "orgs/gadget-guild" (`Assoc [ ("login", `String "gadget-guild") ]);
  match
    Source_reg.register_source ctx (F.mixed_env forge)
      ~deadline:(Unix.gettimeofday () -. 1.)
      (`Assoc [ ("kind", `String "github-org"); ("name", `String "gadget-guild") ])
  with
  | Error e -> Alcotest.failf "unexpected error: %s" e
  | Ok o ->
      Alcotest.(check bool) "partial" false o.complete;
      Alcotest.(check bool)
        "continue hint" true
        (List.exists (fun g -> contains g "library-index refresh") o.gaps)

(* ------------------------------------------------------------ HIGH 3: unregister *)

let curated_lib =
  {|{"github:example-user/lib": {"what": "A lib", "interest": "high",
  "notes": "precious notes", "consumers": ["app"]}}|}

let unregister_keeps_curated () =
  let dir = temp_dir () in
  let src = Filename.concat dir "src" in
  Unix.mkdir src 0o755;
  F.write_file (Filename.concat src "curated.json") curated_lib;
  F.write_file (Filename.concat src "whitehat_tools.json") {|{"tools": []}|};
  let db_path = Filename.concat dir "t.db" in
  ignore (ok_or_fail (Init.init ~id ~db_path ~from_dir:src ~force:false ()));
  let ctx = open_ctx db_path in
  copy_file (fixture "sources.test.toml") (Filename.concat dir Config.file_name);
  let forge = Fake_forge.create () in
  Fake_forge.json forge "repos/example-user/lib" (Fake_forge.gh_repo "example-user/lib");
  let target = "https://github.com/example-user/lib" in
  ignore
    (ok_or_fail
       (Ops.register_repo ctx (F.mixed_env forge) (`Assoc [ ("target", `String target) ])));
  let r = ok_or_fail (Ops.unregister_repo ctx (`Assoc [ ("target", `String target) ])) in
  Alcotest.(check string) "kept" "kept" (to_str (member "row" r));
  let exported = read_file (Filename.concat dir "curated.json") in
  Alcotest.(check bool)
    "curated.json keeps the notes" true
    (contains exported "precious notes");
  Alcotest.(check bool)
    "not registered" true
    (scalar ctx "SELECT registered FROM repos WHERE key = ?" "github:example-user/lib"
    = Some "0");
  Db.exec ctx.db "INSERT INTO api_items_fts(api_items_fts) VALUES('integrity-check')";
  fts_integrity ctx

let unregister_created_but_curated () =
  let ctx = data_ctx () in
  let forge = Fake_forge.create () in
  let fn = "outside-dev/gadget" in
  Fake_forge.json forge ("repos/" ^ fn) (Fake_forge.gh_repo fn);
  let target = "https://github.com/" ^ fn in
  ignore
    (ok_or_fail
       (Ops.register_repo ctx (F.mixed_env forge)
          (`Assoc [ ("target", `String target); ("notes", `String "keep me") ])));
  let r = ok_or_fail (Ops.unregister_repo ctx (`Assoc [ ("target", `String target) ])) in
  Alcotest.(check string) "curated row kept" "kept" (to_str (member "row" r));
  Alcotest.(check (option string))
    "notes kept" (Some "keep me")
    (scalar ctx "SELECT notes FROM repos WHERE key = ?" ("github:" ^ fn));
  (* A registration-created row with usage only: kept too. *)
  let fn2 = "outside-dev/used" in
  Fake_forge.json forge ("repos/" ^ fn2) (Fake_forge.gh_repo fn2);
  let t2 = "https://github.com/" ^ fn2 in
  ignore
    (ok_or_fail
       (Ops.register_repo ctx (F.mixed_env forge) (`Assoc [ ("target", `String t2) ])));
  ignore
    (ok_or_fail
       (Ops.record_usage ctx
          (`Assoc [ ("repo", `String ("github:" ^ fn2)); ("consumer", `String "dash") ])));
  let r2 = ok_or_fail (Ops.unregister_repo ctx (`Assoc [ ("target", `String t2) ])) in
  Alcotest.(check string) "used row kept" "kept" (to_str (member "row" r2))

(* A failed sources.toml write leaves the DB unchanged (rolled back). The write is made to
   fail by a directory occupying the editor's temporary file name (works as root too). *)
let unregister_consistent () =
  let ctx = data_ctx () in
  let forge = Fake_forge.create () in
  let fn = "outside-dev/solo" in
  Fake_forge.json forge ("repos/" ^ fn) (Fake_forge.gh_repo fn);
  let target = "https://github.com/" ^ fn in
  ignore
    (ok_or_fail
       (Ops.register_repo ctx (F.mixed_env forge) (`Assoc [ ("target", `String target) ])));
  let before = config_text ctx in
  let blocker =
    Filename.concat ctx.data_dir
      (Printf.sprintf ".%s.tmp.%d" Config.file_name (Unix.getpid ()))
  in
  Unix.mkdir blocker 0o700;
  let r =
    Fun.protect
      ~finally:(fun () -> Unix.rmdir blocker)
      (fun () -> Ops.unregister_repo ctx (`Assoc [ ("target", `String target) ]))
  in
  Alcotest.(check bool) "error reported" true (Result.is_error r);
  Alcotest.(check string) "config unchanged" before (config_text ctx);
  Alcotest.(check (option string))
    "row kept and still registered" (Some "1")
    (scalar ctx "SELECT registered FROM repos WHERE key = ?" ("github:" ^ fn));
  (* Without the blocker the same call succeeds and deletes the registration-only row. *)
  let r = ok_or_fail (Ops.unregister_repo ctx (`Assoc [ ("target", `String target) ])) in
  Alcotest.(check string) "deleted" "deleted" (to_str (member "row" r))

let migration_v3 () =
  let ctx = empty_ctx () in
  let path = ctx.db_path in
  Db.exec ctx.db "ALTER TABLE repos DROP COLUMN created_by_registration";
  Db.set_meta ctx.db "schema_version" "3";
  Db.close ctx.db;
  let db = ok_or_fail (Db.open_existing path) in
  Alcotest.(check (option string))
    "version 4" (Some "4")
    (Db.get_meta db "schema_version");
  Db.exec db "SELECT created_by_registration FROM repos";
  Db.close db

let () =
  run "hardening"
    [
      ( "register-source",
        [
          Alcotest.test_case "identity for one source" `Quick source_identity;
          Alcotest.test_case "campaign local root" `Quick register_source_campaign;
          Alcotest.test_case "register-repo campaign remote" `Quick
            register_repo_campaign_remote;
          Alcotest.test_case "campaign registration immediate" `Quick
            register_campaign_flag_immediate;
          Alcotest.test_case "register-source checkout remote" `Quick
            register_source_checkout_remote;
          Alcotest.test_case "flags merged, comment kept" `Quick source_merge;
          Alcotest.test_case "missing local root" `Quick local_root_missing;
          Alcotest.test_case "deadline" `Quick source_deadline;
        ] );
      ( "unregister",
        [
          Alcotest.test_case "pre-curated row kept" `Quick unregister_keeps_curated;
          Alcotest.test_case "curated registered row kept" `Quick
            unregister_created_but_curated;
          Alcotest.test_case "DB and config consistent" `Quick unregister_consistent;
          Alcotest.test_case "schema 3 to 4" `Quick migration_v3;
        ] );
    ]
