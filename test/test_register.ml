(* Registration (CHECK-32, AC-32) and ownership / unregistration (CHECK-33, AC-33), plus
   the sources.toml entry editor. Every data directory is a temp copy holding the
   synthetic test configuration; gh/glab are the fake forge, git is the real (read-only)
   binary on temp checkouts. Names are fictional. *)

open Library_index
open Test_support.Support
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

let strs = Alcotest.(list string)
let contains s sub = Re.execp (Re.compile (Re.str sub)) s

(* An empty index whose data directory holds the synthetic sources.toml. *)
let data_ctx () =
  let ctx = empty_ctx () in
  copy_file (fixture "sources.test.toml") (Filename.concat ctx.data_dir Config.file_name);
  ctx

let config_text (ctx : Ctx.t) = read_file (Filename.concat ctx.data_dir Config.file_name)
let count_sub s sub = List.length (Re.all (Re.compile (Re.str sub)) s)
let get ctx key = ok_or_fail (Ops.get ctx (`Assoc [ ("repo", `String key) ]))

let row_count (ctx : Ctx.t) table key =
  Option.value ~default:(-1)
    (Option.join
       (Db.query_one ctx.db
          (Printf.sprintf
             "SELECT count(*) FROM %s WHERE repo_id IN (SELECT id FROM repos WHERE key = \
              ?)"
             table)
          [ Db.text key ]
          (fun r -> Db.to_int_opt r.(0))))

let register ctx env args = ok_or_fail (Ops.register_repo ctx env (`Assoc args))

(* A GitHub repository with a README, a docs page and a package manifest. *)
let widget_forge ?(forge = Fake_forge.create ()) () =
  let fn = "outside-dev/widget-lib" in
  Fake_forge.json forge ("repos/" ^ fn)
    (Fake_forge.gh_repo ~description:"Widgets for terminal apps" ~language:"TypeScript" fn);
  Fake_forge.ok forge
    ("repos/" ^ fn ^ "/readme")
    "# widget-lib\n\nWidgets for terminal apps.\n";
  Fake_forge.json forge (F.tree_path fn)
    (F.gh_tree [ ("README.md", "r1"); ("docs/usage.md", "u1"); ("package.json", "p1") ]);
  Fake_forge.ok forge (F.file_path fn "README.md")
    "# widget-lib\n\nWidgets for terminal apps.\n";
  Fake_forge.ok forge
    (F.file_path fn "docs/usage.md")
    "## Rendering a gauge\n\nCall drawGauge with a value between 0 and 1.\n";
  Fake_forge.ok forge (F.file_path fn "package.json") {|{"name": "@outside/widgets"}|};
  Fake_forge.json forge
    ("repos/" ^ fn ^ "/commits?per_page=1")
    (`List
       [
         `Assoc
           [
             ( "commit",
               `Assoc
                 [ ("committer", `Assoc [ ("date", `String "2026-09-30T10:00:00Z") ]) ] );
           ];
       ]);
  (forge, fn)

(* ------------------------------------------------------------ config editor *)

let preamble =
  "# Hand-written header comment\n\
   [owner]\n\
   display_name = \"Ada Example\"   # trailing comment kept\n\n\
   [[source]]\n\
   kind = \"github-org\"\n\
   name = \"example-org\"\n\n\
   # a comment that belongs to what follows\n\
   [classify]\n\
   domains = [\n\
  \  \"Libraries\",\n\
  \  \"Other\",\n\
   ]\n"

let repo target key : Config.repo =
  {
    target;
    key = Some key;
    ownership = Some External;
    own = false;
    confidential = Not_confidential;
    campaign = false;
    campaign_prefix = false;
    deep = false;
  }

let config_edit () =
  let pred key = function
    | `Repo (r : Config.repo) -> r.key = Some key
    | `Source _ -> false
  in
  let block = Config_edit.render_repo (repo "https://github.com/o/a" "github:o/a") in
  let t1, o1 =
    ok_or_fail (Config_edit.upsert preamble Repo ~pred:(pred "github:o/a") ~block)
  in
  Alcotest.(check bool) "created" true (o1 = Config_edit.Created);
  Alcotest.(check bool)
    "unrelated text byte-preserved (prefix)" true
    (String.starts_with ~prefix:preamble t1);
  let block2 =
    Config_edit.render_repo
      { (repo "https://github.com/o/a" "github:o/a") with deep = true }
  in
  let t2, o2 =
    ok_or_fail (Config_edit.upsert t1 Repo ~pred:(pred "github:o/a") ~block:block2)
  in
  Alcotest.(check bool) "updated" true (o2 = Config_edit.Updated);
  Alcotest.(check int) "one [[repo]] block" 1 (count_sub t2 "[[repo]]");
  Alcotest.(check bool) "new flag written" true (contains t2 "deep = true");
  Alcotest.(check bool)
    "unrelated text byte-preserved after update" true
    (String.starts_with ~prefix:preamble t2);
  (* A block in the middle is replaced in place; what follows it is untouched. *)
  let mid = t2 ^ "\n# tail comment\n[render]\ntitle = \"T\"\n" in
  let block3 = Config_edit.render_repo (repo "git@github.com:o/a.git" "github:o/a") in
  let t3, _ =
    ok_or_fail (Config_edit.upsert mid Repo ~pred:(pred "github:o/a") ~block:block3)
  in
  Alcotest.(check bool)
    "tail kept" true
    (String.ends_with ~suffix:"\n# tail comment\n[render]\ntitle = \"T\"\n" t3);
  (* Removing the appended entry restores the original text exactly. *)
  let back = ok_or_fail (Config_edit.remove t1 Repo ~pred:(pred "github:o/a")) in
  Alcotest.(check (option string)) "remove restores the text" (Some preamble) back;
  Alcotest.(check (option string))
    "remove of an unknown entry" None
    (ok_or_fail (Config_edit.remove preamble Repo ~pred:(pred "github:x/y")));
  (* The source block is found by parsing it, its comment line stays outside. *)
  let src_pred = function
    | `Source (s : Config.source) -> s.name = "example-org"
    | `Repo _ -> false
  in
  let t4 = Option.get (ok_or_fail (Config_edit.remove preamble Source ~pred:src_pred)) in
  Alcotest.(check bool) "source removed" false (contains t4 "example-org");
  Alcotest.(check bool)
    "following comment kept" true
    (contains t4 "# a comment that belongs");
  Alcotest.(check bool) "header comment kept" true (contains t4 "# Hand-written header");
  match Config_edit.upsert "[[repo]]\ncolour = 1\n" Repo ~pred:(pred "x") ~block with
  | Error e -> Alcotest.(check bool) "invalid file refused" true (contains e "invalid")
  | Ok _ -> Alcotest.fail "an invalid sources.toml was accepted"

(* ------------------------------------------------------------ CHECK-32 *)

let registration () =
  let ctx = data_ctx () in
  let before = config_text ctx in
  let forge, fn = widget_forge () in
  let env = F.mixed_env forge in
  let url = "https://github.com/" ^ fn in
  let o =
    register ctx env [ ("target", `String url); ("ownership", `String "external") ]
  in
  Alcotest.(check bool) "fetch complete" true o.complete;
  let key = "github:" ^ fn in
  Alcotest.(check string) "created" "created" (to_str (member "registered" o.json));
  Alcotest.(check string) "returned entry" key (to_str (member "key" o.json));
  Alcotest.(check string)
    "ownership override" "external"
    (to_str (member "ownership" o.json));
  Alcotest.(check bool)
    "registered flag" true
    (member "registered" (get ctx key) = `Bool true);
  Alcotest.(check string)
    "content fetched whatever the tier (last commit)" "2026-09-30T10:00:00Z"
    (to_str (member "last_commit" (get ctx key)));
  let text = config_text ctx in
  Alcotest.(check bool)
    "existing sources.toml byte-preserved" true
    (String.starts_with ~prefix:before text);
  Alcotest.(check bool) "[[repo]] appended" true (contains text ("key = \"" ^ key ^ "\""));
  (* Searchable at once: metadata, README and documents. *)
  let s =
    ok_or_fail (Ops.search ctx (`Assoc [ ("query", `String "widgets terminal") ]))
  in
  Alcotest.(check bool)
    "searchable" true
    (contains (Yojson.Safe.to_string (member "hits" s)) key);
  Alcotest.(check (list (triple string string string)))
    "documents of an external registered repo (any tier)"
    [ ("README.md", "readme", "api"); ("docs/usage.md", "doc", "api") ]
    (F.docs ctx key);
  let d =
    ok_or_fail
      (Ops.query_docs ctx
         (`Assoc [ ("libraryId", `String key); ("query", `String "gauge") ]))
  in
  Alcotest.(check bool) "query-docs" true (contains (Yojson.Safe.to_string d) "drawGauge");
  Alcotest.(check bool)
    "aliases from package.json" true
    (contains (Yojson.Safe.to_string (member "aliases" (get ctx key))) "@outside/widgets");
  List.iter Fake_forge.assert_get (Fake_forge.calls forge);
  (* Second call with new flags: an update, no duplicate entry, row or document. *)
  let o2 =
    register ctx env
      [
        ("target", `String ("git@github.com:" ^ fn ^ ".git"));
        ("notes", `String "Used for the status bar");
        ("deep", `Bool true);
      ]
  in
  Alcotest.(check string) "updated" "updated" (to_str (member "registered" o2.json));
  let text2 = config_text ctx in
  Alcotest.(check int) "still one [[repo]] entry" 1 (count_sub text2 "[[repo]]");
  Alcotest.(check bool)
    "absent flag kept" true
    (contains text2 "ownership = \"external\"");
  Alcotest.(check bool) "deep recorded" true (contains text2 "deep = true");
  (* S13: deep runs the analysis; a forge-only repository has no checkout. *)
  Alcotest.(check string)
    "deep analysis skipped" "skipped"
    (to_str (member "status" (member "deep" o2.json)));
  Alcotest.(check bool)
    "cause: no local checkout" true
    (contains (to_str (member "cause" (member "deep" o2.json))) "no local checkout");
  Alcotest.(check int) "one analysis row" 1 (row_count ctx "analysis" key);
  Alcotest.(check int)
    "one row" 1
    (Option.value ~default:0
       (Option.join
          (Db.query_one ctx.db "SELECT count(*) FROM repos WHERE key = ?"
             [ Db.text key ]
             (fun r -> Db.to_int_opt r.(0)))));
  Alcotest.(check int) "documents not duplicated" 2 (row_count ctx "documents" key);
  let g = get ctx key in
  Alcotest.(check string)
    "curated notes" "Used for the status bar"
    (to_str (member "notes" g));
  Alcotest.(check string)
    "notes curated" "curated"
    (to_str (member "notes" (member "provenance" g)));
  Alcotest.(check bool)
    "notes exported" true
    (contains (read_file (Filename.concat ctx.export_dir "curated.json")) "status bar");
  (* A 404 registers nothing. *)
  (match
     Ops.register_repo ctx env
       (`Assoc [ ("target", `String "https://github.com/nobody/none") ])
   with
  | Error e -> Alcotest.(check bool) "404 refused" true (contains e "not found")
  | Ok _ -> Alcotest.fail "a missing repository was registered");
  Alcotest.(check string) "config unchanged by the 404" text2 (config_text ctx);
  (* Invalid flag values are rejected before anything is written. *)
  (match
     Ops.register_repo ctx env
       (`Assoc [ ("target", `String url); ("ownership", `String "mine") ])
   with
  | Error e -> Alcotest.(check bool) "bad ownership" true (contains e "ownership")
  | Ok _ -> Alcotest.fail "invalid ownership accepted");
  Alcotest.(check string) "config unchanged by a bad flag" text2 (config_text ctx)

let local_targets () =
  let ctx = data_ctx () in
  let dev = temp_dir () in
  let plain = Filename.concat dev "gauge-kit" in
  F.git_checkout plain
    [
      ("README.md", "# gauge-kit\n\nGauges drawn with box characters, for dashboards.\n");
      ("dune-project", "(lang dune 3.0)\n(name gauge-kit)\n");
      ("gauge-kit.opam", "opam-version: \"2.0\"\n");
      ("src/gauge.mli", "(** Draw a gauge. *)\nval draw : float -> string\n");
    ];
  let forge = Fake_forge.create () in
  let env = F.mixed_env forge in
  let o = register ctx env [ ("target", `String plain); ("ownership", `String "own") ] in
  Alcotest.(check string) "local key" "local:gauge-kit" (to_str (member "key" o.json));
  Alcotest.(check (list (triple string string string)))
    "local documents"
    [ ("README.md", "readme", "local"); ("src/gauge.mli", "api", "local") ]
    (F.docs ctx "local:gauge-kit");
  Alcotest.check strs "api item" [ "val Gauge.draw" ]
    (F.chunk_titles ctx "local:gauge-kit" "src/gauge.mli");
  Alcotest.(check bool)
    "alias from the opam file" true
    (contains
       (Yojson.Safe.to_string (member "aliases" (get ctx "local:gauge-kit")))
       "gauge-kit");
  (* A checkout whose origin is a GitHub repository registers that repository, with
     documents read from the checkout. *)
  let cloned = Filename.concat dev "widget-checkout" in
  F.git_checkout cloned [ ("README.md", "# widget\n\nLocal copy.\n") ];
  ignore
    (Sys.command
       (Printf.sprintf
          "git -C %s remote add origin https://github.com/outside-dev/widget-lib.git"
          (Filename.quote cloned)));
  let forge2, _ = widget_forge () in
  let o2 = register ctx (F.mixed_env forge2) [ ("target", `String cloned) ] in
  Alcotest.(check string)
    "remote key" "github:outside-dev/widget-lib"
    (to_str (member "key" o2.json));
  Alcotest.(check (list (triple string string string)))
    "documents from the checkout"
    [ ("README.md", "readme", "local") ]
    (F.docs ctx "github:outside-dev/widget-lib");
  match
    Ops.register_repo ctx env
      (`Assoc [ ("target", `String (Filename.concat dev "nope")) ])
  with
  | Error e -> Alcotest.(check bool) "not a directory" true (contains e "neither")
  | Ok _ -> Alcotest.fail "a missing directory was registered"

let register_source () =
  let ctx = data_ctx () in
  let forge = Fake_forge.create () in
  Fake_forge.json forge "orgs/gadget-guild" (`Assoc [ ("login", `String "gadget-guild") ]);
  Fake_forge.json forge "orgs/gadget-guild/repos?type=all&per_page=100"
    (`List
       [
         Fake_forge.gh_repo "gadget-guild/sprocket";
         Fake_forge.gh_repo "gadget-guild/flywheel";
       ]);
  Fake_forge.json forge "repos/gadget-guild/sprocket"
    (Fake_forge.gh_repo "gadget-guild/sprocket");
  let progress = ref [] in
  let o =
    ok_or_fail
      (Ops.register_source ctx (F.mixed_env forge)
         ~progress:(fun p _ -> progress := p :: !progress)
         (`Assoc [ ("kind", `String "github-org"); ("name", `String "gadget-guild") ]))
  in
  Alcotest.(check int)
    "repos added" 2
    (match member "repos_added" o.json with `Int n -> n | _ -> -1);
  Alcotest.(check int)
    "repos updated" 0
    (match member "repos_updated" o.json with `Int n -> n | _ -> -1);
  Alcotest.(check bool) "progress reported" true (List.length !progress >= 3);
  Alcotest.(check bool)
    "[[source]] written" true
    (contains (config_text ctx) "name = \"gadget-guild\"");
  Alcotest.(check string)
    "listed row" "github:gadget-guild/flywheel"
    (to_str (member "key" (get ctx "gadget-guild/flywheel")));
  (* A listed row keeps the provenance labels other sources gave it. *)
  Db.run ctx.db "UPDATE repos SET sources = '[\"gh-affiliation\"]' WHERE key = ?"
    [ Db.text "github:gadget-guild/flywheel" ];
  (* Upsert: again, nothing duplicated, repos now updated. *)
  let o2 =
    ok_or_fail
      (Ops.register_source ctx (F.mixed_env forge)
         (`Assoc [ ("kind", `String "github-org"); ("name", `String "gadget-guild") ]))
  in
  Alcotest.(check int)
    "updated" 2
    (match member "repos_updated" o2.json with `Int n -> n | _ -> -1);
  Alcotest.(check int)
    "one source entry" 1
    (count_sub (config_text ctx) "name = \"gadget-guild\"");
  Alcotest.(check string)
    "labels merged" {|["gh-affiliation","gh-org:gadget-guild"]|}
    (Yojson.Safe.to_string (member "sources" (get ctx "github:gadget-guild/flywheel")));
  let l = ok_or_fail (Ops.list_sources ctx (`Assoc [])) in
  Alcotest.(check bool)
    "list-sources" true
    (contains (Yojson.Safe.to_string (member "sources" l)) "gadget-guild")

(* A full refresh refreshes a registered repository no source lists, keeps its
   registration, re-runs the pre-fill, and never marks it missing when its metadata
   cannot be read. *)
let full_refresh () =
  let ctx = data_ctx () in
  let forge, fn = widget_forge ~forge:(Fake_forge.quiet ()) () in
  let key = "github:" ^ fn in
  let env = F.mixed_env forge in
  ignore (register ctx env [ ("target", `String ("https://github.com/" ^ fn)) ]);
  Fake_forge.json forge ("repos/" ^ fn)
    (Fake_forge.gh_repo ~description:"Widgets, now with gauges" ~language:"TypeScript" fn);
  let full () =
    ok_or_fail
      (Ops.refresh ctx env ~dev_root:Fake_forge.no_dev_root ~allow_full:true (`Assoc []))
  in
  let o = full () in
  Alcotest.(check (list string)) "complete" [] o.gaps;
  let g = get ctx key in
  Alcotest.(check string)
    "metadata refreshed" "Widgets, now with gauges"
    (to_str (member "description" g));
  Alcotest.(check string)
    "auto what follows" "Widgets, now with gauges"
    (to_str (member "what" g));
  Alcotest.(check bool) "still registered" true (member "registered" g = `Bool true);
  Alcotest.(check bool) "not missing" true (member "missing_since" g = `Null);
  (* A hand edit of sources.toml is mirrored by the next full refresh. *)
  let path = Filename.concat ctx.data_dir Config.file_name in
  F.write_file path
    (Re.replace_string
       (Re.compile (Re.str "confidential = \"none\"\n"))
       ~by:"ownership = \"participant\"\nconfidential = \"none\"\n" (config_text ctx));
  ignore (full ());
  Alcotest.(check string)
    "hand-edited override mirrored" "participant"
    (to_str (member "ownership" (get ctx key)));
  Fake_forge.fail forge ("repos/" ^ fn) `Network;
  let o2 = full () in
  Alcotest.(check bool)
    "metadata failure is a gap" true
    (List.exists (fun l -> contains l "registered repository metadata failed") o2.gaps);
  Alcotest.(check bool)
    "never marked missing" true
    (member "missing_since" (get ctx key) = `Null)

(* Registration flags drive confidentiality: a campaign registration withholds the
   description and keeps no README or document; exclude-from-page keeps the entry out of
   the rendered views while it stays searchable. *)
let confidentiality () =
  let ctx = data_ctx () in
  let forge, fn = widget_forge () in
  let secret = "outside-dev/secret-tool" in
  Fake_forge.json forge ("repos/" ^ secret)
    (Fake_forge.gh_repo ~description:"Secret details" secret);
  Fake_forge.ok forge ("repos/" ^ secret ^ "/readme") "# secret\n\nSecret readme text.\n";
  Fake_forge.json forge (F.tree_path secret) (F.gh_tree [ ("README.md", "s1") ]);
  Fake_forge.ok forge (F.file_path secret "README.md") "# secret\n\nSecret readme text.\n";
  let env = F.mixed_env forge in
  ignore
    (register ctx env
       [ ("target", `String ("https://github.com/" ^ secret)); ("campaign", `Bool true) ]);
  let g = get ctx ("github:" ^ secret) in
  Alcotest.(check string)
    "description withheld" Classify.withheld_description
    (to_str (member "description" g));
  Alcotest.(check bool) "no README" true (member "readme" g = `Null);
  Alcotest.(check (list (triple string string string)))
    "no document" []
    (F.docs ctx ("github:" ^ secret));
  ignore
    (register ctx env
       [
         ("target", `String ("https://github.com/" ^ fn));
         ("ownership", `String "external");
         ("confidential", `String "exclude-from-page");
       ]);
  let dir = temp_dir () in
  let out = Filename.concat dir "INDEX.md" in
  ignore (render ctx ~out ~notes_dir:dir ~now:parity_now ~home:"/nonexistent-home");
  let md = read_file out in
  Alcotest.(check bool) "excluded from the page" false (contains md "widget-lib");
  Alcotest.(check bool) "campaign repo not named" false (contains md "secret-tool");
  let s = ok_or_fail (Ops.search ctx (`Assoc [ ("query", `String "widgets") ])) in
  Alcotest.(check bool)
    "still searchable" true
    (contains (Yojson.Safe.to_string (member "hits" s)) fn)

(* Progress through the MCP tool: a modern request with a token gets notifications. *)
let mcp_progress () =
  let ctx = data_ctx () in
  let forge, fn = widget_forge () in
  let server = ok_or_fail (Mcp_server.server ctx (F.mixed_env forge)) in
  let req =
    Yojson.Safe.from_string
      (Printf.sprintf
         {|{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"register-repo","arguments":{"target":"https://github.com/%s"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{},"progressToken":"reg-1"}}}|}
         fn)
  in
  let frames = Mcp_kit.Server.dispatch_json_frames server req in
  let progress =
    List.filter (fun f -> member "method" f = `String "notifications/progress") frames
  in
  Alcotest.(check bool) "progress notifications" true (List.length progress >= 2);
  match List.rev frames with
  | last :: _ ->
      let r = member "result" last in
      Alcotest.(check bool) "not an error" false (member "isError" r = `Bool true);
      Alcotest.(check string)
        "structured entry" ("github:" ^ fn)
        (to_str (member "key" (member "structuredContent" r)))
  | [] -> Alcotest.fail "no frame"

(* ------------------------------------------------------------ CHECK-33 *)

let set_role (ctx : Ctx.t) key role =
  Db.run ctx.db "UPDATE repos SET role = ? WHERE key = ?"
    [ (match role with Some r -> Db.text r | None -> Sqlite3.Data.NULL); Db.text key ]

let ownership_of ctx key = to_str (member "ownership" (get ctx key))

let ownership () =
  let ctx = data_ctx () in
  let a = F.insert_repo ctx ~owner:"example-user" "own-lib" in
  let b = F.insert_repo ctx ~owner:"example-org" "team-lib" in
  let c = F.insert_repo ctx ~owner:"stranger" ~tier:"2" "other-lib" in
  set_role ctx a (Some "owner");
  set_role ctx b (Some "contributor");
  set_role ctx c None;
  Alcotest.(check (list string))
    "inferred"
    [ "own"; "participant"; "external" ]
    (List.map (ownership_of ctx) [ a; b; c ]);
  let keys f =
    match
      member "repos" (ok_or_fail (Ops.list ctx (`Assoc [ ("ownership", `String f) ])))
    with
    | `List l -> List.map (fun r -> to_str (member "key" r)) l
    | _ -> []
  in
  Alcotest.check strs "list ownership filter" [ c ] (keys "external");
  (match Ops.list ctx (`Assoc [ ("ownership", `String "theirs") ]) with
  | Error e ->
      Alcotest.(check bool) "filter validated" true (contains e "invalid ownership")
  | Ok _ -> Alcotest.fail "invalid ownership filter accepted");
  (match
     Ops.search ctx
       (`Assoc
          [
            ("query", `String "repository");
            ("filters", `Assoc [ ("ownership", `String "external") ]);
          ])
   with
  | Ok s ->
      Alcotest.(check bool) "search filter" false (contains (Yojson.Safe.to_string s) a)
  | Error e -> Alcotest.fail e);
  (* Override: a Tier-1 row registered as external leaves sections 1-2 and the counts and
     appears in section 8. *)
  let forge, fn = widget_forge () in
  let env = F.mixed_env forge in
  ignore (register ctx env [ ("target", `String ("https://github.com/" ^ fn)) ]);
  let ext = "github:" ^ fn in
  Alcotest.(check string) "inferred external (no role)" "external" (ownership_of ctx ext);
  ignore
    (register ctx env
       [
         ("target", `String ("https://github.com/" ^ fn));
         ("ownership", `String "external");
       ]);
  Db.run ctx.db "UPDATE repos SET tier = '1', role = 'owner' WHERE key = ?"
    [ Db.text ext ];
  Alcotest.(check string) "override wins" "external" (ownership_of ctx ext);
  let dir = temp_dir () in
  let out = Filename.concat dir "INDEX.md" in
  ignore (render ctx ~out ~notes_dir:dir ~now:parity_now ~home:"/nonexistent-home");
  let md = read_file out in
  let section n =
    let parts = Re.split (Re.compile (Re.str "\n## ")) md in
    let prefix = string_of_int n ^ ". " in
    Option.value ~default:"" (List.find_opt (fun p -> String.starts_with ~prefix p) parts)
  in
  Alcotest.(check bool) "absent from section 1" false (contains (section 1) fn);
  Alcotest.(check bool) "absent from section 2" false (contains (section 2) fn);
  Alcotest.(check bool) "section 8 present" true (contains md "## 8. External libraries");
  Alcotest.(check bool) "listed in section 8" true (contains (section 8) ext);
  Alcotest.(check bool)
    "tier-1 count excludes it" true
    (contains md "github: 4 total, 2 tier-1");
  let html = read_file (Filename.concat dir (Render.notes_file_name parity_now)) in
  Alcotest.(check bool) "html section 8" true (contains html "8. External libraries");
  let d =
    ok_or_fail
      (Ops.query_docs ctx
         (`Assoc
            [
              ("libraryId", `String ext);
              ("query", `String "gauge");
              ("filters", `Assoc [ ("ownership", `String "external") ]);
            ]))
  in
  Alcotest.(check bool)
    "external in query-docs" true
    (contains (Yojson.Safe.to_string d) "drawGauge")

let unregister () =
  let ctx = data_ctx () in
  let forge, fn = widget_forge () in
  let env = F.mixed_env forge in
  let key = "github:" ^ fn in
  let before = config_text ctx in
  (* A registered-only row nobody curated (FR-105): deleted with everything on it. *)
  ignore (register ctx env [ ("target", `String ("https://github.com/" ^ fn)) ]);
  Alcotest.(check bool) "documents before" true (row_count ctx "documents" key > 0);
  let id =
    Option.value ~default:0
      (Option.join
         (Db.query_one ctx.db "SELECT id FROM repos WHERE key = ?"
            [ Db.text key ]
            (fun r -> Db.to_int_opt r.(0))))
  in
  let r = ok_or_fail (Ops.unregister_repo ctx (`Assoc [ ("target", `String key) ])) in
  Alcotest.(check string) "deleted" "deleted" (to_str (member "row" r));
  Alcotest.(check string) "config restored" before (config_text ctx);
  let count sql =
    Option.value ~default:(-1)
      (Option.join (Db.query_one ctx.db sql [ Db.int id ] (fun r -> Db.to_int_opt r.(0))))
  in
  List.iter
    (fun (what, sql) -> Alcotest.(check int) (what ^ " gone") 0 (count sql))
    [
      ("row", "SELECT count(*) FROM repos WHERE id = ?");
      ("documents", "SELECT count(*) FROM documents WHERE repo_id = ?");
      ("usage", "SELECT count(*) FROM usage WHERE repo_id = ?");
      ("provenance", "SELECT count(*) FROM field_meta WHERE repo_id = ?");
      ("analysis", "SELECT count(*) FROM analysis WHERE repo_id = ?");
    ];
  Alcotest.(check int)
    "no orphan chunk" 0
    (Option.value ~default:(-1)
       (Option.join
          (Db.query_one ctx.db
             "SELECT count(*) FROM doc_chunks WHERE doc_id NOT IN (SELECT id FROM \
              documents)"
             [] (fun r -> Db.to_int_opt r.(0)))));
  F.docs_integrity ctx;
  fts_integrity ctx;
  Db.exec ctx.db "INSERT INTO api_items_fts(api_items_fts) VALUES('integrity-check')";
  Alcotest.(check bool)
    "FTS has no trace" true
    (Db.query ctx.db
       "SELECT rowid FROM doc_chunks_fts WHERE doc_chunks_fts MATCH 'drawGauge'" []
       (fun _ -> ())
    = []);
  Alcotest.(check bool)
    "export rewritten" false
    (contains (read_file (Filename.concat ctx.export_dir "curated.json")) key);
  (match Ops.unregister_repo ctx (`Assoc [ ("target", `String key) ]) with
  | Error e -> Alcotest.(check bool) "not registered" true (contains e "not registered")
  | Ok _ -> Alcotest.fail "unregistered twice");
  (* A repository still discovered by a source keeps its row and curated data. *)
  ignore
    (register ctx env
       [ ("target", `String ("https://github.com/" ^ fn)); ("notes", `String "kept") ]);
  Db.run ctx.db "UPDATE repos SET sources = '[\"gh-affiliation\"]' WHERE key = ?"
    [ Db.text key ];
  let r2 =
    ok_or_fail
      (Ops.unregister_repo ctx
         (`Assoc [ ("target", `String ("https://github.com/" ^ fn)) ]))
  in
  Alcotest.(check string) "kept" "kept" (to_str (member "row" r2));
  let g = get ctx key in
  Alcotest.(check string) "curated kept" "kept" (to_str (member "notes" g));
  Alcotest.(check bool) "registration removed" true (member "registered" g = `Bool false);
  Alcotest.(check bool) "documents kept" true (row_count ctx "documents" key > 0);
  (* unregister-source: rows listed only by it go, the others keep their row. *)
  let forge2 = Fake_forge.create () in
  Fake_forge.json forge2 "orgs/gadget-guild"
    (`Assoc [ ("login", `String "gadget-guild") ]);
  Fake_forge.json forge2 "orgs/gadget-guild/repos?type=all&per_page=100"
    (`List
       [
         Fake_forge.gh_repo "gadget-guild/sprocket"; Fake_forge.gh_repo "gadget-guild/cog";
       ]);
  ignore
    (ok_or_fail
       (Ops.register_source ctx (F.mixed_env forge2)
          (`Assoc [ ("kind", `String "github-org"); ("name", `String "gadget-guild") ])));
  Db.run ctx.db
    "UPDATE repos SET sources = '[\"gh-org:gadget-guild\",\"gh-affiliation\"]' WHERE key \
     = ?"
    [ Db.text "github:gadget-guild/cog" ];
  let s =
    ok_or_fail
      (Ops.unregister_source ctx
         (`Assoc [ ("kind", `String "github-org"); ("name", `String "gadget-guild") ]))
  in
  Alcotest.(check (pair int int))
    "removed, kept" (1, 1)
    ( (match member "repos_removed" s with `Int n -> n | _ -> -1),
      match member "repos_kept" s with `Int n -> n | _ -> -1 );
  Alcotest.(check bool)
    "cog keeps its other label" true
    (contains
       (Yojson.Safe.to_string (member "sources" (get ctx "github:gadget-guild/cog")))
       "gh-affiliation");
  (match Ops.get ctx (`Assoc [ ("repo", `String "github:gadget-guild/sprocket") ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "source-only row kept");
  Alcotest.(check bool)
    "source entry gone" false
    (contains (config_text ctx) "gadget-guild")

(* A label shared by several sources (every github-collab source writes "gh-collab")
   is not removed while another source still writes it. *)
let shared_label () =
  let ctx = data_ctx () in
  let path = Filename.concat ctx.data_dir Config.file_name in
  F.write_file path
    (config_text ctx
   ^ "\n[[source]]\nkind = \"github-collab\"\nname = \"second-collab\"\n");
  let key = F.insert_repo ctx ~owner:"second-collab" "shared" in
  Db.run ctx.db "UPDATE repos SET sources = '[\"gh-collab\"]' WHERE key = ?"
    [ Db.text key ];
  let s =
    ok_or_fail
      (Ops.unregister_source ctx
         (`Assoc [ ("kind", `String "github-collab"); ("name", `String "second-collab") ]))
  in
  Alcotest.(check int)
    "nothing removed" 0
    (match member "repos_removed" s with `Int n -> n | _ -> -1);
  Alcotest.(check string) "row kept" key (to_str (member "key" (get ctx key)))

let () =
  run "register"
    [
      ("config-edit", [ Alcotest.test_case "entry editor" `Quick config_edit ]);
      ( "registration",
        [
          Alcotest.test_case "AC-32 forge url" `Quick registration;
          Alcotest.test_case "AC-32 local targets" `Quick local_targets;
          Alcotest.test_case "AC-32 register-source" `Quick register_source;
          Alcotest.test_case "AC-32 confidentiality flags" `Quick confidentiality;
          Alcotest.test_case "AC-32 full refresh" `Quick full_refresh;
          Alcotest.test_case "AC-32 progress" `Quick mcp_progress;
        ] );
      ( "ownership",
        [
          Alcotest.test_case "AC-33 inference, override, views" `Quick ownership;
          Alcotest.test_case "AC-33 unregister" `Quick unregister;
          Alcotest.test_case "AC-33 shared label" `Quick shared_label;
        ] );
    ]
