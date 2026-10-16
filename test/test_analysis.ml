(* Optional arch-index analysis (CHECK-34, AC-34; spec "Optional arch-index analysis").

   A fake arch-index ([arch_index_cli], a shell script writing a synthetic comment_db
   with the sqlite3 command-line tool from the SQL file named by $FAKE_ARCH_SQL), a fake
   language server and a fake dune sit in a temp bin directory that replaces PATH (plus
   /usr/bin and /bin for git, env and sqlite3). Each fake logs its argv and environment
   to $FAKE_ARCH_LOG. Nothing real is analysed or built. *)

open Library_index
open Test_support.Support
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

let contains s sub = Re.execp (Re.compile (Re.str sub)) s

let write_exe path text =
  F.write_file path text;
  Unix.chmod path 0o755

let fake_cli =
  {|#!/bin/sh
out=""; proj=""; lang=""; enrich=1
while [ $# -gt 0 ]; do
  case "$1" in
    --output) out="$2"; shift 2;;
    --project) proj="$2"; shift 2;;
    --language) lang="$2"; shift 2;;
    --no-enrich) enrich=0; shift;;
    *) shift;;
  esac
done
echo "arch lang=$lang project=$proj enrich=$enrich timeout=${EPURE_ARCH_INDEX_TIMEOUT_S:-unset}" >> "$FAKE_ARCH_LOG"
[ -s "$FAKE_ARCH_SQL" ] || exit 3
sqlite3 "$out" < "$FAKE_ARCH_SQL" || exit 4
|}

let fake_dune =
  {|#!/bin/sh
echo "dune $*" >> "$FAKE_ARCH_LOG"
[ "$1" = build ] && [ "$2" = --root ] || exit 2
mkdir -p "$3/_build/default/src" && touch "$3/_build/default/src/widget.cmt"
|}

(* A comment_db as the epure contract describes it (calls by id). *)
let sql_by_id ?(version = "1") ?(calls = true) () =
  Printf.sprintf
    {|CREATE TABLE comment_db_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);
INSERT INTO comment_db_meta VALUES ('schema_version', '%s');
INSERT INTO comment_db_meta VALUES ('language', 'ocaml');
INSERT INTO comment_db_meta VALUES ('built_by', 'arch_index_lsp');
CREATE TABLE functions (id INTEGER PRIMARY KEY, file_path TEXT NOT NULL, name TEXT NOT NULL,
  exported BOOLEAN NOT NULL DEFAULT 0, line_start INTEGER, summary TEXT, signature TEXT);
INSERT INTO functions VALUES (1, 'src/widget.ml', 'parse_widget', 1, 3, 'Parse a widget description.', 'string -> widget');
INSERT INTO functions VALUES (2, 'src/replay.ml', 'replay_trace', 1, 7, 'Replay a recorded trace against the model.', 'trace -> unit');
INSERT INTO functions VALUES (3, 'src/replay.ml', '_internal', 0, 12, NULL, NULL);
CREATE TABLE calls (id INTEGER PRIMARY KEY, caller_id INTEGER NOT NULL, callee_id INTEGER,
  callee_name TEXT NOT NULL, call_site TEXT);
%s|}
    version
    (if calls then
       "INSERT INTO calls VALUES (1, 2, 1, 'parse_widget', 'src/replay.ml:8');\n\
        INSERT INTO calls VALUES (4, 2, 1, 'parse_widget', 'src/replay.ml:10');\n\
        INSERT INTO calls VALUES (2, 2, 3, '_internal', 'src/replay.ml:9');\n\
        INSERT INTO calls VALUES (3, 2, NULL, 'List.iter', 'src/replay.ml:9');"
     else "")

(* The flat schema arch-index's LSP runner actually writes (calls by name and file). *)
let sql_by_name =
  {|CREATE TABLE comment_db_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);
INSERT INTO comment_db_meta VALUES ('schema_version', '1.3');
INSERT INTO comment_db_meta VALUES ('built_by', 'arch_index_lsp');
CREATE TABLE functions (id INTEGER PRIMARY KEY, name TEXT NOT NULL, file_path TEXT NOT NULL,
  line_start INTEGER NOT NULL DEFAULT 0, line_end INTEGER NOT NULL DEFAULT 0,
  exported INTEGER NOT NULL DEFAULT 0, signature TEXT, summary TEXT, language TEXT);
INSERT INTO functions VALUES (1, 'parse_widget', 'src/widget.ml', 3, 4, 1, NULL, 'Parse.', 'ocaml');
INSERT INTO functions VALUES (2, 'render', 'src/widget.ml', 6, 8, 1, NULL, 'Render.', 'ocaml');
CREATE TABLE calls (id INTEGER PRIMARY KEY, caller_name TEXT NOT NULL, caller_file TEXT NOT NULL,
  callee_name TEXT NOT NULL, callee_file TEXT, call_site TEXT, kind TEXT, edge_form TEXT);
INSERT INTO calls VALUES (1, 'render', 'src/widget.ml', 'parse_widget', 'src/widget.ml', 'src/widget.ml:7', 'MAY_ENUMERATED', NULL);
|}

let sql_empty =
  {|CREATE TABLE comment_db_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);
INSERT INTO comment_db_meta VALUES ('schema_version', '1');
CREATE TABLE functions (id INTEGER PRIMARY KEY, file_path TEXT NOT NULL, name TEXT NOT NULL,
  exported BOOLEAN NOT NULL DEFAULT 0);
|}

type env = { log : string; sql : string }

(* PATH = the fake bin dir + /usr/bin + /bin. *)
let setup ?(lsp = true) ?(cli = true) () =
  let bin = temp_dir () in
  let log = Filename.concat bin "log" and sql = Filename.concat bin "db.sql" in
  if cli then write_exe (Filename.concat bin "arch_index_cli") fake_cli;
  if lsp then write_exe (Filename.concat bin "ocamllsp") "#!/bin/sh\nexit 0\n";
  write_exe (Filename.concat bin "dune") fake_dune;
  Unix.putenv "PATH" (bin ^ ":/usr/bin:/bin");
  Unix.putenv "FAKE_ARCH_LOG" log;
  Unix.putenv "FAKE_ARCH_SQL" sql;
  F.write_file log "";
  { log; sql }

let set_sql e s = F.write_file e.sql s
let log e = read_file e.log

let commit dir =
  let q = Filename.quote dir in
  let cmd =
    Printf.sprintf
      "GIT_CONFIG_NOSYSTEM=1 git -C %s add -A && GIT_CONFIG_NOSYSTEM=1 git -C %s -c \
       user.name=t -c user.email=t@example.invalid commit -qm c"
      q q
  in
  if Sys.command cmd <> 0 then failwith "git commit failed"

let widget_files =
  [
    ("README.md", "# widget\n\nWidgets.\n");
    ("dune-project", "(lang dune 3.0)\n");
    ("src/widget.mli", "(** Parse. *)\nval parse_widget : string -> unit\n");
    ("src/widget.ml", "let parse_widget _ = ()\n");
  ]

(* A Tier-1 own local repository with a committed checkout. *)
let repo ?(built = true) ?(files = widget_files) ctx =
  let dev = temp_dir () in
  let dir = Filename.concat dev "widget" in
  F.git_checkout dir files;
  commit dir;
  if built then F.write_file (Filename.concat dir "_build/default/src/widget.cmt") "";
  let key =
    F.insert_repo ctx ~host:"local" ~local_dir:dir ~owner:"" ~what:"Widgets" "widget"
  in
  Db.run ctx.db "UPDATE repos SET role = 'owner' WHERE key = ?" [ Db.text key ];
  (key, dir)

let analyze ?(allow_build = false) ?(run = Some Proc.run) ctx key =
  ok_or_fail (Analysis.analyze ctx ~run ~allow_build key)

let status j = to_str (member "status" j)
let cause j = to_str (member "cause" j)

let arch_items (ctx : Ctx.t) key =
  Db.query ctx.db
    {|SELECT i.qualified_path, i.analyzed_head FROM api_items i JOIN repos r ON r.id = i.repo_id
WHERE r.key = ? AND i.source = 'arch-index' ORDER BY i.qualified_path|}
    [ Db.text key ]
    (fun r -> (Option.value (Db.to_string_opt r.(0)) ~default:"", Db.to_string_opt r.(1)))

let calls (ctx : Ctx.t) =
  Db.query ctx.db
    {|SELECT a.name, b.name FROM api_calls c JOIN api_items a ON a.id = c.caller_id
JOIN api_items b ON b.id = c.callee_id ORDER BY 1, 2|}
    [] (fun r ->
      ( Option.value (Db.to_string_opt r.(0)) ~default:"",
        Option.value (Db.to_string_opt r.(1)) ~default:"" ))

let analysis_status (ctx : Ctx.t) key =
  Option.join
    (Db.query_one ctx.db
       "SELECT a.status FROM analysis a JOIN repos r ON r.id = a.repo_id WHERE r.key = ?"
       [ Db.text key ]
       (fun r -> Db.to_string_opt r.(0)))

(* ------------------------------------------------------------ preconditions *)

let preconditions () =
  let ctx = empty_ctx () in
  let remote = F.insert_repo ctx ~owner:"example-user" "remote-only" in
  let e = setup ~lsp:false () in
  let r = analyze ctx remote in
  Alcotest.(check string) "no checkout: skipped" "skipped" (status r);
  Alcotest.(check bool) "cause" true (contains (cause r) "no local checkout");
  let key, _ = repo ctx in
  let r = analyze ctx key in
  Alcotest.(check string) "no LSP: skipped" "skipped" (status r);
  Alcotest.(check string) "LSP cause" "LSP binary ocamllsp not found on PATH" (cause r);
  Alcotest.(check bool) "HEAD recorded" true (member "head_sha" r <> `Null);
  Alcotest.(check string) "arch-index not run" "" (log e);
  let _ = setup ~cli:false () in
  let r = analyze ctx key in
  Alcotest.(check bool)
    "no arch-index on PATH" true
    (status r = "skipped" && contains (cause r) "arch-index not found on PATH");
  let _ = setup () in
  let r = analyze ~run:None ctx key in
  Alcotest.(check bool)
    "disabled runner" true
    (status r = "skipped" && contains (cause r) "disabled");
  (* No language arch-index supports. *)
  let ctx2 = empty_ctx () in
  let k2, _ = repo ~files:[ ("README.md", "# x\n"); ("notes.txt", "x") ] ctx2 in
  let r = analyze ctx2 k2 in
  Alcotest.(check bool)
    "unsupported language" true
    (status r = "skipped" && contains (cause r) "no language arch-index supports");
  (* A campaign row is never analysed. *)
  Db.run ctx.db "UPDATE repos SET campaign = 1 WHERE key = ?" [ Db.text key ];
  let r = analyze ctx key in
  Alcotest.(check bool)
    "campaign: skipped" true
    (status r = "skipped" && contains (cause r) "campaign");
  Alcotest.(check (option string)) "recorded" (Some "skipped") (analysis_status ctx key)

(* ------------------------------------------------------------ import *)

let import () =
  let ctx = empty_ctx () in
  let key, dir = repo ctx in
  let e = setup () in
  set_sql e (sql_by_id ());
  let r = analyze ctx key in
  Alcotest.(check string) "ok" "ok" (status r);
  Alcotest.(check bool) "items (exported only)" true (member "items" r = `Int 2);
  Alcotest.(check bool) "calls between imported items" true (member "calls" r = `Int 1);
  Alcotest.(check string)
    "tool version" "arch_index_lsp schema 1"
    (to_str (member "tool_version" r));
  let head = Analysis.head ~run:Proc.run dir in
  Alcotest.(check bool) "head read" true (head <> None);
  Alcotest.(check (list (pair string (option string))))
    "items with module path and analysed head"
    [ ("Replay.replay_trace", head); ("Widget.parse_widget", head) ]
    (arch_items ctx key);
  Alcotest.(check (list (pair string string)))
    "api_calls"
    [ ("replay_trace", "parse_widget") ]
    (calls ctx);
  Db.exec ctx.db "INSERT INTO api_items_fts(api_items_fts) VALUES('integrity-check')";
  let l = log e in
  Alcotest.(check bool)
    "invocation: language, project, timeout env, enrichment" true
    (contains l "lang=ocaml"
    && contains l ("project=" ^ dir)
    && contains l "timeout=300" && contains l "enrich=1");
  let tmp = Filename.concat ctx.data_dir "analysis" in
  Alcotest.(check (array string)) "temp db removed" [||] (Sys.readdir tmp);
  (* find-capability surfaces an item only arch-index knows (no .mli for replay). *)
  let f =
    ok_or_fail
      (Ops.find_capability ctx (`Assoc [ ("query", `String "replay recorded trace") ]))
  in
  (match member "results" f with
  | `List (top :: _) ->
      Alcotest.(check string)
        "arch-index item" "Replay.replay_trace"
        (to_str (member "qualified_path" (member "item" top)));
      Alcotest.(check string)
        "source" "arch-index"
        (to_str (member "source" (member "item" top)))
  | _ -> Alcotest.fail "no find-capability result");
  let x = ok_or_fail (Ops.explain ctx (`Assoc [ ("repo", `String key) ])) in
  Alcotest.(check string)
    "explain analysis" "ok"
    (to_str (member "status" (member "analysis" x)));
  Alcotest.(check bool)
    "explain counts" true
    (member "arch-index" (member "api_item_count" x) = `Int 2);
  (* The flat layout arch-index's LSP runner writes: calls by name and file. *)
  set_sql e sql_by_name;
  let r = analyze ctx key in
  Alcotest.(check string) "flat schema ok" "ok" (status r);
  Alcotest.(check (list (pair string string)))
    "calls by name"
    [ ("render", "parse_widget") ]
    (calls ctx);
  Alcotest.(check string)
    "tool version 1.3" "arch_index_lsp schema 1.3"
    (to_str (member "tool_version" r));
  (* The re-import replaced the arch-index items through the FTS triggers. *)
  Db.exec ctx.db "INSERT INTO api_items_fts(api_items_fts) VALUES('integrity-check')"

(* ------------------------------------------------------------ partial / failed *)

let failures () =
  let ctx = empty_ctx () in
  let key, _ = repo ctx in
  let e = setup () in
  set_sql e (sql_by_id ~calls:false ());
  let r = analyze ctx key in
  Alcotest.(check string) "empty call graph: partial" "partial" (status r);
  Alcotest.(check string) "cause" "empty call graph (LSP prepareCallHierarchy)" (cause r);
  Alcotest.(check int) "items of the partial run" 2 (List.length (arch_items ctx key));
  (* Each failure keeps the previous arch-index items and records its cause. *)
  let fails sql expect =
    set_sql e sql;
    let r = analyze ctx key in
    Alcotest.(check string) ("failed: " ^ expect) "failed" (status r);
    Alcotest.(check bool) ("cause: " ^ expect) true (contains (cause r) expect);
    Alcotest.(check int) ("items kept: " ^ expect) 2 (List.length (arch_items ctx key))
  in
  fails "" "arch-index failed (other)";
  fails sql_empty "indexed no function";
  fails (sql_by_id ~version:"2" ()) "unsupported comment_db schema_version 2";
  fails "CREATE TABLE x (y);" "no comment_db schema_version";
  Alcotest.(check (option string)) "recorded" (Some "failed") (analysis_status ctx key);
  (* The scan index and the docs refresh are untouched by any of it. *)
  let s = F.ingest ctx (F.mixed_env (Fake_forge.create ())) in
  Alcotest.(check (list string)) "docs refresh green" [] s.gaps;
  Alcotest.(check bool) "scan items present" true (s.scanned > 0)

(* ------------------------------------------------------------ build *)

let build () =
  let ctx = empty_ctx () in
  let key, dir = repo ~built:false ctx in
  let e = setup () in
  set_sql e (sql_by_id ());
  let r = analyze ctx key in
  Alcotest.(check string) "not built: partial" "partial" (status r);
  Alcotest.(check bool)
    "cause" true
    (contains (cause r) "not built: CMT enrichment skipped");
  Alcotest.(check bool) "run with --no-enrich" true (contains (log e) "enrich=0");
  Alcotest.(check bool) "nothing built" false (contains (log e) "dune ");
  F.write_file e.log "";
  let r = analyze ~allow_build:true ctx key in
  Alcotest.(check string) "allow_build: ok" "ok" (status r);
  Alcotest.(check bool)
    "the project's own dune build ran" true
    (contains (log e) ("dune build --root " ^ dir));
  Alcotest.(check bool) "then enriched" true (contains (log e) "enrich=1")

(* ------------------------------------------------------------ refresh hook, MCP, deep *)

let hook () =
  let ctx = empty_ctx () in
  let key, dir = repo ctx in
  let e = setup () in
  set_sql e (sql_by_id ());
  let s = Analysis.refresh_hook ctx ~run:Proc.run in
  Alcotest.(check (pair int int)) "first run analyses" (1, 1) (s.considered, s.analysed);
  Alcotest.(check int) "ok" 1 s.ok;
  let s = Analysis.refresh_hook ctx ~run:Proc.run in
  Alcotest.(check int) "same HEAD: not analysed again" 0 s.analysed;
  F.write_file
    (Filename.concat dir "src/widget.ml")
    "let parse_widget _ = ()\nlet x = 1\n";
  commit dir;
  let s = Analysis.refresh_hook ctx ~run:Proc.run in
  Alcotest.(check int) "new HEAD: analysed" 1 s.analysed;
  (* Not own: never analysed by the hook. *)
  Db.run ctx.db "UPDATE repos SET role = NULL WHERE key = ?" [ Db.text key ];
  F.write_file (Filename.concat dir "README.md") "# changed\n";
  commit dir;
  let s = Analysis.refresh_hook ctx ~run:Proc.run in
  Alcotest.(check int) "external: not considered" 0 s.considered;
  (* A failing runner never raises. *)
  Db.run ctx.db "UPDATE repos SET role = 'owner' WHERE key = ?" [ Db.text key ];
  let failing ~timeout:_ _ = Error `Other in
  let s = Analysis.refresh_hook ctx ~run:failing in
  Alcotest.(check int) "unreadable HEAD: not analysed" 0 s.analysed

let mcp_and_deep () =
  let ctx = empty_ctx () in
  let key, dir = repo ctx in
  let e = setup () in
  set_sql e (sql_by_id ());
  let forge = Fake_forge.create () in
  (* Real read-only git for the checkout: a checkout whose remotes cannot be read is
     treated as a campaign checkout (fail closed). *)
  let env = { (F.mixed_env forge) with analyze = Some Proc.run } in
  let server = ok_or_fail (Mcp_server.server ctx env) in
  let sess = Mcp_kit.Server.create_session server in
  let send j =
    match Mcp_kit.Server.dispatch_session_json sess (Yojson.Safe.from_string j) with
    | Some r -> r
    | None -> Alcotest.fail "no response"
  in
  ignore
    (send
       {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}|});
  ignore
    (Mcp_kit.Server.dispatch_session_json sess
       (Yojson.Safe.from_string {|{"jsonrpc":"2.0","method":"notifications/initialized"}|}));
  let call id args =
    send
      (Printf.sprintf
         {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"analyze-repo","arguments":%s}}|}
         id args)
  in
  let r = call 2 (Printf.sprintf {|{"repo":"%s"}|} key) in
  let res = member "result" r in
  Alcotest.(check bool)
    "analyze-repo answered, schema satisfied" true
    (member "error" r = `Null && member "isError" res = `Bool false);
  Alcotest.(check string)
    "status" "ok"
    (to_str (member "status" (member "structuredContent" res)));
  (* A skip is a normal result too. *)
  Unix.putenv "PATH" "/usr/bin:/bin";
  let r = call 3 (Printf.sprintf {|{"repo":"%s","allow_build":false}|} key) in
  let res = member "result" r in
  Alcotest.(check bool) "skip is not an isError" true (member "isError" res = `Bool false);
  Alcotest.(check string)
    "skipped" "skipped"
    (to_str (member "status" (member "structuredContent" res)));
  (* register-repo deep=true runs the analysis on a local target. *)
  let e = setup () in
  set_sql e (sql_by_id ());
  let o =
    ok_or_fail
      (Register.register_repo ctx env
         (`Assoc [ ("target", `String dir); ("deep", `Bool true) ]))
  in
  Alcotest.(check string)
    "deep registration analysed" "ok"
    (to_str (member "status" (member "deep" o.json)))

let () =
  run "analysis"
    [
      ( "arch-index",
        [
          Alcotest.test_case "preconditions" `Quick preconditions;
          Alcotest.test_case "import" `Quick import;
          Alcotest.test_case "partial and failed" `Quick failures;
          Alcotest.test_case "build only when allowed" `Quick build;
          Alcotest.test_case "refresh hook" `Quick hook;
          Alcotest.test_case "MCP tool and deep registration" `Quick mcp_and_deep;
        ] );
    ]
