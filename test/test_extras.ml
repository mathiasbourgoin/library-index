(* The extras batch tools (S13; spec US-12, US-13 extras, FR-091, FR-095):
   - CHECK-28 (AC-28): find-capability over the scanned API items and the document
     chunks, copies (mirrors, forks, local copies, aliases, maintained side), explain and
     whats-new (newest first);
   - CHECK-30 (AC-30): template completions and the prompts, listed and callable over an
     MCP session, and refresh progress with a token. *)

open Library_index
open Test_support.Support
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

let contains s sub = Re.execp (Re.compile (Re.str sub)) s
let items k j = match member k j with `List l -> l | _ -> []
let strs = Alcotest.(list string)

let checkout dev name files =
  let dir = Filename.concat dev name in
  F.git_checkout dir files;
  dir

let itf_mli =
  "(** ITF traces. *)\n\n\
   type trace\n\
   (** A parsed trace. *)\n\n\
   val parse_file : string -> trace\n\
   (** Parse an ITF trace file and replay-ready states. *)\n\n\
   val states : trace -> string list\n\
   (** The states, in order. *)\n"

let guide = "# Trace kit\n\n## Replaying\n\nReplay a trace against a model.\n"

(* A Tier-1 OCaml library with markdown docs (so no api document) and an .mli. *)
let trace_kit ctx dev =
  let dir =
    checkout dev "trace-kit"
      [
        ("README.md", "# trace-kit\n\nTraces.\n");
        ("docs/guide.md", guide);
        ("dune-project", "(lang dune 3.0)\n");
        ("src/itf.mli", itf_mli);
      ]
  in
  (F.insert_repo ctx ~owner:"example-user" ~local_dir:dir "trace-kit", dir)

let ingest ctx = F.ingest ctx (F.mixed_env (Fake_forge.create ()))
let results r = items "results" r
let find ctx args = ok_or_fail (Ops.find_capability ctx (`Assoc args))

let item_count (ctx : Ctx.t) key source =
  Option.value ~default:0
    (Option.join
       (Db.query_one ctx.db
          "SELECT count(*) FROM api_items i JOIN repos r ON r.id = i.repo_id WHERE r.key \
           = ? AND i.source = ?"
          [ Db.text key; Db.text source ]
          (fun r -> Db.to_int_opt r.(0))))

(* ------------------------------------------------------------ CHECK-28 *)

let find_capability () =
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let key, _ = trace_kit ctx dev in
  (* Eligibility: a Tier-2 and a campaign row with the same item are never found. *)
  let mk ?campaign ~tier name =
    let dir =
      checkout dev name
        [ ("dune-project", "(lang dune 3.0)\n"); ("src/itf.mli", itf_mli) ]
    in
    F.insert_repo ctx ~owner:"example-org" ?campaign ~tier ~local_dir:dir name
  in
  let t2 = mk ~tier:"2" "other-trace" in
  let camp = mk ~campaign:true ~tier:"1" "camp-trace" in
  let s = ingest ctx in
  Alcotest.(check (list string)) "no gap" [] s.gaps;
  Alcotest.(check (list (triple string string string)))
    "markdown docs: no api document (gating unchanged)"
    [ ("README.md", "readme", "local"); ("docs/guide.md", "doc", "local") ]
    (F.docs ctx key);
  Alcotest.(check int)
    "scan items despite the gating (module doc, type, two vals)" 4
    (item_count ctx key "scan");
  Alcotest.(check int) "no items for Tier 2" 0 (item_count ctx t2 "scan");
  Alcotest.(check int) "no items for a campaign row" 0 (item_count ctx camp "scan");
  let r = find ctx [ ("query", `String "ITF trace replay") ] in
  Alcotest.(check string) "AND match" "and" (to_str (member "mode" r));
  (match results r with
  | top :: _ ->
      let lib = member "library" top and item = member "item" top in
      Alcotest.(check string) "library id" key (to_str (member "id" lib));
      Alcotest.(check string) "library name" "trace-kit" (to_str (member "name" lib));
      Alcotest.(check string)
        "the .mli value" "Itf.parse_file"
        (to_str (member "qualified_path" item));
      Alcotest.(check string) "kind" "val" (to_str (member "kind" item));
      Alcotest.(check string) "source" "scan" (to_str (member "source" item));
      Alcotest.(check string) "file" "src/itf.mli" (to_str (member "file" item));
      Alcotest.(check bool) "line" true (member "line" item = `Int 6);
      Alcotest.(check string)
        "signature" "val parse_file : string -> trace"
        (to_str (member "signature" item));
      Alcotest.(check bool)
        "doc excerpt" true
        (contains (to_str (member "doc_excerpt" item)) "Parse an ITF trace file");
      Alcotest.(check bool)
        "library uri" true
        (contains
           (to_str (member "uri" lib))
           "libindex://repo/github/example-user/trace-kit");
      Alcotest.(check bool)
        "no stored document for the .mli: uri null" true
        (member "uri" top = `Null)
  | [] -> Alcotest.fail "no result");
  let keys = List.map (fun h -> to_str (member "id" (member "library" h))) (results r) in
  Alcotest.(check bool)
    "only the eligible library" true
    (List.for_all (fun k -> k = key) keys);
  (* Defence in depth: an API item stored for a campaign row is never returned. *)
  Db.run ctx.db
    {|INSERT INTO api_items(repo_id, module_path, name, qualified_path, kind, file, line,
  doc, source) SELECT id, 'Itf', 'parse_file', 'Itf.parse_file', 'val', 'src/itf.mli', 6,
  'Parse an ITF trace file and replay-ready states.', 'scan'
FROM repos WHERE key = ?|}
    [ Db.text camp ];
  let again = find ctx [ ("query", `String "ITF trace replay") ] in
  Alcotest.(check bool)
    "campaign item never returned" true
    (List.for_all
       (fun h -> to_str (member "id" (member "library" h)) <> camp)
       (results again));
  (* A documentation section is found too, with its document URI. *)
  let r = find ctx [ ("query", `String "replay a trace against a model") ] in
  let docs =
    List.filter (fun h -> to_str (member "source" (member "item" h)) = "doc") (results r)
  in
  (match docs with
  | d :: _ ->
      Alcotest.(check string)
        "doc section" "Trace kit > Replaying"
        (to_str (member "qualified_path" (member "item" d)));
      Alcotest.(check bool)
        "doc uri" true
        (contains (to_str (member "uri" d)) "docs%2Fguide.md")
  | [] -> Alcotest.fail "no doc result");
  (* Determinism, detail, filters, budget, validation. *)
  let q = [ ("query", `String "trace states") ] in
  Alcotest.(check string)
    "deterministic"
    (Yojson.Safe.to_string (find ctx q))
    (Yojson.Safe.to_string (find ctx q));
  let brief = find ctx (("detail", `String "brief") :: q) in
  Alcotest.(check bool)
    "brief: no signature" true
    (List.for_all (fun h -> member "signature" (member "item" h) = `Null) (results brief));
  let none = find ctx (("filters", `Assoc [ ("ownership", `String "own") ]) :: q) in
  Alcotest.(check int) "filters apply to the library" 0 (List.length (results none));
  let cut = find ctx (("budget_chars", `Int 300) :: q) in
  Alcotest.(check bool) "budget" true (member "truncated" cut = `Bool true);
  Alcotest.(check bool)
    "bad filter rejected" true
    (Result.is_error
       (Ops.find_capability ctx
          (`Assoc (("filters", `Assoc [ ("tier", `String "9") ]) :: q))));
  Alcotest.(check int)
    "empty query" 0
    (List.length (results (find ctx [ ("query", `String "  ") ])))

(* A library whose only documents are interface files: each item once (its api chunk is
   not returned next to its scanned item). *)
let item_once () =
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let dir =
    checkout dev "itf-only"
      [
        ("README.md", "# itf-only\n");
        ("dune-project", "(lang dune 3.0)\n");
        ("src/itf.mli", itf_mli);
      ]
  in
  let key = F.insert_repo ctx ~owner:"example-user" ~local_dir:dir "itf-only" in
  ignore (ingest ctx);
  Alcotest.(check bool)
    "api document stored" true
    (List.exists (fun (_, k, _) -> k = "api") (F.docs ctx key));
  let r = find ctx [ ("query", `String "parse_file") ] in
  let hits =
    List.filter
      (fun h -> to_str (member "qualified_path" (member "item" h)) = "Itf.parse_file")
      (results r)
  in
  Alcotest.(check int) "one result for the item" 1 (List.length hits);
  Alcotest.(check string)
    "the scanned item" "scan"
    (to_str (member "source" (member "item" (List.hd hits))));
  Alcotest.(check bool)
    "with its document uri" true
    (contains (to_str (member "uri" (List.hd hits))) "src%2Fitf.mli")

(* Incremental scanning by file content hash; removed files lose their items. *)
let scan_items () =
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let key, dir = trace_kit ctx dev in
  let s = ingest ctx in
  Alcotest.(check int) "first run scans the .mli" 1 s.scanned;
  let s = ingest ctx in
  Alcotest.(check int) "unchanged: not re-extracted" 0 s.scanned;
  F.write_file
    (Filename.concat dir "src/itf.mli")
    (itf_mli ^ "\nval replay : trace -> unit\n(** Replay it. *)\n");
  let s = ingest ctx in
  Alcotest.(check int) "changed: re-extracted" 1 s.scanned;
  Alcotest.(check int) "new item" 5 (item_count ctx key "scan");
  Sys.remove (Filename.concat dir "src/itf.mli");
  F.git_add dir;
  ignore (ingest ctx);
  Alcotest.(check int) "removed file: items gone" 0 (item_count ctx key "scan");
  Db.exec ctx.db "INSERT INTO api_items_fts(api_items_fts) VALUES('integrity-check')"

(* ------------------------------------------------------------ copies *)

let copies () =
  let ctx, main, mirror, fork, _ = F.mcp_fixture () in
  let dev = temp_dir () in
  let ldir = checkout dev "acme-mcp" [ ("README.md", "# acme-mcp\n") ] in
  let local =
    F.insert_repo ctx ~host:"local" ~local_dir:ldir ~last_commit:"2026-08-01" ~owner:""
      "acme-mcp"
  in
  let c = ok_or_fail (Ops.copies ctx (`Assoc [ ("name", `String "acme-mcp") ])) in
  (match items "groups" c with
  | [ g ] ->
      Alcotest.(check string)
        "maintained: most recent, GitHub first" main
        (to_str (member "maintained" g));
      Alcotest.check strs "members: maintained first, then by key"
        [ main; fork; mirror; local ]
        (List.map (fun m -> to_str (member "key" m)) (items "members" g));
      let m k = List.find (fun m -> to_str (member "key" m) = k) (items "members" g) in
      Alcotest.(check bool)
        "maintained flag" true
        (member "maintained" (m main) = `Bool true);
      Alcotest.(check bool)
        "others not maintained" true
        (member "maintained" (m mirror) = `Bool false);
      Alcotest.(check bool)
        "local checkout path" true
        (items "local_paths" (m local) = [ `String ldir ]);
      Alcotest.(check bool)
        "last_commit" true
        (member "last_commit" (m fork) = `String "2026-09-01")
  | _ -> Alcotest.fail "one group expected");
  Alcotest.(check int) "count" 4 (Option.value ~default:0 (Jsonx.int_opt "count" c));
  (* A curated maintained_side wins. *)
  ignore
    (ok_or_fail
       (Curate.annotate ctx ~repo:mirror
          ~fields:[ ("maintained_side", `String "gitlab") ]));
  let c = ok_or_fail (Ops.copies ctx (`Assoc [ ("name", `String "acme-mcp") ])) in
  Alcotest.(check string)
    "curated maintained side" mirror
    (to_str (member "maintained" (List.hd (items "groups" c))));
  Alcotest.check strs "the maintained member comes first" [ mirror; main; fork; local ]
    (List.map
       (fun m -> to_str (member "key" m))
       (items "members" (List.hd (items "groups" c))));
  (* Package aliases find the library. *)
  let gauge = F.insert_repo ctx ~owner:"example-user" "ocaml-gauge" in
  Db.run ctx.db "UPDATE repos SET aliases = '[\"gauge-kit\"]' WHERE key = ?"
    [ Db.text gauge ];
  let c = ok_or_fail (Ops.copies ctx (`Assoc [ ("name", `String "gauge-kit") ])) in
  Alcotest.check strs "found by alias" [ gauge ]
    (List.concat_map
       (fun g -> List.map (fun m -> to_str (member "key" m)) (items "members" g))
       (items "groups" c));
  let c = ok_or_fail (Ops.copies ctx (`Assoc [ ("name", `String "nothing-here") ])) in
  Alcotest.(check bool)
    "no match: hint" true
    (items "groups" c = [] && member "hint" c <> `Null)

(* ------------------------------------------------------------ explain *)

let explain () =
  let ctx, main, mirror, fork, _ = F.mcp_fixture () in
  ignore
    (ok_or_fail
       (Curate.record_usage ctx ~repo:main ~consumer:"notes-app" ~note:(Some "server")));
  ignore
    (ok_or_fail
       (Curate.annotate ctx ~repo:main ~fields:[ ("rules", `String "Use the v2 API") ]));
  let e = ok_or_fail (Ops.explain ctx (`Assoc [ ("repo", `String main) ])) in
  Alcotest.(check string) "key" main (to_str (member "key" e));
  Alcotest.(check string) "what" "MCP server library for OCaml" (to_str (member "what" e));
  Alcotest.(check string) "rules" "Use the v2 API" (to_str (member "rules" e));
  Alcotest.(check string) "status" "active" (to_str (member "status" e));
  Alcotest.(check int) "usage history" 1 (List.length (items "usage" e));
  Alcotest.(check bool)
    "consumers" true
    (contains (Yojson.Safe.to_string (member "consumers" e)) "notes-app");
  Alcotest.(check string)
    "provenance of rules" "curated"
    (to_str (member "rules" (member "provenance" e)));
  let cp = member "copies" e in
  Alcotest.(check int)
    "copies count" 2
    (Option.value ~default:0 (Jsonx.int_opt "count" cp));
  Alcotest.check strs "copies keys" [ fork; mirror ] (List.map to_str (items "keys" cp));
  Alcotest.(check string) "maintained" main (to_str (member "maintained" cp));
  Alcotest.(check bool) "doc_count" true (member "doc_count" e = `Int 1);
  Alcotest.(check bool)
    "api items" true
    (member "total" (member "api_item_count" e) = `Int 0);
  Alcotest.(check bool) "no analysis yet" true (member "analysis" e = `Null);
  Db.run ctx.db
    {|INSERT INTO analysis(repo_id, status, cause, head_sha, at, tool_version)
SELECT id, 'partial', 'empty call graph (LSP prepareCallHierarchy)', 'abc', '2026-10-03T00:00:00Z', 't'
FROM repos WHERE key = ?|}
    [ Db.text main ];
  let e = ok_or_fail (Ops.explain ctx (`Assoc [ ("repo", `String main) ])) in
  Alcotest.(check string)
    "analysis status" "partial"
    (to_str (member "status" (member "analysis" e)));
  Alcotest.(check bool)
    "unknown repo" true
    (Result.is_error (Ops.explain ctx (`Assoc [ ("repo", `String "no/such") ])))

(* ------------------------------------------------------------ whats-new *)

let whats_new () =
  let ctx, main, mirror, fork, user = F.mcp_fixture () in
  let set key pushed commit =
    Db.run ctx.db "UPDATE repos SET pushed_at = ?, last_commit = ? WHERE key = ?"
      [ Db.text pushed; Db.text commit; Db.text key ]
  in
  set main "2026-09-20T00:00:00Z" "2026-09-20";
  set mirror "2026-09-01T00:00:00Z" "2026-09-01";
  (* An offset: 2026-10-02T10:00+02:00 is 08:00Z. *)
  set fork "2026-10-02T10:00:00+02:00" "2026-09-01";
  set user "2026-08-01T00:00:00Z" "2026-08-01";
  let w = ok_or_fail (Ops.whats_new ctx (`Assoc [ ("since", `String "2026-09-15") ])) in
  Alcotest.check strs "newest first; older rows absent" [ main; fork ]
    (List.map (fun r -> to_str (member "key" r)) (items "repos" w));
  let first = List.hd (items "repos" w) in
  Alcotest.(check string)
    "latest is the document change" "2026-10-03T00:00:00Z"
    (to_str (member "latest" first));
  Alcotest.(check bool)
    "docs change listed" true
    (member "count" (member "docs" (member "changes" first)) = `Int 1);
  Alcotest.(check bool)
    "metadata change listed" true
    (member "pushed_at" (member "metadata" (member "changes" first)) <> `Null);
  Alcotest.(check string)
    "offset normalised" "2026-10-02T08:00:00Z"
    (to_str (member "latest" (List.nth (items "repos" w) 1)));
  let w =
    ok_or_fail
      (Ops.whats_new ctx
         (`Assoc [ ("since", `String "2026-09-15T00:00:00Z"); ("limit", `Int 1) ]))
  in
  Alcotest.(check bool)
    "limit" true
    (member "count" w = `Int 1 && member "total" w = `Int 2);
  Alcotest.(check bool)
    "invalid since rejected" true
    (Result.is_error (Ops.whats_new ctx (`Assoc [ ("since", `String "yesterday") ])))

(* ------------------------------------------------------------ CHECK-30 *)

let init_session server =
  let sess = Mcp_kit.Server.create_session server in
  let send j = Mcp_kit.Server.dispatch_session_json sess (Yojson.Safe.from_string j) in
  ignore
    (send
       {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}|});
  ignore (send {|{"jsonrpc":"2.0","method":"notifications/initialized"}|});
  send

let response send j =
  match send j with Some r -> r | None -> Alcotest.fail "no response"

let complete send ~uri ~name ~value ?(args = []) () =
  let ctx_args = `Assoc (List.map (fun (k, v) -> (k, `String v)) args) in
  response send
    (Yojson.Safe.to_string
       (`Assoc
          [
            ("jsonrpc", `String "2.0");
            ("id", `Int 5);
            ("method", `String "completion/complete");
            ( "params",
              `Assoc
                [
                  ( "ref",
                    `Assoc [ ("type", `String "ref/resource"); ("uri", `String uri) ] );
                  ("argument", `Assoc [ ("name", `String name); ("value", `String value) ]);
                  ("context", `Assoc [ ("arguments", ctx_args) ]);
                ] );
          ]))

let values r = List.map to_str (items "values" (member "completion" (member "result" r)))

let prompt_get send name args =
  response send
    (Yojson.Safe.to_string
       (`Assoc
          [
            ("jsonrpc", `String "2.0");
            ("id", `Int 6);
            ("method", `String "prompts/get");
            ( "params",
              `Assoc
                [
                  ("name", `String name);
                  ("arguments", `Assoc (List.map (fun (k, v) -> (k, `String v)) args));
                ] );
          ]))

let mcp_extras () =
  let ctx, main, _, _, _ = F.mcp_fixture () in
  let camp = F.insert_repo ctx ~owner:"example-campaigns" ~campaign:true "camp" in
  ignore camp;
  for i = 0 to 119 do
    ignore (F.insert_repo ctx ~owner:"bulk" (Printf.sprintf "lib-%03d" i))
  done;
  let forge = Fake_forge.create () in
  let server = ok_or_fail (Mcp_server.server ctx (Fake_forge.env forge)) in
  let send = init_session server in
  let init =
    let sess = Mcp_kit.Server.create_session server in
    Mcp_kit.Server.dispatch_session_json sess
      (Yojson.Safe.from_string
         {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}|})
  in
  let caps = member "capabilities" (member "result" (Option.get init)) in
  Alcotest.(check bool) "completions capability" true (member "completions" caps <> `Null);
  Alcotest.(check bool) "prompts capability" true (member "prompts" caps <> `Null);
  let repo_t = Liburi.repo_template and doc_t = Liburi.doc_template in
  Alcotest.check strs "host" [ "github"; "gitlab" ]
    (values (complete send ~uri:repo_t ~name:"host" ~value:"" ()));
  Alcotest.check strs "owner prefix, host given (campaign owners never)"
    [ "example-org"; "example-user" ]
    (values
       (complete send ~uri:repo_t ~name:"owner" ~value:"ex"
          ~args:[ ("host", "github") ]
          ()));
  Alcotest.check strs "name" [ "acme-mcp" ]
    (values
       (complete send ~uri:repo_t ~name:"name" ~value:"AC"
          ~args:[ ("host", "github"); ("owner", "example-org") ]
          ()));
  Alcotest.check strs "doc path, percent-encoded" [ "docs%2Fguide.md" ]
    (values
       (complete send ~uri:doc_t ~name:"path" ~value:"docs/"
          ~args:[ ("host", "github"); ("owner", "example-org"); ("name", "acme-mcp") ]
          ()));
  let many = complete send ~uri:repo_t ~name:"name" ~value:"lib-" () in
  let c = member "completion" (member "result" many) in
  Alcotest.(check int) "capped at 100" 100 (List.length (values many));
  Alcotest.(check bool)
    "total and has_more" true
    (member "total" c = `Int 120 && member "hasMore" c = `Bool true);
  let bad = complete send ~uri:repo_t ~name:"path" ~value:"" () in
  Alcotest.(check bool) "unknown variable rejected" true (member "error" bad <> `Null);
  (* Prompts. *)
  let listed = response send {|{"jsonrpc":"2.0","id":3,"method":"prompts/list"}|} in
  Alcotest.check strs "prompts listed"
    [ "before-building"; "record-usage" ]
    (List.sort compare
       (List.map
          (fun p -> to_str (member "name" p))
          (items "prompts" (member "result" listed))));
  let text r =
    match items "messages" (member "result" r) with
    | m :: _ -> to_str (member "text" (member "content" m))
    | [] -> ""
  in
  let b =
    text (prompt_get send "before-building" [ ("capability", "parse a trace\nfile") ])
  in
  List.iter
    (fun tool -> Alcotest.(check bool) ("names " ^ tool) true (contains b tool))
    [ "find-capability"; "search"; "resolve-library-id"; "explain"; "record_usage" ];
  Alcotest.(check bool)
    "argument quoted on one line" true
    (contains b "\"parse a trace file\"");
  let q =
    text (prompt_get send "before-building" [ ("capability", "say \"hi\" then stop") ])
  in
  Alcotest.(check bool)
    "inner quotes cannot close the quoted argument" true
    (contains q "\"say 'hi' then stop\"");
  let missing = prompt_get send "before-building" [] in
  Alcotest.(check bool) "capability required" true (member "error" missing <> `Null);
  let u =
    text (prompt_get send "record-usage" [ ("repo", main); ("consumer", "notes-app") ])
  in
  Alcotest.(check bool)
    "record-usage names the tool and arguments" true
    (contains u "record_usage" && contains u main && contains u "notes-app");
  (* The extra tools are callable and valid against their output schemas. *)
  List.iter
    (fun (name, args) ->
      let r =
        response send
          (Yojson.Safe.to_string
             (`Assoc
                [
                  ("jsonrpc", `String "2.0");
                  ("id", `Int 9);
                  ("method", `String "tools/call");
                  ("params", `Assoc [ ("name", `String name); ("arguments", `Assoc args) ]);
                ]))
      in
      Alcotest.(check bool) (name ^ " answered") true (member "error" r = `Null);
      Alcotest.(check bool)
        (name ^ " ok") true
        (member "isError" (member "result" r) = `Bool false))
    [
      ("find-capability", [ ("query", `String "acme server") ]);
      ("copies", [ ("name", `String "acme-mcp") ]);
      ("explain", [ ("repo", `String main) ]);
      ("whats-new", [ ("since", `String "2026-01-01") ]);
    ];
  (* Refresh progress with a token (FR-094, AC-30). *)
  let fn = "example-org/acme-mcp" in
  Fake_forge.json forge ("repos/" ^ fn) (Fake_forge.gh_repo fn);
  let frames =
    Mcp_kit.Server.dispatch_json_frames server
      (Yojson.Safe.from_string
         (Printf.sprintf
            {|{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"refresh","arguments":{"repo":"%s"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{},"progressToken":"tok-30"}}}|}
            main))
  in
  Alcotest.(check int)
    "refresh progress notifications" 3
    (List.length
       (List.filter
          (fun f -> member "method" f = `String "notifications/progress")
          frames))

let () =
  run "extras"
    [
      ( "find-capability",
        [ Alcotest.test_case "AC-28 items and sections" `Quick find_capability ] );
      ( "scan-items",
        [
          Alcotest.test_case "incremental by sha" `Quick scan_items;
          Alcotest.test_case "each item once" `Quick item_once;
        ] );
      ("copies", [ Alcotest.test_case "AC-28 groups" `Quick copies ]);
      ("explain", [ Alcotest.test_case "AC-28 fields" `Quick explain ]);
      ("whats-new", [ Alcotest.test_case "AC-28 newest first" `Quick whats_new ]);
      ("mcp-extras", [ Alcotest.test_case "AC-30 completions, prompts" `Quick mcp_extras ]);
    ]
