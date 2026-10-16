(* MCP server through an mcp-kit session (CHECK-14): the eighteen tools, including refresh
   (S9) driven by a fake gh/glab runner. Results are read from structuredContent; the
   text content is a short rendering (S12, FR-092). *)

open Library_index
open Test_support.Support

let expected_tools =
  [
    "analyze-repo";
    "annotate";
    "copies";
    "explain";
    "find-capability";
    "get";
    "list";
    "list-sources";
    "query-docs";
    "record_usage";
    "refresh";
    "register-repo";
    "register-source";
    "resolve-library-id";
    "search";
    "unregister-repo";
    "unregister-source";
    "whats-new";
  ]

module Fake_forge = Test_support.Fake_forge

let send sess j = Mcp_kit.Server.dispatch_session_json sess (Yojson.Safe.from_string j)

let session_of server =
  let sess = Mcp_kit.Server.create_session server in
  let init =
    send sess
      {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}|}
  in
  Alcotest.(check bool) "initialize answered" true (init <> None);
  Alcotest.(check bool)
    "initialized is a notification" true
    (send sess {|{"jsonrpc":"2.0","method":"notifications/initialized"}|} = None);
  (sess, Option.get init)

let session ?(forge = Fake_forge.create ()) ctx =
  session_of (ok_or_fail (Mcp_server.server ctx (Fake_forge.env forge)))

let call sess name args =
  let req =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int 7);
        ("method", `String "tools/call");
        ("params", `Assoc [ ("name", `String name); ("arguments", args) ]);
      ]
  in
  match Mcp_kit.Server.dispatch_session_json sess req with
  | Some r -> member "result" r
  | None -> Alcotest.fail "no response to tools/call"

let is_error r = member "isError" r = `Bool true

let text r =
  match member "content" r with
  | `List (c :: _) -> to_str (member "text" c)
  | _ -> Alcotest.fail "no content"

let structured r = member "structuredContent" r
let contains s sub = Re.execp (Re.compile (Re.str sub)) s

let handshake () =
  let ctx = fresh_ctx () in
  let sess, init = session ctx in
  let instr = to_str (member "instructions" (member "result" init)) in
  Alcotest.(check bool)
    "instructions flag untrusted content" true (contains instr "untrusted");
  let tools =
    match send sess {|{"jsonrpc":"2.0","id":2,"method":"tools/list"}|} with
    | Some r -> ( match member "tools" (member "result" r) with `List l -> l | _ -> [])
    | None -> []
  in
  let names = List.sort compare (List.map (fun t -> to_str (member "name" t)) tools) in
  Alcotest.(check (list string)) "exact tool set" expected_tools names;
  List.iter
    (fun t ->
      Alcotest.(check string)
        (to_str (member "name" t) ^ " has object schema")
        "object"
        (to_str (member "type" (member "inputSchema" t))))
    tools

let get_tool () =
  let ctx = fresh_ctx () in
  let sess, _ = session ctx in
  let r = call sess "get" (`Assoc [ ("repo", `String "local:sample-lib") ]) in
  Alcotest.(check bool) "ok" false (is_error r);
  let j = structured r in
  Alcotest.(check string) "key" "local:sample-lib" (to_str (member "key" j));
  Alcotest.(check bool) "short text rendering" true (String.length (text r) <= 2000 * 4);
  Alcotest.(check string)
    "structured" "local:sample-lib"
    (to_str (member "key" (member "structuredContent" r)));
  let r = call sess "get" (`Assoc [ ("repo", `String "acme-mcp") ]) in
  Alcotest.(check bool) "ambiguous is isError" true (is_error r);
  Alcotest.(check bool) "lists keys" true (contains (text r) "gitlab:acme/acme-mcp");
  let r = call sess "get" (`Assoc []) in
  Alcotest.(check string)
    "same message as CLI layer" "missing required argument 'repo'" (text r);
  let r = call sess "get" (`Assoc [ ("repo", `String "x"); ("bogus", `Int 1) ]) in
  Alcotest.(check bool)
    "unknown argument rejected" true
    (contains (text r) "unknown argument 'bogus'")

(* AM-8: the Ops layer is the single guard. An exception inside an operation is a
   classified message (never its payload), and every served tool, refresh included,
   stays an [isError] result on a broken DB. *)
let no_leak () =
  Alcotest.(check bool)
    "guard classifies" true
    (Errors.guard (fun () -> failwith "SELECT secret FROM repos /home/x ghp_token")
    = Error "internal error");
  Alcotest.(check bool)
    "db error classified" true
    (Errors.guard (fun () -> raise (Db.Db_error "database error (CONSTRAINT)"))
    = Error "database error (CONSTRAINT)");
  let ctx = fresh_ctx () in
  let sess, _ = session ctx in
  Db.close ctx.db;
  List.iter
    (fun (tool, args) ->
      let r = call sess tool args in
      Alcotest.(check bool) (tool ^ ": closed DB → isError") true (is_error r);
      Alcotest.(check bool) (tool ^ ": no SQL text") false (contains (text r) "SELECT"))
    [
      ("get", `Assoc [ ("repo", `String "sample-lib") ]);
      ("search", `Assoc [ ("query", `String "ocaml") ]);
      ("list", `Assoc []);
      ( "annotate",
        `Assoc
          [
            ("repo", `String "sample-lib"); ("fields", `Assoc [ ("notes", `String "x") ]);
          ] );
      ( "record_usage",
        `Assoc [ ("repo", `String "sample-lib"); ("consumer", `String "c") ] );
      ("refresh", `Assoc [ ("repo", `String "sample-lib") ]);
    ]

let search_tool () =
  let ctx = fresh_ctx () in
  let sess, _ = session ctx in
  let r =
    call sess "search" (`Assoc [ ("query", `String "ocaml mcp"); ("limit", `Int 3) ])
  in
  Alcotest.(check bool) "ok" false (is_error r);
  let j = structured r in
  Alcotest.(check string) "mode" "and" (to_str (member "mode" j));
  Alcotest.(check bool)
    "≤ 3 hits" true
    (match member "hits" j with `List l -> List.length l <= 3 && l <> [] | _ -> false);
  let r =
    call sess "search"
      (`Assoc [ ("query", `String "x"); ("filters", `Assoc [ ("tier", `String "one") ]) ])
  in
  Alcotest.(check bool) "invalid filter is isError" true (is_error r);
  Alcotest.(check string)
    "same message as CLI"
    "invalid tier 'one'; allowed values: 1, 2, fork, collab, inherited, whitehat" (text r);
  let r = call sess "search" (`Assoc [ ("query", `String "\"foo AND ( OR") ]) in
  Alcotest.(check bool) "hostile query is not an error" false (is_error r)

let list_tool () =
  let ctx = fresh_ctx () in
  let sess, _ = session ctx in
  let r = call sess "list" (`Assoc [ ("host", `String "local"); ("limit", `Int 3) ]) in
  Alcotest.(check bool) "ok" false (is_error r);
  let j = structured r in
  Alcotest.(check string) "total local" "3" (to_str (member "total" j));
  let r = call sess "list" (`Assoc [ ("colour", `String "x") ]) in
  Alcotest.(check bool) "unknown key isError" true (is_error r);
  Alcotest.(check bool) "names keys" true (contains (text r) "allowed keys: host")

let curation_tools () =
  let ctx = fresh_ctx () in
  let sess, _ = session ctx in
  let r =
    call sess "record_usage"
      (`Assoc
         [
           ("repo", `String "github:example-org/acme-mcp");
           ("consumer", `String "library-index");
           ("note", `String "MCP server");
         ])
  in
  Alcotest.(check bool) "record_usage ok" false (is_error r);
  let r =
    call sess "annotate"
      (`Assoc
         [ ("repo", `String "sample-lib"); ("fields", `Assoc [ ("notes", `String "x") ]) ])
  in
  Alcotest.(check bool) "annotate ok" false (is_error r);
  let r =
    call sess "annotate"
      (`Assoc
         [ ("repo", `String "sample-lib"); ("fields", `Assoc [ ("url", `String "u") ]) ])
  in
  Alcotest.(check bool) "annotate url isError" true (is_error r);
  Alcotest.(check bool)
    "lists allowed fields" true
    (contains (text r) "allowed fields: what");
  let r =
    call sess "record_usage"
      (`Assoc [ ("repo", `String "acme-mcp"); ("consumer", `String "c") ])
  in
  Alcotest.(check bool) "ambiguous isError" true (is_error r);
  let exported = read_file (Filename.concat ctx.export_dir "curated.json") in
  Alcotest.(check bool) "curated.json updated" true (contains exported "library-index")

(* S0 spike (b), kept as a regression probe of mcp-kit at the pin: a nullable property
   schema is accepted and validated. If this ever fails, record it as an upstream gap. *)
let nullable_schema () =
  let schema =
    ok_or_fail
      (Mcp_kit.Schema.of_string
         {|{"type":"object","properties":{"v":{"type":["string","null"]}}}|})
  in
  let t =
    Mcp_kit.Tool.make ~input_schema:schema "t" (fun _ -> Ok (Mcp_kit.Tool.text "ok"))
  in
  let ok a = not (Mcp_kit.Tool.call t (Yojson.Safe.from_string a)).is_error in
  Alcotest.(check bool) "null accepted" true (ok {|{"v":null}|});
  Alcotest.(check bool) "string accepted" true (ok {|{"v":"x"}|});
  Alcotest.(check bool) "number rejected" false (ok {|{"v":5}|})

let refresh_tool () =
  let ctx = fresh_ctx () in
  let forge = Fake_forge.create () in
  let fn = "example-org/acme-mcp" in
  Fake_forge.json forge ("repos/" ^ fn) (Fake_forge.gh_repo fn);
  Fake_forge.ok forge
    ("repos/" ^ fn ^ "/readme")
    "# mcp-kit\n\n\
     An OCaml library for building Model Context Protocol servers, for tests.\n";
  let timeouts = ref [] in
  let inner = Fake_forge.runner forge in
  let env =
    {
      (Fake_forge.env forge) with
      run =
        (fun ~timeout argv ->
          timeouts := timeout :: !timeouts;
          inner ~timeout argv);
    }
  in
  let sess, _ = session_of (ok_or_fail (Mcp_server.server ctx env)) in
  let r = call sess "refresh" (`Assoc [ ("repo", `String ("github:" ^ fn)) ]) in
  Alcotest.(check bool) "ok" false (is_error r);
  let j = structured r in
  Alcotest.(check string) "key" ("github:" ^ fn) (to_str (member "key" j));
  Alcotest.(check string) "readme fetched" "fetched" (to_str (member "readme" j));
  Alcotest.(check bool)
    "per-command timeout within 30 s" true
    (!timeouts <> [] && List.for_all (fun t -> t > 0. && t <= 30.) !timeouts);
  let r = call sess "get" (`Assoc [ ("repo", `String ("github:" ^ fn)) ]) in
  Alcotest.(check bool)
    "README visible through get" true
    (contains (to_str (member "readme" (structured r))) "Model Context");
  (* C-31: gh not authenticated → classified isError. *)
  Fake_forge.fail forge ("repos/" ^ fn) `Auth;
  let r = call sess "refresh" (`Assoc [ ("repo", `String ("github:" ^ fn)) ]) in
  Alcotest.(check bool) "auth is isError" true (is_error r);
  Alcotest.(check bool) "auth class" true (contains (text r) "metadata failed (auth)");
  Alcotest.(check bool)
    "partial: report kept as structured content" true
    (member "complete" (member "structuredContent" r) = `Bool false);
  (* Same arguments, same Ops layer, same message as the CLI. *)
  let cli = Ops.refresh ctx env ~allow_full:false (`Assoc [ ("full", `Bool true) ]) in
  let r = call sess "refresh" (`Assoc [ ("full", `Bool true) ]) in
  Alcotest.(check bool)
    "missing repo: same message" true
    (is_error r && cli = Error (text r));
  let r = call sess "refresh" (`Assoc [ ("repo", `String "acme-mcp") ]) in
  Alcotest.(check bool) "ambiguous is isError" true (is_error r);
  let r =
    call sess "refresh" (`Assoc [ ("repo", `String "x"); ("full", `String "yes") ])
  in
  Alcotest.(check bool)
    "schema-invalid full is isError (EC-24)" true
    (is_error r && contains (text r) "full");
  let r =
    call sess "refresh"
      (`Assoc [ ("repo", `String "whitehat:shared-tools (shared)/proof-checker/") ])
  in
  Alcotest.(check bool) "whitehat is isError" true (is_error r)

(* AM-20: stored bytes that are not UTF-8 (written behind the sanitising helpers) still
   come out as valid UTF-8 JSON. *)
let utf8_output () =
  let ctx = fresh_ctx () in
  Db.run ctx.db "UPDATE repos SET notes = ? WHERE key = 'local:sample-lib'"
    [ Sqlite3.Data.TEXT "bad \xff\xfe \xc3 bytes" ];
  let sess, _ = session ctx in
  let r = call sess "get" (`Assoc [ ("repo", `String "sample-lib") ]) in
  Alcotest.(check bool) "get ok" false (is_error r);
  Alcotest.(check bool) "valid UTF-8 text" true (String.is_valid_utf_8 (text r));
  Alcotest.(check bool)
    "valid UTF-8 JSON" true
    (String.is_valid_utf_8 (Yojson.Safe.to_string (member "structuredContent" r)));
  let r = call sess "search" (`Assoc [ ("query", `String "x\xff") ]) in
  Alcotest.(check bool) "query echo valid" true (String.is_valid_utf_8 (text r))

let () =
  run "mcp"
    [
      ("handshake", [ Alcotest.test_case "initialize + tools/list" `Quick handshake ]);
      ("get", [ Alcotest.test_case "get tool" `Quick get_tool ]);
      ("search", [ Alcotest.test_case "search tool" `Quick search_tool ]);
      ("list", [ Alcotest.test_case "list tool" `Quick list_tool ]);
      ( "curation",
        [ Alcotest.test_case "annotate + record_usage tools" `Quick curation_tools ] );
      ("no-leak", [ Alcotest.test_case "exceptions are classified" `Quick no_leak ]);
      ("refresh", [ Alcotest.test_case "refresh tool" `Quick refresh_tool ]);
      ("utf8", [ Alcotest.test_case "valid UTF-8 output" `Quick utf8_output ]);
      ( "nullable-schema",
        [ Alcotest.test_case "mcp-kit accepts nullable" `Quick nullable_schema ] );
    ]
