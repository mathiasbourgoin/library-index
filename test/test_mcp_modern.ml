(* The modern MCP surface (S12, spec US-13, FR-092..FR-094, CHECK-29): outputSchema and
   valid structuredContent for every query tool, URIs on every hit, the short text
   rendering, the repo/doc resource templates (not-found for whitehat, unknown and
   campaign documents) and refresh progress. *)

open Library_index
open Test_support.Support
module Fake_forge = Test_support.Fake_forge
module F = Test_support.Doc_fixture

let contains s sub = Re.execp (Re.compile (Re.str sub)) s
let items k j = match member k j with `List l -> l | _ -> []

let init_session server =
  let sess = Mcp_kit.Server.create_session server in
  let send j = Mcp_kit.Server.dispatch_session_json sess j in
  ignore
    (send
       (Yojson.Safe.from_string
          {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}|}));
  ignore
    (send
       (Yojson.Safe.from_string {|{"jsonrpc":"2.0","method":"notifications/initialized"}|}));
  (sess, send)

let req id meth params =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String meth);
      ("params", params);
    ]

let call send name args =
  match
    send (req 7 "tools/call" (`Assoc [ ("name", `String name); ("arguments", args) ]))
  with
  | Some r ->
      Alcotest.(check bool)
        (name ^ ": no JSON-RPC error (outputSchema satisfied)")
        true
        (member "error" r = `Null);
      member "result" r
  | None -> Alcotest.fail "no response"

let read send uri =
  match send (req 8 "resources/read" (`Assoc [ ("uri", `String uri) ])) with
  | Some r -> r
  | None -> Alcotest.fail "no response"

let modern_mcp () =
  let ctx, main, mirror, _, _ = F.mcp_fixture () in
  let deep = F.insert_repo ctx ~host:"gitlab" ~gl_id:9 ~owner:"acme/sub" "deep" in
  let camp = F.insert_repo ctx ~owner:"acme-campaigns" ~campaign:true "camp" in
  F.add_doc ctx camp "docs/secret.md" "## Secret\n\ncampaign text\n";
  Db.run ctx.db "UPDATE repos SET readme = ? WHERE key = ?"
    [ Db.text (String.make 10_000 'r'); Db.text main ];
  Db.run ctx.db
    {|INSERT INTO repos(key, host, owner, name, full_name, tier) VALUES
('whitehat:c/tool', 'whitehat', 'c', 'tool', 'tool', 'whitehat')|}
    [];
  let forge = Fake_forge.create () in
  let server = ok_or_fail (Mcp_server.server ctx (Fake_forge.env forge)) in
  let _, send = init_session server in
  let tools =
    match send (req 2 "tools/list" (`Assoc [])) with
    | Some r -> items "tools" (member "result" r)
    | None -> []
  in
  let schema name =
    match List.find_opt (fun t -> to_str (member "name" t) = name) tools with
    | Some t -> member "outputSchema" t
    | None -> `Null
  in
  let query_tools =
    [
      "search";
      "get";
      "list";
      "resolve-library-id";
      "query-docs";
      "list-sources";
      "find-capability";
      "copies";
      "explain";
      "whats-new";
    ]
  in
  List.iter
    (fun n -> Alcotest.(check bool) (n ^ " has outputSchema") true (schema n <> `Null))
    (query_tools
    @ [
        "annotate";
        "record_usage";
        "refresh";
        "register-repo";
        "register-source";
        "unregister-repo";
        "unregister-source";
        "analyze-repo";
      ]);
  let check_valid name args =
    let r = call send name args in
    Alcotest.(check bool) (name ^ " ok") false (member "isError" r = `Bool true);
    let sc = member "structuredContent" r in
    (match Mcp_kit.Schema.validate (schema name) sc with
    | Ok () -> ()
    | Error e ->
        Alcotest.failf "%s: %s" name (Mcp_kit.Schema.validation_errors_to_string e));
    let text =
      match items "content" r with c :: _ -> to_str (member "text" c) | [] -> ""
    in
    (sc, text)
  in
  let sc, text = check_valid "search" (`Assoc [ ("query", `String "ocaml mcp") ]) in
  Alcotest.(check bool)
    "search text names the uri" true
    (contains text "libindex://repo/github/example-org/acme-mcp");
  List.iter
    (fun h -> Alcotest.(check bool) "every hit has a uri" true (member "uri" h <> `Null))
    (items "hits" sc);
  let sc, text = check_valid "get" (`Assoc [ ("repo", `String main) ]) in
  Alcotest.(check bool)
    "get text ≤ 2000 despite a 10 kB README" true
    (Utf8.length text <= 2000);
  Alcotest.(check int)
    "README in structuredContent" 10_000
    (String.length (to_str (member "readme" sc)));
  let doc_uri = to_str (member "uri" (List.hd (items "documents" sc))) in
  let sc, _ = check_valid "list" (`Assoc [ ("limit", `Int 10) ]) in
  List.iter
    (fun r ->
      let whitehat = String.starts_with ~prefix:"whitehat:" (to_str (member "key" r)) in
      Alcotest.(check bool)
        "list rows carry a uri (null for whitehat)" (not whitehat)
        (member "uri" r <> `Null))
    (items "repos" sc);
  let sc, _ =
    check_valid "resolve-library-id" (`Assoc [ ("libraryName", `String "acme-mcp") ])
  in
  Alcotest.(check bool)
    "resolve uri" true
    (List.for_all (fun c -> member "uri" c <> `Null) (items "candidates" sc));
  ignore (check_valid "resolve-library-id" (`Assoc [ ("libraryName", `String "react") ]));
  let sc, _ =
    check_valid "query-docs"
      (`Assoc [ ("libraryId", `String main); ("query", `String "install") ])
  in
  Alcotest.(check string)
    "excerpt uri = document uri" doc_uri
    (to_str (member "uri" (List.hd (items "excerpts" sc))));
  ignore
    (check_valid "query-docs"
       (`Assoc
          [
            ("libraryId", `String mirror);
            ("query", `String "install");
            ("detail", `String "full");
          ]));
  (* Text rendering: ≤ 2000 code points unless detail = full. *)
  let ctx2 = fresh_ctx () in
  let server2 = ok_or_fail (Mcp_server.server ctx2 (Fake_forge.env forge)) in
  let _, send2 = init_session server2 in
  let t args =
    match items "content" (call send2 "search" args) with
    | c :: _ -> to_str (member "text" c)
    | [] -> ""
  in
  let short = t (`Assoc [ ("query", `String "ocaml"); ("limit", `Int 50) ]) in
  Alcotest.(check bool) "≤ 2000 chars" true (Utf8.length short <= 2000);
  let long =
    t
      (`Assoc
         [ ("query", `String "ocaml"); ("limit", `Int 50); ("detail", `String "full") ])
  in
  Alcotest.(check bool)
    "detail=full: complete JSON" true
    (Utf8.length long > 2000 && Yojson.Safe.from_string long <> `Null);
  (* Resource templates. *)
  let templates =
    match send (req 3 "resources/templates/list" (`Assoc [])) with
    | Some r ->
        List.map
          (fun t -> to_str (member "uriTemplate" t))
          (items "resourceTemplates" (member "result" r))
    | None -> []
  in
  Alcotest.(check (list string))
    "two templates"
    [ Liburi.repo_template; Liburi.doc_template ]
    templates;
  let content r =
    match items "contents" (member "result" r) with c :: _ -> c | [] -> `Null
  in
  let c = content (read send "libindex://repo/github/example-org/acme-mcp") in
  Alcotest.(check string) "repo mime" "application/json" (to_str (member "mimeType" c));
  Alcotest.(check string)
    "repo entry" main
    (to_str (member "key" (Yojson.Safe.from_string (to_str (member "text" c)))));
  let c = content (read send doc_uri) in
  Alcotest.(check string) "doc mime" "text/markdown" (to_str (member "mimeType" c));
  Alcotest.(check string) "doc text" F.guide_md (to_str (member "text" c));
  let c =
    content (read send "libindex://doc/github/example-org/acme-mcp/docs/guide.md")
  in
  Alcotest.(check string) "raw slashes accepted" F.guide_md (to_str (member "text" c));
  let c = content (read send "libindex://repo/gitlab/acme%2Fsub/deep") in
  Alcotest.(check string)
    "subgroup owner as one segment" deep
    (to_str (member "key" (Yojson.Safe.from_string (to_str (member "text" c)))));
  List.iter
    (fun uri ->
      let r = read send uri in
      Alcotest.(check bool) (uri ^ " not found") true (member "error" r <> `Null);
      Alcotest.(check bool)
        (uri ^ ": no leak") false
        (contains (Yojson.Safe.to_string r) "campaign text"))
    [
      "libindex://repo/whitehat/c/tool";
      "libindex://repo/github/nobody/nothing";
      "libindex://doc/github/acme-campaigns/camp/docs%2Fsecret.md";
      "libindex://doc/github/example-org/acme-mcp/missing.md";
      "libindex://repo/github/example-org/acme-mcp?x=1";
    ];
  (* Progress (FR-094): a modern request with a progress token gets 1, 2, 3 of 3. *)
  let fn = "example-org/acme-mcp" in
  Fake_forge.json forge ("repos/" ^ fn) (Fake_forge.gh_repo fn);
  let modern =
    Yojson.Safe.from_string
      (Printf.sprintf
         {|{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"refresh","arguments":{"repo":"%s"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{},"progressToken":"tok-1"}}}|}
         main)
  in
  let frames = Mcp_kit.Server.dispatch_json_frames server modern in
  let progress =
    List.filter_map
      (fun f ->
        if member "method" f = `String "notifications/progress" then
          Some
            ( member "progress" (member "params" f),
              member "progressToken" (member "params" f) )
        else None)
      frames
  in
  Alcotest.(check (list string))
    "progress notifications" [ "1.0"; "2.0"; "3.0" ]
    (List.map (fun (p, _) -> Yojson.Safe.to_string p) progress);
  Alcotest.(check bool)
    "token echoed" true
    (List.for_all (fun (_, t) -> t = `String "tok-1") progress);
  (match List.rev frames with
  | last :: _ ->
      Alcotest.(check bool) "final response last" true (member "result" last <> `Null)
  | [] -> Alcotest.fail "no frame");
  (* mcp-kit gap R-9 probe: a legacy (initialize) session ignores progressToken. If this
     ever fails, mcp-kit gained legacy progress: update the spec's R-9. *)
  let sess = Mcp_kit.Server.create_session server in
  ignore
    (Mcp_kit.Server.dispatch_session_json sess
       (Yojson.Safe.from_string
          {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}|}));
  let legacy =
    Yojson.Safe.from_string
      (Printf.sprintf
         {|{"jsonrpc":"2.0","id":10,"method":"tools/call","params":{"name":"refresh","arguments":{"repo":"%s"},"_meta":{"progressToken":"tok-2"}}}|}
         main)
  in
  Alcotest.(check int)
    "legacy session: response only (R-9)" 1
    (List.length (Mcp_kit.Server.dispatch_session_json_frames sess legacy))

let () =
  run "mcp-modern" [ ("surface", [ Alcotest.test_case "AC-29" `Quick modern_mcp ]) ]
