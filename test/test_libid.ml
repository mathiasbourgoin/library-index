(* context7-shaped lookup (S12, spec US-10, US-11): resolve-library-id (CHECK-24), the
   no-third-party scan (CHECK-25) and the optional parameters (CHECK-27). The modern MCP
   surface (CHECK-29) is in test_mcp_modern.ml. *)

open Library_index
open Test_support.Support
module Fake_forge = Test_support.Fake_forge
module F = Test_support.Doc_fixture

let contains s sub = Re.execp (Re.compile (Re.str sub)) s
let items k j = match member k j with `List l -> l | _ -> []
let keys = function `Assoc m -> List.map fst m | _ -> []
let resolve ctx args = ok_or_fail (Ops.resolve_library_id ctx (`Assoc args))

let find_cand id r =
  List.find_opt (fun c -> to_str (member "id" c) = id) (items "candidates" r)

let index_of id r =
  let rec go i = function
    | [] -> -1
    | c :: rest -> if to_str (member "id" c) = id then i else go (i + 1) rest
  in
  go 0 (items "candidates" r)

(* ------------------------------------------------------------ CHECK-24 *)

let resolve_ids () =
  let ctx, main, mirror, fork, user = F.mcp_fixture () in
  let extras =
    F.insert_repo ctx ~owner:"example-user" ~what:"Helpers around the MCP server library"
      "acme-mcp-extras"
  in
  let react_user =
    F.insert_repo ctx ~owner:"example-user" ~what:"React web UI for the notes" "notes-ui"
  in
  let r =
    resolve ctx [ ("libraryName", `String "acme-mcp"); ("query", `String "mcp server") ]
  in
  Alcotest.(check string)
    "maintained GitHub repo first" main
    (to_str (member "id" (List.hd (items "candidates" r))));
  Alcotest.(check bool)
    "GitHub above its GitLab mirror" true
    (index_of main r < index_of mirror r && index_of mirror r >= 0);
  let c = Option.get (find_cand main r) in
  Alcotest.(check (list string))
    "candidate fields"
    [ "id"; "name"; "full_name"; "what"; "maintained_copy"; "score"; "doc_count"; "uri" ]
    (keys c);
  Alcotest.(check string) "what" "MCP server library for OCaml" (to_str (member "what" c));
  Alcotest.(check bool)
    "canonical: no maintained copy" true
    (member "maintained_copy" c = `Null);
  Alcotest.(check int)
    "doc_count" 1
    (match member "doc_count" c with `Int n -> n | _ -> -1);
  Alcotest.(check string)
    "uri" "libindex://repo/github/example-org/acme-mcp"
    (to_str (member "uri" c));
  let mc id = to_str (member "maintained_copy" (Option.get (find_cand id r))) in
  Alcotest.(check string) "mirror → maintained copy" main (mc mirror);
  Alcotest.(check string) "fork → maintained copy" main (mc fork);
  Alcotest.(check bool)
    "name-containing candidate after the exact group" true
    (index_of extras r > index_of fork r);
  Alcotest.(check int) "a description mention is not a candidate" (-1) (index_of user r);
  Alcotest.(check bool) "no hint when found" true (member "hint" r = `Null);
  let r = resolve ctx [ ("libraryName", `String "react") ] in
  Alcotest.(check int)
    ("no candidate (" ^ react_user ^ " only mentions it)")
    0
    (List.length (items "candidates" r));
  Alcotest.(check string)
    "exact hint"
    "not in Ada's index — use context7 resolve-library-id with libraryName=react"
    (to_str (member "hint" r));
  Alcotest.(check string)
    "label without a configured owner" "the library index"
    (Library_index.Library_id.index_label ~display_name:"");
  let r = resolve ctx [ ("libraryName", `String "/example-org/acme-mcp") ] in
  Alcotest.(check string)
    "/owner/name exact" main
    (to_str (member "id" (List.hd (items "candidates" r))));
  (match Ops.resolve_library_id ctx (`Assoc [ ("libraryName", `String "  ") ]) with
  | Error e ->
      Alcotest.(check string) "blank name" "libraryName must be a non-empty string" e
  | Ok _ -> Alcotest.fail "blank name accepted");
  (* Curated maintained_side wins over recency. *)
  ignore
    (ok_or_fail
       (Ops.annotate ctx
          (`Assoc
             [
               ("repo", `String mirror);
               ("fields", `Assoc [ ("maintained_side", `String "gitlab") ]);
             ])));
  let r = resolve ctx [ ("libraryName", `String "acme-mcp") ] in
  Alcotest.(check string)
    "curated side first" mirror
    (to_str (member "id" (List.hd (items "candidates" r))));
  Alcotest.(check string)
    "GitHub now points at the curated copy" mirror
    (to_str (member "maintained_copy" (Option.get (find_cand main r))));
  let r =
    resolve ctx
      [
        ("libraryName", `String "acme-mcp");
        ("filters", `Assoc [ ("host", `String "gitlab") ]);
      ]
  in
  Alcotest.(check (list string))
    "filters apply" [ mirror ]
    (List.map (fun c -> to_str (member "id" c)) (items "candidates" r));
  (* A renamed fork joins the group through its parent: the curated side reaches it. *)
  let lab =
    F.insert_repo ctx ~owner:"example-user" ~fork:true ~parent:"example-org/acme-mcp"
      "mcp-lab"
  in
  let idx = Copies.load ctx.db in
  Alcotest.(check (option string))
    "renamed fork → curated copy" (Some mirror)
    (Copies.maintained_copy idx lab);
  Alcotest.(check (list string))
    "copies of the renamed fork" [ main; fork; mirror ]
    (List.map (fun (c : Copies.row) -> c.key) (Copies.copies idx lab));
  Db.run ctx.db "UPDATE repos SET maintained_side = NULL" [];
  Alcotest.(check (option string))
    "renamed fork → parent (not archived)" (Some main)
    (Copies.maintained_copy (Copies.load ctx.db) lab);
  Db.run ctx.db "UPDATE repos SET archived = 1 WHERE key = ?" [ Db.text main ];
  Alcotest.(check (option string))
    "archived parent: the fork is its own copy" None
    (Copies.maintained_copy (Copies.load ctx.db) lab)

(* ------------------------------------------------------------ CHECK-25 *)

let source_dirs () =
  let ok d =
    Sys.file_exists (Filename.concat d "lib/library_id.ml")
    && Sys.file_exists (Filename.concat d "bin/main.ml")
  in
  match List.find_opt ok [ ".."; "."; Filename.concat exe_dir "../../.." ] with
  | Some root -> [ Filename.concat root "lib"; Filename.concat root "bin" ]
  | None -> Alcotest.fail "cannot locate lib/ and bin/"

let source_files () =
  List.concat_map
    (fun d ->
      Sys.readdir d |> Array.to_list
      |> List.filter (fun f ->
          Filename.check_suffix f ".ml" || Filename.check_suffix f ".mli")
      |> List.map (Filename.concat d))
    (source_dirs ())

let url = Re.compile (Re.Perl.re {|https?://([A-Za-z0-9.-]+)|})

let allowed_host h =
  let h = String.lowercase_ascii h in
  List.exists
    (fun base -> h = base || String.ends_with ~suffix:("." ^ base) h)
    [ "github.com"; "gitlab.com" ]

let no_context7 () =
  let files = source_files () in
  Alcotest.(check bool) "scanned lib/ and bin/" true (List.length files > 40);
  let named = ref [] and hosts = ref [] in
  List.iter
    (fun f ->
      List.iteri
        (fun i line ->
          if contains (String.lowercase_ascii line) "context7" then
            named := Printf.sprintf "%s:%d" (Filename.basename f) (i + 1) :: !named;
          List.iter
            (fun g ->
              let h = Re.Group.get g 1 in
              if not (allowed_host h) then
                hosts :=
                  Printf.sprintf "%s:%d %s" (Filename.basename f) (i + 1) h :: !hosts)
            (Re.all url line))
        (String.split_on_char '\n' (read_file f)))
    files;
  Alcotest.(check (list string))
    "the service is named only by the hint constant" [ "library_id.ml:1" ]
    (List.rev !named);
  Alcotest.(check bool)
    "that line is the constant" true
    (String.starts_with ~prefix:{|let fallback_service = "context7"|}
       (List.hd
          (String.split_on_char '\n'
             (read_file
                (List.find (fun f -> Filename.basename f = "library_id.ml") files)))));
  Alcotest.(check (list string)) "no http(s) host but github.com / gitlab.com" [] !hosts;
  (* FR-110 extension: no model API host, endpoint or SDK either. *)
  Alcotest.(check (list string)) "no model API" [] (model_api_hits files);
  (* The two tools are served, and answering them issues no command at all. *)
  let ctx, main, _, _, _ = F.mcp_fixture () in
  let forge = Fake_forge.create () in
  let server = ok_or_fail (Mcp_server.server ctx (Fake_forge.env forge)) in
  let sess = Mcp_kit.Server.create_session server in
  let send j = Mcp_kit.Server.dispatch_session_json sess (Yojson.Safe.from_string j) in
  ignore
    (send
       {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}|});
  ignore (send {|{"jsonrpc":"2.0","method":"notifications/initialized"}|});
  let tools =
    match send {|{"jsonrpc":"2.0","id":2,"method":"tools/list"}|} with
    | Some r ->
        List.map (fun t -> to_str (member "name" t)) (items "tools" (member "result" r))
    | None -> []
  in
  Alcotest.(check bool)
    "resolve-library-id served" true
    (List.mem "resolve-library-id" tools);
  Alcotest.(check bool) "query-docs served" true (List.mem "query-docs" tools);
  ignore
    (send
       (Printf.sprintf
          {|{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"resolve-library-id","arguments":{"libraryName":"react"}}}|}));
  ignore
    (send
       (Printf.sprintf
          {|{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"query-docs","arguments":{"libraryId":"%s","query":"install"}}}|}
          main));
  Alcotest.(check int)
    "no subprocess, no network" 0
    (List.length (Fake_forge.calls forge))

(* ------------------------------------------------------------ CHECK-27 *)

let core_hit_keys =
  [
    "key";
    "full_name";
    "host";
    "private";
    "tier";
    "status";
    "what";
    "url";
    "score";
    "snippet";
    "uri";
  ]

let optional_params () =
  let ctx = fresh_ctx () in
  let search args = ok_or_fail (Ops.search ctx (`Assoc args)) in
  let q = ("query", `String "ocaml") in
  let core = search [ q; ("limit", `Int 20) ] in
  Alcotest.(check bool)
    "explicit defaults = absent" true
    (core
    = search
        [
          q;
          ("limit", `Int 20);
          ("detail", `String "standard");
          ("filters", `Assoc []);
          ("kind", `Null);
          ("budget_chars", `Null);
        ]);
  Alcotest.(check bool)
    "maintained_only false = absent" true
    (core
    = search
        [
          q; ("limit", `Int 20); ("filters", `Assoc [ ("maintained_only", `Bool false) ]);
        ]);
  List.iter
    (fun h ->
      Alcotest.(check (list string))
        "core hit shape (no documents)" core_hit_keys (keys h))
    (items "hits" core);
  Alcotest.(check bool)
    "no truncated key without a budget" true
    (member "truncated" core = `Null);
  let brief = search [ q; ("detail", `String "brief") ] in
  List.iter
    (fun h ->
      Alcotest.(check (list string))
        "brief"
        [ "key"; "full_name"; "what"; "score"; "uri" ]
        (keys h))
    (items "hits" brief);
  let full = search [ q; ("detail", `String "FULL") ] in
  Alcotest.(check bool)
    "full adds doc_count" true
    (List.for_all (fun h -> member "doc_count" h <> `Null) (items "hits" full));
  let big = search [ q; ("limit", `Int 50) ] in
  let cut = search [ q; ("limit", `Int 50); ("budget_chars", `Int 1000) ] in
  Alcotest.(check bool)
    "budget respected" true
    (Utf8.length (Yojson.Safe.to_string cut) <= 1000);
  Alcotest.(check bool) "truncated" true (member "truncated" cut = `Bool true);
  Alcotest.(check int)
    "count follows the cut"
    (List.length (items "hits" cut))
    (match member "count" cut with `Int n -> n | _ -> -1);
  Alcotest.(check bool)
    "fewer hits" true
    (List.length (items "hits" cut) < List.length (items "hits" big));
  let roomy = search [ q; ("limit", `Int 50); ("budget_chars", `Int 1_000_000) ] in
  Alcotest.(check bool)
    "large budget: not truncated" true
    (member "truncated" roomy = `Bool false);
  Alcotest.(check bool)
    "large budget: same hits" true
    (member "hits" roomy = member "hits" big);
  let err args =
    match Ops.search ctx (`Assoc args) with Error e -> e | Ok _ -> "accepted"
  in
  Alcotest.(check string)
    "invalid detail" "invalid detail 'verbose'; allowed values: brief, standard, full"
    (err [ q; ("detail", `String "verbose") ]);
  Alcotest.(check string)
    "invalid kind" "invalid kind 'x'; allowed values: readme, doc, api"
    (err [ q; ("kind", `String "x") ]);
  Alcotest.(check string)
    "invalid budget" "argument 'budget_chars' must be an integer between 100 and 1000000"
    (err [ q; ("budget_chars", `Int 5) ]);
  Alcotest.(check string)
    "invalid has_docs" "filter 'has_docs' must be a boolean"
    (err [ q; ("filters", `Assoc [ ("has_docs", `String "maybe") ]) ]);
  (* With documents: excerpts, has_docs, kind, maintained_only. *)
  let ctx, main, mirror, _, _ = F.mcp_fixture () in
  let search args = ok_or_fail (Ops.search ctx (`Assoc args)) in
  let hit k r = List.find_opt (fun h -> to_str (member "key" h) = k) (items "hits" r) in
  (* Repository hits come from the repository index; the excerpt from its documents. *)
  let r = search [ ("query", `String "mcp server install") ] in
  let ex = member "excerpt" (Option.get (hit main r)) in
  Alcotest.(check string) "excerpt title" "Installing" (to_str (member "title" ex));
  Alcotest.(check string)
    "excerpt uri" "libindex://doc/github/example-org/acme-mcp/docs%2Fguide.md"
    (to_str (member "uri" ex));
  let r = search [ ("query", `String "mcp server install"); ("kind", `String "api") ] in
  Alcotest.(check bool)
    "kind filters the excerpt" true
    (match hit main r with Some h -> member "excerpt" h = `Null | None -> true);
  let ids r = List.map (fun h -> to_str (member "key" h)) (items "hits" r) in
  let r =
    search
      [ ("query", `String "ocaml mcp"); ("filters", `Assoc [ ("has_docs", `Bool true) ]) ]
  in
  Alcotest.(check (list string)) "has_docs" [ main ] (ids r);
  let r =
    search
      [
        ("query", `String "ocaml mcp"); ("filters", `Assoc [ ("has_docs", `Bool false) ]);
      ]
  in
  Alcotest.(check bool) "has_docs false" false (List.mem main (ids r));
  let r =
    search
      [
        ("query", `String "ocaml mcp");
        ("filters", `Assoc [ ("maintained_only", `Bool true) ]);
      ]
  in
  Alcotest.(check bool) "maintained_only keeps the canonical" true (List.mem main (ids r));
  Alcotest.(check bool) "maintained_only drops the mirror" false (List.mem mirror (ids r));
  let l = ok_or_fail (Ops.list ctx (`Assoc [ ("maintained_only", `Bool true) ])) in
  Alcotest.(check bool)
    "list honours maintained_only" false
    (List.exists (fun r -> to_str (member "key" r) = mirror) (items "repos" l));
  let d args =
    ok_or_fail (Ops.query_docs ctx (`Assoc (("libraryId", `String main) :: args)))
  in
  let b = d [ ("query", `String "install"); ("detail", `String "brief") ] in
  Alcotest.(check (list string))
    "query-docs brief" [ "title"; "path"; "uri" ]
    (keys (List.hd (items "excerpts" b)));
  let f = d [ ("query", `String "install"); ("detail", `String "full") ] in
  Alcotest.(check bool)
    "query-docs full text" true
    (contains (to_str (member "text" (List.hd (items "excerpts" f)))) "opam install demo");
  let k = d [ ("query", `String "install"); ("kind", `String "readme") ] in
  Alcotest.(check int) "query-docs kind" 0 (List.length (items "excerpts" k));
  let r =
    resolve ctx [ ("libraryName", `String "acme-mcp"); ("detail", `String "brief") ]
  in
  Alcotest.(check (list string))
    "resolve brief"
    [ "id"; "name"; "maintained_copy"; "uri" ]
    (keys (List.hd (items "candidates" r)));
  let r =
    resolve ctx
      [
        ("libraryName", `String "acme-mcp");
        ("filters", `Assoc [ ("maintained_only", `Bool true) ]);
      ]
  in
  Alcotest.(check (list string))
    "resolve maintained_only" [ main ]
    (List.filter
       (fun i -> i <> mirror)
       [ to_str (member "id" (List.hd (items "candidates" r))) ])

let () =
  run "libid"
    [
      ("resolve", [ Alcotest.test_case "AC-24" `Quick resolve_ids ]);
      ("no-context7", [ Alcotest.test_case "AC-25" `Quick no_context7 ]);
      ("optional-params", [ Alcotest.test_case "AC-27" `Quick optional_params ]);
    ]
