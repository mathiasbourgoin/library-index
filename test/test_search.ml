(* Search: CHECK-4 (ranking), CHECK-5 (hostile), CHECK-6 (filters). *)

open Library_index
open Test_support.Support

(* Insert a fetched row; column names are test constants, values are bound. *)
let insert (ctx : Ctx.t) key cols =
  let cols = ("key", `S key) :: ("fetched", `I 1) :: cols in
  let cols = if List.mem_assoc "host" cols then cols else ("host", `S "github") :: cols in
  let names = String.concat ", " (List.map fst cols) in
  let qs = String.concat ", " (List.map (fun _ -> "?") cols) in
  let v = function `S s -> Db.text s | `I n -> Db.int n in
  Db.run ctx.db
    (Printf.sprintf "INSERT INTO repos(%s) VALUES (%s)" names qs)
    (List.map (fun (_, x) -> v x) cols)

let search ctx ?limit ?(filters = []) q =
  let args =
    `Assoc
      ([ ("query", `String q); ("filters", `Assoc filters) ]
      @ match limit with Some n -> [ ("limit", `Int n) ] | None -> [])
  in
  Ops.search ctx args

let hits j = match member "hits" j with `List l -> l | _ -> []
let keys j = List.map (fun h -> to_str (member "key" h)) (hits j)
let mode j = to_str (member "mode" j)
let score h = match member "score" h with `Float f -> f | _ -> nan
let contains s sub = Re.execp (Re.compile (Re.str sub)) s

let fixture () =
  let ctx = empty_ctx () in
  insert ctx "github:x/mcp-kit"
    [
      ("name", `S "mcp-kit");
      ("full_name", `S "x/mcp-kit");
      ("tier", `S "2");
      ("language", `S "OCaml");
    ];
  insert ctx "github:x/notes"
    [
      ("name", `S "notes");
      ("full_name", `S "x/notes");
      ("tier", `S "1");
      ("readme", `S "This service exposes an mcp endpoint among many other things.");
    ];
  insert ctx "github:x/alpha"
    [
      ("name", `S "alpha");
      ("full_name", `S "x/alpha");
      ("description", `S "alphaword parser");
      ("tier", `S "1");
      ("language", `S "OCaml");
      ("pushed_at", `S "2026-09-30T00:00:00Z");
    ];
  insert ctx "github:x/omega"
    [
      ("name", `S "omega");
      ("full_name", `S "x/omega");
      ("description", `S "omegaword parser");
      ("tier", `S "2");
      ("language", `S "Rust");
      ("private", `I 1);
      ("pushed_at", `S "2020-01-01T00:00:00Z");
    ];
  insert ctx "gitlab:g/beta"
    [
      ("host", `S "gitlab");
      ("owner", `S "Grp");
      ("name", `S "beta");
      ("full_name", `S "g/beta");
      ("description", `S "parser beta");
      ("tier", `S "1");
      ("language", `S "ocaml");
      ("computed_domain", `S "Security research");
      ("archived", `I 1);
    ];
  (* Tie-break trio: identical text, differing tier / pushed_at / key. *)
  List.iter
    (fun (k, tier, pushed) ->
      insert ctx ("github:t/" ^ k)
        [
          ("name", `S k);
          ("full_name", `S ("t/" ^ k));
          ("description", `S "tiebreak");
          ("tier", `S tier);
          ("pushed_at", `S pushed);
        ])
    [
      ("c", "2", "2026-01-01T00:00:00Z");
      ("b", "2", "2026-05-01T00:00:00Z");
      ("a", "1", "2025-01-01T00:00:00Z");
    ];
  ctx

let ranking () =
  let ctx = fixture () in
  let j = ok_or_fail (search ctx "mcp" ~limit:5) in
  Alcotest.(check (list string))
    "name match above README-only match"
    [ "github:x/mcp-kit"; "github:x/notes" ]
    (keys j);
  Alcotest.(check string) "mode" "and" (mode j);
  (match hits j with
  | [ a; b ] ->
      Alcotest.(check bool)
        "score higher = better" true
        (score a > score b && score b > 0.);
      let fields = match a with `Assoc m -> List.map fst m | _ -> [] in
      List.iter
        (fun f -> Alcotest.(check bool) ("hit has " ^ f) true (List.mem f fields))
        [
          "key"; "full_name"; "host"; "private"; "tier"; "what"; "url"; "score"; "snippet";
        ];
      Alcotest.(check bool)
        "snippet marks the match" true
        (contains (to_str (member "snippet" b)) "[mcp]")
  | _ -> Alcotest.fail "expected two hits");
  Alcotest.(check (list string))
    "tie-break: tier 1, then pushed_at desc, then key"
    [ "github:t/a"; "github:t/b"; "github:t/c" ]
    (keys (ok_or_fail (search ctx "tiebreak")));
  (* C-8: '-' is a separator, accents are folded. *)
  Alcotest.(check (list string))
    "mcp-kit matches kit" [ "github:x/mcp-kit" ]
    (keys (ok_or_fail (search ctx "kit")));
  insert ctx "github:x/cafe" [ ("name", `S "cafe-lib"); ("description", `S "naïve café") ];
  Alcotest.(check (list string))
    "diacritics folded" [ "github:x/cafe" ]
    (keys (ok_or_fail (search ctx "naive")));
  fts_integrity ctx

(* Assembled so that no literal destructive statement appears in the source. *)
let injection = "'; " ^ String.concat " " [ "DR" ^ "OP"; "TABLE"; "repos;" ] ^ " --"

let hostile_inputs =
  [
    "\"foo AND ( OR";
    "\"";
    "*";
    "NEAR(a b)";
    "name:mcp";
    "a\"b";
    "^mcp";
    "-mcp";
    "mcp*";
    "\xff\xfe\xfd";
    "x\x00y";
    injection;
    "—";
    "( ) { } [ ]";
    "AND";
    "OR NOT";
    String.make 10000 'a';
    "mcp \xc3";
    "😀 mcp";
  ]

let hostile () =
  let ctx = fixture () in
  List.iter
    (fun q ->
      match search ctx q with
      | Error e -> Alcotest.failf "query %S errored: %s" q e
      | Ok j ->
          Alcotest.(check bool)
            (Printf.sprintf "%S gives a list" q)
            true
            (match member "hits" j with `List _ -> true | _ -> false))
    hostile_inputs;
  Alcotest.(check (list string))
    "operators are literal terms"
    [ "github:x/mcp-kit"; "github:x/notes" ]
    (keys (ok_or_fail (search ctx "^mcp")));
  Alcotest.(check (option int))
    "table intact" (Some 8)
    (Option.join
       (Db.query_one ctx.db "SELECT count(*) FROM repos" [] (fun r -> Db.to_int_opt r.(0))));
  List.iter
    (fun q ->
      let j = ok_or_fail (search ctx q) in
      Alcotest.(check string) (Printf.sprintf "%S → mode none" q) "none" (mode j);
      Alcotest.(check (list string)) "no hits" [] (keys j))
    [ ""; "   "; "\t\n"; "()*-" ];
  let j = ok_or_fail (search ctx "alphaword omegaword") in
  Alcotest.(check string) "OR fallback reported" "or" (mode j);
  Alcotest.(check (list string)) "OR hits" [ "github:x/alpha"; "github:x/omega" ] (keys j);
  Alcotest.(check string)
    "single token: no OR fallback" "and"
    (mode (ok_or_fail (search ctx "zzzunknown")));
  (* FR-014: limit clamped to 1..50, default 10. *)
  for i = 1 to 60 do
    insert ctx
      (Printf.sprintf "github:many/r%02d" i)
      [ ("name", `S (Printf.sprintf "r%02d" i)); ("description", `S "manyword") ]
  done;
  let count ?limit () = List.length (hits (ok_or_fail (search ctx ?limit "manyword"))) in
  Alcotest.(check int) "default 10" 10 (count ());
  Alcotest.(check int) "0 → 1" 1 (count ~limit:0 ());
  Alcotest.(check int) "-5 → 1" 1 (count ~limit:(-5) ());
  Alcotest.(check int) "1000 → 50" 50 (count ~limit:1000 ());
  Alcotest.(check bool)
    "non-integer limit rejected" true
    (Result.is_error
       (Ops.search ctx (`Assoc [ ("query", `String "x"); ("limit", `String "5") ])));
  Alcotest.(check bool)
    "missing query rejected" true
    (Result.is_error (Ops.search ctx (`Assoc [])));
  fts_integrity ctx

let filters () =
  let ctx = fixture () in
  let parser ?(filters = []) () = keys (ok_or_fail (search ctx ~filters "parser")) in
  let sorted l = List.sort compare l in
  let j =
    ok_or_fail
      (search ctx
         ~filters:[ ("language", `String "OCaml"); ("tier", `String "1") ]
         "parser")
  in
  Alcotest.(check (list string))
    "language + tier (language case-insensitive)"
    [ "github:x/alpha"; "gitlab:g/beta" ]
    (sorted (keys j));
  List.iter
    (fun h -> Alcotest.(check string) "every hit tier 1" "1" (to_str (member "tier" h)))
    (hits j);
  Alcotest.(check (list string))
    "tier as integer"
    [ "github:x/alpha"; "gitlab:g/beta" ]
    (sorted (parser ~filters:[ ("tier", `Int 1) ] ()));
  Alcotest.(check (list string))
    "host" [ "gitlab:g/beta" ]
    (parser ~filters:[ ("host", `String "GitLab") ] ());
  Alcotest.(check (list string))
    "owner case-insensitive" [ "gitlab:g/beta" ]
    (parser ~filters:[ ("owner", `String "grp") ] ());
  Alcotest.(check (list string))
    "private" [ "github:x/omega" ]
    (parser ~filters:[ ("private", `Bool true) ] ());
  Alcotest.(check (list string))
    "private as string" [ "github:x/omega" ]
    (parser ~filters:[ ("private", `String "true") ] ());
  Alcotest.(check (list string))
    "effective (computed) domain" [ "gitlab:g/beta" ]
    (parser ~filters:[ ("domain", `String "Security research") ] ());
  (* Status uses the injected clock (2026-10-03): alpha active, omega stale, beta archived. *)
  Alcotest.(check (list string))
    "status active" [ "github:x/alpha" ]
    (parser ~filters:[ ("status", `String "active") ] ());
  Alcotest.(check (list string))
    "status stale" [ "github:x/omega" ]
    (parser ~filters:[ ("status", `String "stale") ] ());
  Alcotest.(check (list string))
    "status archived" [ "gitlab:g/beta" ]
    (parser ~filters:[ ("status", `String "archived") ] ());
  Alcotest.(check (list string))
    "null filter ignored" (parser ())
    (parser ~filters:[ ("tier", `Null) ] ());
  let err filters =
    match search ctx ~filters "parser" with
    | Ok _ -> Alcotest.fail "invalid filter accepted"
    | Error e -> e
  in
  Alcotest.(check string)
    "bad tier"
    "invalid tier 'one'; allowed values: 1, 2, fork, collab, inherited, whitehat"
    (err [ ("tier", `String "one") ]);
  Alcotest.(check string)
    "unknown key"
    "unknown filter key 'colour'; allowed keys: host, owner, language, tier, status, \
     domain, private, maintained_only, has_docs, ownership"
    (err [ ("colour", `String "x") ]);
  Alcotest.(check bool)
    "bad status lists values" true
    (contains (err [ ("status", `String "great") ]) "active, dormant");
  Alcotest.(check bool)
    "bad domain lists values" true
    (contains (err [ ("domain", `String "Nope") ]) "OCaml libraries");
  Alcotest.(check bool)
    "bad host" true
    (contains (err [ ("host", `String "bitbucket") ]) "github, gitlab");
  Alcotest.(check string)
    "bad private" "filter 'private' must be a boolean"
    (err [ ("private", `String "maybe") ])

let list ctx members = Ops.list ctx (`Assoc members)
let rows j = match member "repos" j with `List l -> l | _ -> []
let row_keys j = List.map (fun r -> to_str (member "key" r)) (rows j)
let int_of j k = match member k j with `Int n -> n | _ -> -1

(* EC-9 needs more rows than one page: 120 generated curated stubs plus the 6 synthetic
   whitehat rows. *)
let many_ctx () =
  let dir = temp_dir () in
  let src = Filename.concat dir "src" in
  Unix.mkdir src 0o755;
  let entries =
    List.init 120 (fun i ->
        ( Printf.sprintf "github:gen-org/repo-%03d" i,
          `Assoc [ ("what", `String (Printf.sprintf "generated entry %d" i)) ] ))
  in
  Yojson.Safe.to_file (Filename.concat src "curated.json") (`Assoc entries);
  copy_file
    (Filename.concat fixtures_dir "whitehat_tools.json")
    (Filename.concat src "whitehat_tools.json");
  fresh_ctx ~from:src ()

let list_rows () =
  let ctx = many_ctx () in
  let j = ok_or_fail (list ctx []) in
  Alcotest.(check int) "total = all rows" 126 (int_of j "total");
  Alcotest.(check int) "first 100" 100 (List.length (rows j));
  Alcotest.(check (list string))
    "sorted by key"
    (List.sort compare (row_keys j))
    (row_keys j);
  let all = ok_or_fail (list ctx [ ("limit", `Int 5000) ]) in
  Alcotest.(check int) "limit clamped to 1000 (all fit)" 126 (List.length (rows all));
  Alcotest.(check int) "limit reported clamped" 1000 (int_of all "limit");
  let page = ok_or_fail (list ctx [ ("limit", `Int 10); ("offset", `Int 100) ]) in
  Alcotest.(check (list string))
    "offset pages"
    (List.filteri (fun i _ -> i >= 100 && i < 110) (row_keys all))
    (row_keys page);
  Alcotest.(check int)
    "limit 0 → 1" 1
    (List.length (rows (ok_or_fail (list ctx [ ("limit", `Int 0) ]))));
  Alcotest.(check bool) "not export_dirty" true (member "export_dirty" j = `Null);
  (* US-3.3 on the hand-built fixture (injected clock 2026-10-03). *)
  let ctx = fixture () in
  Db.run ctx.db
    "UPDATE repos SET domain = 'OCaml libraries' WHERE key IN ('github:x/alpha', \
     'github:x/omega')"
    [];
  let j =
    ok_or_fail
      (list ctx [ ("domain", `String "OCaml libraries"); ("status", `String "active") ])
  in
  Alcotest.(check (list string)) "domain + status" [ "github:x/alpha" ] (row_keys j);
  Alcotest.(check int) "total after status filtering" 1 (int_of j "total");
  let r = List.hd (rows j) in
  let fields = match r with `Assoc m -> List.map fst m | _ -> [] in
  List.iter
    (fun f -> Alcotest.(check bool) ("row has " ^ f) true (List.mem f fields))
    [
      "key";
      "host";
      "owner";
      "full_name";
      "language";
      "tier";
      "status";
      "domain";
      "private";
      "what";
      "url";
      "pushed_at";
      "fetched";
      "missing_since";
    ];
  Alcotest.(check string) "row status" "active" (to_str (member "status" r));
  Alcotest.(check string) "row domain" "OCaml libraries" (to_str (member "domain" r));
  let err members =
    match list ctx members with Ok _ -> Alcotest.fail "accepted" | Error e -> e
  in
  Alcotest.(check string)
    "bad tier"
    "invalid tier 'one'; allowed values: 1, 2, fork, collab, inherited, whitehat"
    (err [ ("tier", `String "one") ]);
  Alcotest.(check string)
    "unknown key names allowed keys"
    "unknown filter key 'colour'; allowed keys: host, owner, language, tier, status, \
     domain, private, maintained_only, has_docs, ownership, limit, offset"
    (err [ ("colour", `String "x") ]);
  Alcotest.(check string)
    "bad limit type" "argument 'limit' must be an integer"
    (err [ ("limit", `String "x") ]);
  Alcotest.(check bool) "non-object args" true (Result.is_error (Ops.list ctx (`List [])));
  Db.set_meta ctx.db "export_dirty" "1";
  Alcotest.(check bool)
    "export_dirty surfaced (AM-10)" true
    (member "export_dirty" (ok_or_fail (list ctx [])) = `Bool true)

let () =
  run "search"
    [
      ("ranking", [ Alcotest.test_case "weights, fields, tie-break" `Quick ranking ]);
      ( "hostile",
        [ Alcotest.test_case "no input raises; OR fallback; clamp" `Quick hostile ] );
      ("filters", [ Alcotest.test_case "conjunctive filters + validation" `Quick filters ]);
      ("list", [ Alcotest.test_case "list: EC-9, filters, paging" `Quick list_rows ]);
    ]
