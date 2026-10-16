(* Storage and migration (synthetic fixtures; the real b70f23d data is checked by
   migration/check-roundtrip.sh, MIG-1): CHECK-1 (roundtrip), CHECK-2 (init-refuses), CHECK-3 (stubs),
   CHECK-17 (schema), plus EC-1/EC-2/EC-3 (edge-cases). *)

open Library_index
open Test_support.Support

let curated_fixture () =
  ok_or_fail (Curated_io.read_json (Filename.concat fixtures_dir "curated.json"))

let roundtrip () =
  let ctx = fresh_ctx () in
  let out = temp_dir () in
  ok_or_fail (Export.export ctx ~dir:out);
  List.iter
    (fun f ->
      let a = ok_or_fail (Curated_io.read_json (Filename.concat fixtures_dir f)) in
      let b = ok_or_fail (Curated_io.read_json (Filename.concat out f)) in
      Alcotest.(check bool)
        (f ^ " parsed-equal to the input")
        true (Yojson.Safe.equal a b))
    [ Export.curated_file; Export.whitehat_file ];
  let n =
    Db.query ctx.db "SELECT count(*) FROM repos WHERE host = 'whitehat'" [] (fun r ->
        Db.to_int_opt r.(0))
  in
  Alcotest.(check (list (option int))) "6 whitehat rows" [ Some 6 ] n;
  (* Every curated key's fields come back unchanged through get. *)
  (match curated_fixture () with
  | `Assoc members ->
      List.iter
        (fun (k, v) ->
          if k <> "_doc" then
            let got = ok_or_fail (Repo_view.get ctx k) in
            List.iter
              (fun f ->
                let expected = member f v in
                Alcotest.(check string)
                  (k ^ "." ^ f)
                  (Yojson.Safe.to_string expected)
                  (Yojson.Safe.to_string (member f got)))
              [ "domain"; "what"; "interest"; "consumers" ])
        members
  | _ -> Alcotest.fail "fixture is not an object");
  (* _doc is meta, never a row. *)
  Alcotest.(check bool)
    "_doc not a repo" true
    (Result.is_error (Repo_view.get ctx "_doc"));
  fts_integrity ctx

let init_refuses () =
  let ctx = fresh_ctx () in
  let before = read_file ctx.db_path in
  Db.close ctx.db;
  (match Init.init ~id ~db_path:ctx.db_path ~from_dir:fixtures_dir ~force:false () with
  | Ok _ -> Alcotest.fail "second init without --force succeeded"
  | Error e ->
      Alcotest.(check bool)
        "message mentions --force" true
        (Re.execp (Re.compile (Re.str "--force")) e));
  Alcotest.(check bool)
    "DB file unchanged" true
    (String.equal before (read_file ctx.db_path));
  (* With --force the DB is recreated (fetched data lost, curated data back). *)
  let db = ok_or_fail (Db.open_existing ctx.db_path) in
  Db.run db "UPDATE repos SET readme = 'x' WHERE key = 'local:sample-lib'" [];
  Db.close db;
  ignore
    (ok_or_fail
       (Init.init ~id ~db_path:ctx.db_path ~from_dir:fixtures_dir ~force:true ()));
  let ctx2 = open_ctx ctx.db_path in
  let got = ok_or_fail (Repo_view.get ctx2 "local:sample-lib") in
  Alcotest.(check string)
    "readme reset by --force" "null"
    (Yojson.Safe.to_string (member "readme" got))

let check_stub ctx key ~host ~owner =
  let got = ok_or_fail (Repo_view.get ctx key) in
  Alcotest.(check string) (key ^ " key") key (to_str (member "key" got));
  Alcotest.(check string) (key ^ " host") host (to_str (member "host" got));
  Alcotest.(check string) (key ^ " owner") owner (to_str (member "owner" got));
  Alcotest.(check string) (key ^ " fetched") "false" (to_str (member "fetched" got));
  Alcotest.(check string) (key ^ " tier") "null" (to_str (member "tier" got));
  Alcotest.(check bool) (key ^ " has what") true (member "what" got <> `Null);
  got

let stubs () =
  let ctx = fresh_ctx () in
  let got = check_stub ctx "local:sample-lib" ~host:"local" ~owner:"(local)" in
  Alcotest.(check string)
    "local stub domain" "OCaml libraries"
    (to_str (member "domain" got));
  ignore
    (check_stub ctx "github:example-org/acme-mcp" ~host:"github" ~owner:"example-org");
  (* AM-17: a stub's campaign flag comes from its owner. *)
  let flag k =
    Db.query_one ctx.db "SELECT campaign FROM repos WHERE key = ?"
      [ Db.text k ]
      (fun r -> Db.to_int_opt r.(0))
  in
  Alcotest.(check bool)
    "plain stub not campaign" true
    (flag "local:sample-lib" = Some (Some 0));
  (match
     Db.query ctx.db
       "SELECT key FROM repos WHERE host = 'gitlab' AND lower(owner) LIKE \
        'acme-campaigns%' AND campaign = 0"
       [] Fun.id
   with
  | [] -> ()
  | _ -> Alcotest.fail "a acme-campaigns stub is not flagged campaign");
  (* The self-entry is ordinary data (FR-102): any curated key becomes a stub. The real
     self-entry is checked against the private data by migration/check-roundtrip.sh. *)
  ignore
    (check_stub ctx "github:example-user/library-index" ~host:"github"
       ~owner:"example-user")

let schema () =
  let ctx = fresh_ctx () in
  let cols =
    Db.query ctx.db "SELECT name, type, pk FROM pragma_table_info('repos')" [] (fun r ->
        (Option.get (Db.to_string_opt r.(0)), Db.to_string_opt r.(1), Db.to_int_opt r.(2)))
  in
  let find c = List.find_opt (fun (n, _, _) -> n = c) cols in
  Alcotest.(check bool)
    "id INTEGER PRIMARY KEY" true
    (find "id" = Some ("id", Some "INTEGER", Some 1));
  Alcotest.(check bool) "readme_sha256 column" true (find "readme_sha256" <> None);
  Alcotest.(check bool)
    "campaign flag column (AM-17)" true
    (find "campaign" = Some ("campaign", Some "INTEGER", Some 0));
  List.iter
    (fun t ->
      let n =
        Db.query ctx.db "SELECT count(*) FROM sqlite_master WHERE name = ?"
          [ Db.text t ]
          (fun r -> Db.to_int_opt r.(0))
      in
      Alcotest.(check (list (option int))) ("table " ^ t) [ Some 1 ] n)
    [
      "repos";
      "usage";
      "meta";
      "local_checkouts";
      "gaps";
      "repos_fts";
      "repos_ai";
      "repos_ad";
      "repos_au";
      "api_items";
      "api_files";
      "api_items_fts";
      "api_items_ai";
      "api_items_ad";
      "api_items_au";
      "api_calls";
      "analysis";
    ];
  Alcotest.(check (option string))
    "schema_version" (Some "4")
    (Db.get_meta ctx.db "schema_version");
  (* ids are stable: an update keeps the id. *)
  let id () =
    Db.query ctx.db "SELECT id FROM repos WHERE key = 'local:sample-lib'" [] (fun r ->
        Db.to_int_opt r.(0))
  in
  let before = id () in
  Db.run ctx.db "UPDATE repos SET notes = 'n' WHERE key = 'local:sample-lib'" [];
  Alcotest.(check (list (option int))) "stable id" before (id ());
  fts_integrity ctx

let write_src dir curated =
  let src = Filename.concat dir "src" in
  Unix.mkdir src 0o755;
  let oc = open_out_bin (Filename.concat src "curated.json") in
  output_string oc curated;
  close_out oc;
  copy_file
    (Filename.concat fixtures_dir "whitehat_tools.json")
    (Filename.concat src "whitehat_tools.json");
  src

let edge_cases () =
  (* EC-1: unknown host prefix → error naming the key; nothing written. *)
  let dir = temp_dir () in
  let src = write_src dir {|{"bitbucket:x/y": {"what": "w"}}|} in
  let db_path = Filename.concat dir "t.db" in
  (match Init.init ~id ~db_path ~from_dir:src ~force:false () with
  | Ok _ -> Alcotest.fail "unknown host accepted"
  | Error e ->
      Alcotest.(check bool)
        "names the key" true
        (Re.execp (Re.compile (Re.str "bitbucket:x/y")) e));
  Alcotest.(check bool) "no DB written (EC-1/EC-3)" false (Sys.file_exists db_path);
  (* EC-2: unknown field and odd-typed field preserved and re-exported. *)
  let dir = temp_dir () in
  let curated =
    {|{"github:a/b": {"what": "w", "typo_field": {"x": [1, 2]}, "interest": 3}}|}
  in
  let src = write_src dir curated in
  let db_path = Filename.concat dir "t.db" in
  ignore (ok_or_fail (Init.init ~id ~db_path ~from_dir:src ~force:false ()));
  let ctx = open_ctx db_path in
  let out = temp_dir () in
  ok_or_fail (Export.export ctx ~dir:out);
  let got = ok_or_fail (Curated_io.read_json (Filename.concat out "curated.json")) in
  Alcotest.(check bool)
    "extra fields round-trip" true
    (Yojson.Safe.equal got (Yojson.Safe.from_string curated));
  (* EC-3: an invalid whitehat file aborts init before any DB is created. *)
  let dir = temp_dir () in
  let src = write_src dir "{}" in
  let oc = open_out_bin (Filename.concat src "whitehat_tools.json") in
  output_string oc {|{"tools": [{"campaign": "c"}]}|};
  close_out oc;
  let db_path = Filename.concat dir "t.db" in
  Alcotest.(check bool)
    "invalid whitehat rejected" true
    (Result.is_error (Init.init ~id ~db_path ~from_dir:src ~force:false ()));
  Alcotest.(check bool) "no DB left" false (Sys.file_exists db_path)

let () =
  run "migration"
    [
      ("roundtrip", [ Alcotest.test_case "init + export = inputs" `Quick roundtrip ]);
      ( "init-refuses",
        [ Alcotest.test_case "second init exits with error" `Quick init_refuses ] );
      ("stubs", [ Alcotest.test_case "curated-only stubs" `Quick stubs ]);
      ("schema", [ Alcotest.test_case "id + readme_sha256 + tables" `Quick schema ]);
      ("edge-cases", [ Alcotest.test_case "EC-1 EC-2 EC-3" `Quick edge_cases ]);
    ]
