(* Review round 3 hardening, part 2 (CHECK-39, CHECK-41): credentials never reach
   sources.toml or an output, the sources.toml editor keeps comments and symlinks, and the
   smaller fixes (init, export, classification, ownership filters). Names are fictional. *)

open Library_index
open Test_support.Support
open Test_support.Hardening_fixture
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

(* ------------------------------------------------------------ HIGH 4: credentials *)

let token = "ghp_AbCdEfGhIjKlMnOpQrStUvWxYz0123456789"

let token_url () =
  let ctx = data_ctx () in
  let forge = Fake_forge.create () in
  Fake_forge.json forge "repos/example-user/lib" (Fake_forge.gh_repo "example-user/lib");
  let url = "https://x-access-token:" ^ token ^ "@github.com/example-user/lib" in
  let o =
    ok_or_fail
      (Ops.register_repo ctx (F.mixed_env forge) (`Assoc [ ("target", `String url) ]))
  in
  let text = config_text ctx in
  Alcotest.(check bool) "no token in sources.toml" false (contains text token);
  Alcotest.(check bool)
    "canonical target" true
    (contains text "target = \"https://github.com/example-user/lib\"");
  Alcotest.(check bool)
    "no token in result" false
    (contains (Yojson.Safe.to_string o.json) token);
  let l = ok_or_fail (Ops.list_sources ctx (`Assoc [])) in
  Alcotest.(check bool) "no token listed" false (contains (Yojson.Safe.to_string l) token);
  let r = ok_or_fail (Ops.unregister_repo ctx (`Assoc [ ("target", `String url) ])) in
  Alcotest.(check string) "same key" "github:example-user/lib" (to_str (member "key" r))

(* ------------------------------------------------------------ config editor *)

let editor_keeps_comments_and_links () =
  let text =
    "[[repo]]\n\
     target = \"https://github.com/o/a\"   # where it lives\n\
     # a note inside the entry\n\
     key = \"github:o/a\"\n\
     confidential = \"none\"\n"
  in
  let r : Config.repo =
    {
      target = "https://github.com/o/a";
      key = Some "github:o/a";
      ownership = None;
      own = false;
      confidential = Not_confidential;
      campaign = false;
      campaign_prefix = false;
      deep = true;
    }
  in
  let t, _ =
    ok_or_fail
      (Config_edit.upsert text Repo
         ~pred:(function `Repo r -> r.key = Some "github:o/a" | `Source _ -> false)
         ~block:(Config_edit.render_repo r))
  in
  Alcotest.(check bool) "inner comment kept" true (contains t "# a note inside the entry");
  Alcotest.(check bool) "trailing comment kept" true (contains t "# where it lives");
  Alcotest.(check bool) "new key" true (contains t "deep = true");
  let dir = temp_dir () in
  let real = Filename.concat dir "real.toml"
  and link = Filename.concat dir "sources.toml" in
  F.write_file real "a = 1\n";
  Unix.chmod real 0o640;
  Unix.symlink real link;
  ok_or_fail (Config_edit.write_atomic link "b = 2\n");
  Alcotest.(check bool) "still a link" true ((Unix.lstat link).st_kind = Unix.S_LNK);
  Alcotest.(check string) "target written" "b = 2\n" (read_file real);
  Alcotest.(check int) "mode kept" 0o640 (Unix.stat real).st_perm

(* ------------------------------------------------------------ small fixes *)

let init_creates_dir () =
  let root = temp_dir () in
  let data = Filename.concat root "fresh/library-index" in
  let db_path = Filename.concat data "library-index.db" in
  let s = ok_or_fail (Init.init ~id ~db_path ~from_dir:data ~force:false ()) in
  Alcotest.(check int) "empty import" 0 s.curated;
  Alcotest.(check bool) "created" true (Sys.file_exists db_path);
  match
    Init.init ~id
      ~db_path:(Filename.concat root "x/y.db")
      ~from_dir:"/nonexistent-dir" ~force:false ()
  with
  | Ok _ -> Alcotest.fail "a missing --from was accepted"
  | Error e ->
      Alcotest.(check bool) "names the directory" true (contains e "/nonexistent-dir")

let export_missing_dir () =
  let ctx = fresh_ctx () in
  match Export.export ctx ~dir:"/nonexistent-export-dir" with
  | Ok () -> Alcotest.fail "exported to a missing directory"
  | Error e -> Alcotest.(check bool) "actionable" true (contains e "does not exist")

let offline_is_network () =
  Alcotest.(check bool)
    "gh offline" true
    (Proc.classify_failure "error connecting to api.github.com" = `Network)

let ownership_filters () =
  let ctx = empty_ctx () in
  let own =
    F.insert_repo ctx ~owner:"example-user" ~last_commit:"2026-10-02T00:00:00Z" "kit"
  in
  let ext =
    F.insert_repo ctx ~owner:"outside" ~last_commit:"2026-10-02T00:00:00Z" "kit"
  in
  Db.run ctx.db "UPDATE repos SET ownership_override = 'own' WHERE key = ?"
    [ Db.text own ];
  Db.run ctx.db "UPDATE repos SET ownership_override = 'external' WHERE key = ?"
    [ Db.text ext ];
  let keys j =
    List.map
      (fun r -> to_str (member "key" r))
      (match member "repos" j with `List l -> l | _ -> [])
  in
  let w =
    ok_or_fail
      (Ops.whats_new ctx
         (`Assoc [ ("since", `String "2026-10-01"); ("ownership", `String "external") ]))
  in
  Alcotest.(check (list string)) "whats-new external" [ ext ] (keys w);
  let c =
    ok_or_fail
      (Ops.copies ctx (`Assoc [ ("name", `String "kit"); ("ownership", `String "Own") ]))
  in
  Alcotest.(check bool) "copies own only" false (contains (Yojson.Safe.to_string c) ext);
  Alcotest.(check bool)
    "invalid ownership" true
    (Result.is_error
       (Ops.copies ctx (`Assoc [ ("name", `String "kit"); ("ownership", `String "x") ])))

let () =
  run "hardening-misc"
    [
      ("credentials", [ Alcotest.test_case "token URL" `Quick token_url ]);
      ( "editor",
        [ Alcotest.test_case "comments and links" `Quick editor_keeps_comments_and_links ]
      );
      ( "small",
        [
          Alcotest.test_case "init creates the data dir" `Quick init_creates_dir;
          Alcotest.test_case "export to a missing dir" `Quick export_missing_dir;
          Alcotest.test_case "offline gh is network" `Quick offline_is_network;
          Alcotest.test_case "ownership filters" `Quick ownership_filters;
        ] );
    ]
