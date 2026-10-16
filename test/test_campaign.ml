(* AM-17: the stored campaign flag (review round 1, HIGH lib/sources.ml:326).

   A row is campaign when its owner, or a remote of one of its local checkouts, is a
   campaign owner. Groups:
   - predicate: key_owner / campaign_flag / row_is_campaign.
   - refresh: a local checkout whose campaign remote the API cannot read becomes a
     campaign [local:] row: its on-disk README is never read, no synopsis, withheld
     description; a GitHub row whose checkout also has a campaign remote is flagged too.
   - import-raw: the cache predates the flag; it is derived from [local] remotes.
   - render: flagged rows stay out of sections 1-2 and the unreachable-remote gap line
     never names them; README text never reaches either view.
   Temp dirs and a fake forge only. *)

open Library_index
open Test_support.Support
module Fake_forge = Test_support.Fake_forge

let contains s sub = Re.execp (Re.compile (Re.str sub)) s
let sentinel = "ZQX-CAMPAIGN-SENTINEL-41b7"

let col (ctx : Ctx.t) key c =
  Option.join
    (Db.query_one ctx.db
       (Printf.sprintf "SELECT %s FROM repos WHERE key = ?" c)
       [ Db.text key ]
       (fun r -> Db.to_string_opt r.(0)))

let predicate () =
  Alcotest.(check (option string))
    "key owner" (Some "acme-campaigns")
    (Campaign.key_owner "gitlab:acme-campaigns/x");
  Alcotest.(check (option string))
    "subgroup owner" (Some "acme/private")
    (Campaign.key_owner "gitlab:acme/private/bounty");
  Alcotest.(check (option string)) "no owner" None (Campaign.key_owner "local:x");
  (* No checkout is indexed and no row has local paths: the decision runs no command. *)
  let decide row =
    Campaign.decide ~id ~call:(fun _ -> assert false) ~local:Local_keys.empty row
  in
  let flag owner remote_keys =
    decide
      (`Assoc
         [
           ("owner", `String owner);
           ("checkout_remote_keys", `List (List.map (fun k -> `String k) remote_keys));
         ])
  in
  Alcotest.(check bool) "owner" true (flag "acme-campaigns/sub" []);
  Alcotest.(check bool) "remote" true (flag "(local)" [ "gitlab:acme-campaigns/x" ]);
  Alcotest.(check bool)
    "private remote" true
    (flag "example-user" [ "github:a/b"; "gitlab:acme/private/c" ]);
  Alcotest.(check bool)
    "audit-house remote" true
    (flag "(local)" [ "github:audit-house/2026-x" ]);
  Alcotest.(check bool) "plain" false (flag "(local)" [ "github:example-user/a" ]);
  Alcotest.(check bool)
    "row flag" true
    (decide (`Assoc [ ("owner", `String "(local)"); ("campaign", `Bool true) ]));
  Alcotest.(check bool)
    "row owner" true
    (decide (`Assoc [ ("owner", `String "audit-house") ]))

(* A dev root with two checkouts: [secret] (origin on an unreadable campaign remote,
   README on disk with the sentinel) and [pub] (a public GitHub repo whose checkout also
   carries a campaign remote). *)
let dev_root forge =
  let root = temp_dir () in
  let mk name remotes =
    let d = Filename.concat root name in
    Unix.mkdir d 0o755;
    Unix.mkdir (Filename.concat d ".git") 0o755;
    Out_channel.with_open_bin (Filename.concat d "README.md") (fun oc ->
        output_string oc
          ("# " ^ name ^ "\n\nThis checkout carries " ^ sentinel
         ^ " exploit notes for the target protocol.\n"));
    let answer argv out = Fake_forge.ok forge (Fake_forge.key_of (argv d)) out in
    answer Commands.git_remotes remotes;
    answer Commands.git_last_commit "2026-10-01T00:00:00Z";
    answer (Commands.git_count_mine ~author:"example") "4";
    answer Commands.git_count "4";
    d
  in
  ignore (mk "secret" "origin\tgit@gitlab.com:acme-campaigns/secret.git (fetch)\n");
  ignore
    (mk "pub"
       "origin\tgit@github.com:example-user/pub.git (fetch)\n\
        audit\tgit@gitlab.com:acme/private/pub.git (fetch)\n");
  root

let refresh () =
  let ctx = empty_ctx () in
  let forge = Fake_forge.quiet () in
  Fake_forge.json forge Fake_forge.gh_user_listing
    (`List [ Fake_forge.gh_repo ~description:"Public repo" "example-user/pub" ]);
  Fake_forge.ok forge "repos/example-user/pub/readme"
    ("# pub\n\nREADME of pub served by the API, " ^ sentinel ^ " inside.\n");
  (* The campaign remote is unreadable: projects/... answers 404 (the default). *)
  let root = dev_root forge in
  let env = Fake_forge.env forge in
  let s = ok_or_fail (Refresh.run ctx env ~dev_root:(Some root) ~full:false) in
  Alcotest.(check int) "listed: pub + local:secret" 2 s.listed;
  let k = "local:secret" in
  Alcotest.(check (option string)) "local row flagged" (Some "1") (col ctx k "campaign");
  Alcotest.(check (option string)) "README not read" None (col ctx k "readme");
  Alcotest.(check (option string)) "no synopsis" None (col ctx k "synopsis");
  Alcotest.(check (option string))
    "description withheld" (Some Classify.withheld_description) (col ctx k "description");
  let p = "github:example-user/pub" in
  Alcotest.(check (option string))
    "checkout remote flags" (Some "1") (col ctx p "campaign");
  Alcotest.(check (option string)) "no README stored" None (col ctx p "readme");
  Alcotest.(check bool)
    "no README call for a flagged row" false
    (Fake_forge.called forge "repos/example-user/pub/readme");
  let get = Yojson.Safe.to_string (ok_or_fail (Repo_view.get ctx k)) in
  Alcotest.(check bool) "get carries no sentinel" false (contains get sentinel);
  Alcotest.(check bool) "get shows the flag" true (contains get {|"campaign":true|});
  (* refresh --repo on the stored row keeps the flag and still reads nothing. *)
  let r = ok_or_fail (Repo_refresh.refresh_one ctx env ~full:true k) in
  Alcotest.(check bool)
    "single refresh report" false
    (contains (Yojson.Safe.to_string r.json) sentinel);
  Alcotest.(check (option string)) "still no README" None (col ctx k "readme");
  ctx

(* A stored README is purged once a later ingest flags the row (EC-15). *)
let purge () =
  let ctx = empty_ctx () in
  let row campaign =
    `Assoc
      [
        ("key", `String "local:x");
        ("host", `String "local");
        ("owner", `String "(local)");
        ("name", `String "x");
        ("full_name", `String "x");
        ("description", `String "d");
        ("synopsis", `String "s");
        ("campaign", `Bool campaign);
      ]
  in
  let upsert r =
    ok_or_fail (Db.with_txn ctx.db (fun () -> Import_raw.upsert_fetched ~id ctx.db r))
  in
  ignore (upsert (row false));
  Db.run ctx.db "UPDATE repos SET readme = 'R', readme_sha256 = 'h' WHERE key = 'local:x'"
    [];
  ignore (upsert (row false));
  Alcotest.(check (option string))
    "kept when not campaign" (Some "R") (col ctx "local:x" "readme");
  ignore (upsert (row true));
  Alcotest.(check (option string)) "README purged" None (col ctx "local:x" "readme");
  Alcotest.(check (option string)) "sha purged" None (col ctx "local:x" "readme_sha256");
  Alcotest.(check (option string)) "synopsis purged" None (col ctx "local:x" "synopsis")

let import_raw () =
  let ctx = empty_ctx () in
  let raw =
    `Assoc
      [
        ( "repos",
          `Assoc
            [
              ( "local:camp",
                `Assoc
                  [
                    ("key", `String "local:camp");
                    ("host", `String "local");
                    ("owner", `String "(local)");
                    ("name", `String "camp");
                    ("full_name", `String "camp");
                    ("description", `String "");
                    ("synopsis", `String ("synopsis with " ^ sentinel));
                    ("local_paths", `List [ `String "/d/camp" ]);
                  ] );
            ] );
        ( "local",
          `List
            [
              `Assoc
                [
                  ("path", `String "/d/camp");
                  ("remote_key", `String "gitlab:acme-campaigns/camp");
                  ("all_remote_keys", `List [ `String "gitlab:acme-campaigns/camp" ]);
                ];
            ] );
        ("gaps", `List []);
      ]
  in
  ignore (ok_or_fail (Import_raw.import ~id ctx.db raw));
  Alcotest.(check (option string))
    "flag from local remotes" (Some "1")
    (col ctx "local:camp" "campaign");
  Alcotest.(check (option string))
    "synopsis withheld" None
    (col ctx "local:camp" "synopsis");
  Alcotest.(check (option string))
    "description withheld" (Some Classify.withheld_description)
    (col ctx "local:camp" "description")

let render () =
  let ctx = refresh () in
  let notes = temp_dir () in
  let out = Filename.concat (temp_dir ()) "INDEX.md" in
  let o =
    ok_or_fail (render ctx ~out ~notes_dir:notes ~now:parity_now ~home:"/nonexistent")
  in
  let md = read_file out in
  let html = read_file (Option.get o.notes_html) in
  List.iter
    (fun (name, text) ->
      Alcotest.(check bool) (name ^ ": no sentinel") false (contains text sentinel))
    [ ("INDEX.md", md); ("notes", html) ];
  let data =
    Render_data.load ctx.db
      ~settings:(render_settings ctx ~out:"INDEX.md" ~notes_dir:None ~home:"")
      ~now:parity_now
  in
  let unreachable = List.filter (fun g -> contains g "could not read") data.gaps in
  Alcotest.(check int) "unreachable line" 1 (List.length unreachable);
  Alcotest.(check bool)
    "campaign row not named in gaps" false
    (contains (List.hd unreachable) "secret");
  Alcotest.(check bool)
    "masked name" true
    (contains (List.hd unreachable) "a campaign repository");
  Alcotest.(check bool)
    "no section-1/2 row for the flagged repos" false
    (contains md "| [secret](file://" || contains md "| [example-user/pub](");
  Alcotest.(check bool) "named in the workspaces line" true (contains md "secret")

let () =
  run "campaign"
    [
      ("predicate", [ Alcotest.test_case "campaign flag predicate" `Quick predicate ]);
      ( "refresh",
        [
          Alcotest.test_case "unreadable campaign remote" `Quick (fun () ->
              ignore (refresh ()));
        ] );
      ("purge", [ Alcotest.test_case "EC-15 purge on the flag" `Quick purge ]);
      ("import-raw", [ Alcotest.test_case "flag from the cache" `Quick import_raw ]);
      ("render", [ Alcotest.test_case "views and gaps" `Quick render ]);
    ]
