(* Refresh-side logic. Groups (one alcotest group per spec check, appended by later
   slices):
   - classify (CHECK-10, AC-10): the ported role/evidence/tier/domain/status/fork-note/
     campaign/what helpers reproduce the golden outputs (test/fixtures/classify: synthetic
     cases over the fictional owners of fixtures/sources.test.toml, one per outcome class
     of the rules, checked under that configuration), both as pure functions and through
     import-raw into a DB; first_para and norm_remote match their golden cases.
   - decision (CHECK-8, AC-8): the FR-031 decision table, including age-only → no README.
   - commands (CHECK-9, AC-9): every built remote argv is `gh|glab api -X GET`, git argv
     is read-only, data cannot inject flags/segments, campaign plans carry no README,
     root-listing or manifest command (FR-071).
   - fetch: SHA-256 vectors, README cap/redaction, manifest and contributor parsers.
   - proc: the real subprocess runner (timeout, both pipes, classification, no stderr).
   - refresh-one: `refresh --repo` end to end with a fake gh/glab (README, sha,
     synopsis, unchanged push → no README call, campaign purge EC-15, classified
     failures that never carry stderr text, CHECK-16 extension).
   - full-refresh: every FR-032 listing through a fake forge and a temp dev root (EC-11 max
     pushed_at, search counts, participation-only and MR-only repos, local scan and
     local-only rows, gaps, globals); every issued command passes the CHECK-9 audit.
   - partial-failure (CHECK-18, AC-18): a failed per-repo fetch keeps previous data,
     records a classified gap and makes the run partial; missing_since is not set while a
     listing fails and is set once all listings succeed; due fetches are retried;
     a rate limit stops further calls to that host (AM-12).
   - redaction (CHECK-16 extension): README token → [redacted]; token-bearing stderr never
     reaches gaps; campaign repos are never named in stored gaps.
   - lock (AM-11): a second full refresh fails while the lock is held. *)

open Library_index
open Test_support.Support
module Fake_forge = Test_support.Fake_forge

let classify_dir =
  first_existing "test/fixtures/classify"
    [
      Filename.concat exe_dir "fixtures/classify";
      "test/fixtures/classify";
      "fixtures/classify";
    ]

(* LIBRARY_INDEX_GOLDEN=<golden.json> together with LIBRARY_INDEX_DATA=<data dir> runs the
   golden checks of the classify group against that golden (and the text.json beside it)
   under that data dir's sources.toml: a golden captured from real data stays with that
   data, outside this tree. Without LIBRARY_INDEX_GOLDEN the synthetic fixtures and test
   configuration are used. *)
let golden_dir, golden_id =
  match Sys.getenv_opt "LIBRARY_INDEX_GOLDEN" with
  | Some p when String.trim p <> "" -> (
      match Sys.getenv_opt "LIBRARY_INDEX_DATA" with
      | Some d when String.trim d <> "" -> (
          match Config.load_dir d with
          | Ok c -> (Filename.dirname p, Identity.of_config c)
          | Error e -> failwith e)
      | _ -> failwith "LIBRARY_INDEX_GOLDEN needs LIBRARY_INDEX_DATA")
  | _ -> (classify_dir, id)

let load name = Yojson.Safe.from_file (Filename.concat golden_dir name)
let str_opt = function `String s -> Some s | _ -> None
let int_of = function `Int n -> n | _ -> 0
let bool_of = function `Bool b -> b | _ -> false

let golden_cases () =
  let g = load "golden.json" in
  match member "cases" g with
  | `List l -> (Option.get (Status.parse_time (to_str (member "now" g))), l)
  | _ -> Alcotest.fail "golden.json: no cases"

let expected_of c k = member k (member "expected" c)

(* Pure ports against each golden case. *)
let check_case ~now c =
  let key = to_str (member "key" c) in
  let input = member "input" c in
  let s k = str_opt (member k input) in
  let f =
    Classify.facts_of_raw
      ~search_commits:(int_of (member "search_commits" c))
      ~search_prs:(int_of (member "search_prs" c))
      input
  in
  let d = Classify.derive ~id:golden_id f in
  let exp k = str_opt (expected_of c k) in
  let check label e g = Alcotest.(check (option string)) (key ^ " " ^ label) e g in
  check "role" (exp "role") d.role;
  check "evidence" (exp "evidence") (Some d.evidence);
  check "tier" (exp "tier") (Some d.tier);
  check "computed_domain" (exp "computed_domain") d.domain;
  check "status" (exp "status")
    (Some
       (Status.computed ~now
          ~archived:(bool_of (member "archived" input))
          ~last_commit:(s "last_commit") ~pushed_at:(s "pushed_at")));
  check "fork_note" (exp "fork_note")
    (Classify.fork_note ~fork:f.fork ~parent:(s "parent") ~maintained_side:None
       ~last_commit:(s "last_commit") ~pushed_at:(s "pushed_at")
       ~parent_pushed_at:(s "parent_pushed_at")
       ~parent_archived:(bool_of (member "parent_archived" input)));
  Alcotest.(check bool)
    (key ^ " is_campaign")
    (bool_of (expected_of c "is_campaign"))
    (Campaign.of_owner ~id:golden_id f.owner);
  check "what" (exp "what_fallback")
    (Some
       (Classify.what_fallback ~what:(s "what") ~synopsis:(s "synopsis")
          ~description:(s "description")))

(* The same cases through import-raw: stored derived columns equal the golden ones. *)
let check_import cases =
  let ctx = empty_ctx () in
  let counts k =
    `Assoc
      (List.filter_map
         (fun c ->
           let input = member "input" c in
           if to_str (member "host" input) = "github" then
             Some (String.lowercase_ascii (to_str (member "full_name" input)), member k c)
           else None)
         cases)
  in
  let raw =
    `Assoc
      [
        ( "repos",
          `Assoc (List.map (fun c -> (to_str (member "key" c), member "input" c)) cases)
        );
        ("local", `List []);
        ("gaps", `List []);
        ("gh_search_commits", counts "search_commits");
        ("gh_search_prs", counts "search_prs");
      ]
  in
  let summary = ok_or_fail (Import_raw.import ~id:golden_id ctx.db raw) in
  Alcotest.(check int) "rows imported" (List.length cases) summary.repos;
  List.iter
    (fun c ->
      let key = to_str (member "key" c) in
      let row =
        Db.query_one ctx.db
          "SELECT role, evidence, tier, computed_domain, fetched FROM repos WHERE key = ?"
          [ Db.text key ]
          (fun r -> Array.map Db.to_string_opt r)
      in
      match row with
      | None -> Alcotest.failf "%s not imported" key
      | Some r ->
          let exp k = str_opt (expected_of c k) in
          Alcotest.(check (option string)) (key ^ " db role") (exp "role") r.(0);
          Alcotest.(check (option string)) (key ^ " db evidence") (exp "evidence") r.(1);
          Alcotest.(check (option string)) (key ^ " db tier") (exp "tier") r.(2);
          Alcotest.(check (option string))
            (key ^ " db domain") (exp "computed_domain") r.(3);
          Alcotest.(check (option string)) (key ^ " fetched") (Some "1") r.(4))
    cases;
  fts_integrity ctx

let check_text () =
  let t = load "text.json" in
  let cases k =
    match member k t with `List l -> l | _ -> Alcotest.failf "text.json: no %s" k
  in
  List.iter
    (fun c ->
      Alcotest.(check (option string))
        "first_para"
        (str_opt (member "expected" c))
        (Classify.first_para (to_str (member "input" c))))
    (cases "first_para");
  List.iter
    (fun c ->
      let input = to_str (member "input" c) in
      Alcotest.(check (option string))
        ("norm_remote " ^ input)
        (str_opt (member "expected" c))
        (Classify.norm_remote input))
    (cases "norm_remote")

let campaign_ingest () =
  let ctx = empty_ctx () in
  let row key owner name =
    `Assoc
      [
        ("key", `String key);
        ("host", `String "gitlab");
        ("owner", `String owner);
        ("name", `String name);
        ("full_name", `String (owner ^ "/" ^ name));
        ("fork", `Bool false);
        ("archived", `Bool false);
        ("description", `String "target internals glpat-abcdefghijklmnopqrstuv");
        ("synopsis", `String "scope details");
        ("url", `String "https://u:p@gitlab.com/x");
      ]
  in
  let raw =
    `Assoc
      [
        ( "repos",
          `Assoc
            [
              ("a", row "gitlab:acme-campaigns/c1/scope" "acme-campaigns/c1" "scope");
              ("b", row "gitlab:acme-campaigns/playbook" "acme-campaigns" "playbook");
            ] );
        ("local", `List []);
        ("gaps", `List [ `String "gap" ]);
      ]
  in
  ignore (ok_or_fail (Import_raw.import ~id ctx.db raw));
  let get key =
    Option.get
      (Db.query_one ctx.db "SELECT description, synopsis, url FROM repos WHERE key = ?"
         [ Db.text key ]
         (fun r -> Array.map Db.to_string_opt r))
  in
  let a = get "gitlab:acme-campaigns/c1/scope" in
  Alcotest.(check (option string)) "withheld" (Some Classify.withheld_description) a.(0);
  Alcotest.(check (option string)) "no synopsis" None a.(1);
  Alcotest.(check (option string)) "userinfo dropped" (Some "https://gitlab.com/x") a.(2);
  let b = get "gitlab:acme-campaigns/playbook" in
  Alcotest.(check (option string))
    "playbook description exempt but redacted" (Some "target internals [redacted]") b.(0)

let classify () =
  let now, cases = golden_cases () in
  Alcotest.(check int) "golden case count" 100 (List.length cases);
  List.iter (check_case ~now) cases;
  check_import cases;
  check_text ();
  campaign_ingest ()

(* ------------------------------------------------------------ decision (CHECK-8) *)

let decision () =
  let t = Some "2026-10-01T00:00:00Z" and t' = Some "2026-10-02T00:00:00Z" in
  let case name ~stored ~listed ~readme_stored ~age ~full (er, ed) =
    let d =
      Decision.decide ~stored_pushed_at:stored ~listed_pushed_at:listed ~readme_stored
        ~detail_age_days:age ~full
    in
    Alcotest.(check (pair bool bool)) name (er, ed) (d.readme, d.detail)
  in
  let unchanged = case ~stored:t ~listed:t ~readme_stored:true ~full:false in
  unchanged "unchanged, fresh detail" ~age:(Some 5) (false, false);
  unchanged "unchanged, 29 days" ~age:(Some 29) (false, false);
  unchanged "age-only 30 days: detail, no README" ~age:(Some 30) (false, true);
  unchanged "age-only 400 days: detail, no README" ~age:(Some 400) (false, true);
  unchanged "detail never fetched" ~age:None (false, true);
  case "pushed_at changed" ~stored:t ~listed:t' ~readme_stored:true ~age:(Some 1)
    ~full:false (true, true);
  case "stored newer than listed (EC-12)" ~stored:t' ~listed:t ~readme_stored:true
    ~age:(Some 1) ~full:false (true, true);
  case "no stored row" ~stored:None ~listed:t ~readme_stored:false ~age:None ~full:false
    (true, true);
  case "no stored README" ~stored:t ~listed:t ~readme_stored:false ~age:(Some 1)
    ~full:false (true, true);
  case "--full" ~stored:t ~listed:t ~readme_stored:true ~age:(Some 1) ~full:true
    (true, true);
  case "listing without pushed_at" ~stored:t ~listed:None ~readme_stored:true
    ~age:(Some 1) ~full:false (true, true);
  Alcotest.(check (option int))
    "age 30 days" (Some 30)
    (Decision.age_days ~now:parity_now (Some "2026-09-03T19:58:30Z"));
  Alcotest.(check (option int))
    "age just under 30 days" (Some 29)
    (Decision.age_days ~now:parity_now (Some "2026-09-03T19:58:31Z"));
  Alcotest.(check (option int)) "absent" None (Decision.age_days ~now:parity_now None)

(* ------------------------------------------------------------ commands (CHECK-9) *)

let assert_get = Fake_forge.assert_get

let plan_repo ?(host = "github") ?(owner = "example-user") ?(fork = false)
    ?(archived = false) ?(sources = []) ?(mrs = 0) ?readme_url ?(gl_id = Some 42) name =
  {
    Commands.host;
    owner;
    full_name = owner ^ "/" ^ name;
    gl_id;
    default_branch = Some "main";
    readme_url;
    fork;
    archived;
    sources;
    gl_contributed = false;
    mrs;
  }

let decisions =
  List.concat_map
    (fun readme -> List.map (fun detail -> { Decision.readme; detail }) [ true; false ])
    [ true; false ]

let tiers = [ None; Some "1"; Some "2"; Some "fork"; Some "collab"; Some "inherited" ]

let content_purpose (c : Commands.t) =
  List.mem c.purpose Commands.[ Readme; Root_listing; Manifest ]

let commands () =
  let gl_readme = "https://gitlab.com/g/p/-/blob/main/README.md" in
  let repos =
    [
      plan_repo "demo";
      plan_repo ~fork:true "forked";
      plan_repo ~owner:"chainx" "chainx";
      plan_repo ~owner:"example-org" ~archived:true "old";
      plan_repo ~owner:"example-collab" "collab";
      plan_repo ~owner:"../.." "-X";
      plan_repo ~host:"gitlab" ~owner:"acme/sub" ~sources:[ "gl-user" ]
        ~readme_url:gl_readme "proj";
      plan_repo ~host:"gitlab" ~owner:"acme" ~gl_id:None ~mrs:2 ~readme_url:gl_readme
        "byname";
    ]
  in
  let campaigns =
    [
      plan_repo ~host:"gitlab" ~owner:"acme-campaigns/c1" ~readme_url:gl_readme "scope";
      plan_repo ~owner:"audit-house" "2026-x";
      plan_repo ~host:"gitlab" ~owner:"acme/private" ~readme_url:gl_readme "t";
    ]
  in
  let total = ref 0 in
  let each repo campaign f =
    List.iter
      (fun tier ->
        List.iter
          (fun d ->
            let plan = Commands.plan ~id repo ~tier ~campaign d in
            total := !total + List.length plan;
            List.iter (fun (c : Commands.t) -> assert_get c.argv) plan;
            f tier d plan)
          decisions)
      tiers
  in
  List.iter (fun r -> each r false (fun _ _ _ -> ())) repos;
  List.iter
    (fun r ->
      each r true (fun _ _ plan ->
          Alcotest.(check int)
            (r.full_name ^ ": campaign plan has no README/synopsis/manifest command")
            0
            (List.length (List.filter content_purpose plan))))
    campaigns;
  (* Not vacuous: a tier-1 own repo whose README is due gets README + root listing. *)
  let p =
    Commands.plan ~id (plan_repo "demo") ~tier:(Some "1") ~campaign:false
      { readme = true; detail = true }
  in
  Alcotest.(check bool)
    "README planned" true
    (List.exists (fun (c : Commands.t) -> c.purpose = Readme) p);
  Alcotest.(check bool)
    "root listing planned" true
    (List.exists (fun (c : Commands.t) -> c.purpose = Root_listing) p);
  Alcotest.(check bool) "some commands checked" true (!total > 50);
  List.iter
    (fun r ->
      List.iter
        (fun f -> assert_get (Commands.manifest r f).argv)
        [ "x.opam"; "package.json"; "../etc/passwd"; "-F" ])
    repos;
  List.iter assert_get
    Commands.
      [
        git_remotes "/d";
        git_last_commit "/d";
        git_count "/d";
        git_count_mine ~author:"example" "/d";
        git_ls_files "/d";
        git_ls_files_z "/d";
      ];
  (* S12 document commands (FR-082): GET only, every data segment encoded. *)
  List.iter assert_get
    Commands.
      [
        gh_doc_tree ~full_name:"o/r" ~branch:(Some "feat/x y");
        gh_doc_tree ~full_name:"../-X" ~branch:None;
        gh_doc_file ~full_name:"o/r" "docs/../-F x.md";
        gl_doc_tree ~id:(Some 42) ~full_name:"g/p";
        gl_doc_tree ~id:None ~full_name:"g/sub/p";
        gl_doc_file ~id:(Some 42) ~full_name:"g/p" ~branch:(Some "main") "docs/a b.md";
      ];
  Alcotest.(check string)
    "doc file path encoded per segment" "repos/o/r/contents/docs/%2E%2E/-F%20x.md"
    (Commands.gh_doc_file ~full_name:"o/r" "docs/../-F x.md").(4);
  Alcotest.(check string)
    "GitLab doc file encoded as one segment"
    "projects/42/repository/files/docs%2Fa%20b%2Emd/raw?ref=main"
    (Commands.gl_doc_file ~id:(Some 42) ~full_name:"g/p" ~branch:(Some "main")
       "docs/a b.md").(4);
  let hostile = Commands.gh_repo_path "../-X" in
  Alcotest.(check string) "segments encoded" "repos/%2E%2E/-X" hostile;
  Alcotest.(check string)
    "AM-13 encoding" "docs%2FREADME%2Emd"
    (Commands.gl_encode "docs/README.md");
  let file = Commands.gl_readme_file in
  Alcotest.(check (option string))
    "blob path" (Some "docs/README.md")
    (file ~default_branch:(Some "main")
       "https://gitlab.com/g/p/-/blob/main/docs/README.md");
  Alcotest.(check (option string))
    "branch with slash" (Some "README.md")
    (file ~default_branch:(Some "feat/x") "https://gitlab.com/g/p/-/blob/feat/x/README.md");
  Alcotest.(check (option string))
    "unknown branch" (Some "README.md")
    (file ~default_branch:None "https://gitlab.com/g/p/-/blob/dev/README.md");
  Alcotest.(check (option string))
    "no blob" None
    (file ~default_branch:(Some "main") "https://gitlab.com/g/p")

(* ------------------------------------------------------------ fetch helpers *)

let sha_vectors () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        ("sha256 " ^ string_of_int (String.length input))
        expected (Sha256.hex input))
    [
      ("", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
      ("abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
      ( "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
        "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1" );
      ( "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu",
        "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1" );
      ( String.make 1_000_000 'a',
        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0" );
    ]

let fetch_helpers () =
  sha_vectors ();
  let big = String.concat "" (List.init 150_000 (fun _ -> "\xc3\xa9")) in
  let p = Fetch.prepare_readme big in
  Alcotest.(check bool) "capped" true (String.length p <= Fetch.readme_cap);
  Alcotest.(check bool) "valid UTF-8 after the cut" true (String.is_valid_utf_8 p);
  Alcotest.(check bool) "marker" true (Re.execp (Re.compile (Re.str "README cut")) p);
  let token = Fake_forge.fake_token () in
  let r = Fetch.prepare_readme ("# T\n\nkey " ^ token ^ " end") in
  Alcotest.(check bool) "token redacted" false (Re.execp (Re.compile (Re.str token)) r);
  Alcotest.(check bool)
    "marker present" true
    (Re.execp (Re.compile (Re.str Redact.marker)) r);
  let syn = Fetch.synopsis_of_manifest in
  Alcotest.(check (option string))
    "opam" (Some "A \"quoted\" lib")
    (syn ~name:"x.opam" "opam-version: \"2.0\"\nsynopsis: \"A \"quoted\" lib\"\n");
  Alcotest.(check (option string))
    "package.json" (Some "JS thing")
    (syn ~name:"package.json" {|{"name":"x","description":"JS thing"}|});
  Alcotest.(check (option string))
    "Cargo.toml" (Some "Rust thing")
    (syn ~name:"Cargo.toml" "[package]\nname = \"x\"\ndescription = \"Rust thing\"\n");
  Alcotest.(check (option string))
    "pyproject" (Some "Py thing")
    (syn ~name:"pyproject.toml" "[project]\ndescription=\"Py thing\"\n");
  Alcotest.(check (option string)) "absent" None (syn ~name:"x.opam" "name: \"x\"\n");
  let contrib s f = Option.map Yojson.Safe.to_string (f s) in
  Alcotest.(check (option string))
    "gh contrib" (Some {|{"commits":20,"rank":2,"share":0.2,"n_contributors":2}|})
    (contrib
       {|[{"login":"a","contributions":80},{"login":"Example-User","contributions":20}]|}
       (Fetch.gh_contrib ~id));
  Alcotest.(check (option string))
    "gh contrib absent" (Some {|{"commits":0,"n_contributors":1}|})
    (contrib {|[{"login":"a","contributions":3}]|} (Fetch.gh_contrib ~id));
  Alcotest.(check (option string))
    "gl contrib" (Some {|{"commits":7,"rank":2,"share":0.7,"n_contributors":3}|})
    (contrib
       {|[{"name":"X","commits":3},{"name":"Ada Example","commits":5},{"name":"m","email":"m.example@x","commits":2}]|}
       (Fetch.gl_contrib ~id));
  Alcotest.(check (option string)) "not a list" None (contrib "{}" (Fetch.gh_contrib ~id))

(* ------------------------------------------------------------ proc *)

let proc () =
  let run argv = Proc.run ~timeout:5. argv in
  let check_err name expected r =
    Alcotest.(check string)
      name expected
      (match r with Ok _ -> "ok" | Error f -> Proc.describe f)
  in
  check_err "exit 0" "ok" (run [| "sh"; "-c"; "printf hello" |]);
  Alcotest.(check (option string))
    "stdout" (Some "hello")
    (Result.to_option (run [| "sh"; "-c"; "printf hello" |]));
  let fails stderr = run [| "sh"; "-c"; "echo \"$0\" >&2; exit 1"; stderr |] in
  check_err "404" "not-found" (fails "gh: Not Found (HTTP 404)");
  check_err "rate" "rate-limit" (fails "API rate limit exceeded for user (HTTP 403)");
  check_err "auth" "auth" (fails "HTTP 401: Bad credentials");
  check_err "network" "network" (fails "dial tcp: lookup api.github.com: no such host");
  check_err "other" "other" (fails "something odd");
  check_err "missing binary" "other" (run [| "/nonexistent/library-index-bin" |]);
  let t0 = Unix.gettimeofday () in
  check_err "timeout" "network" (Proc.run ~timeout:0.3 [| "sh"; "-c"; "sleep 5" |]);
  Alcotest.(check bool) "timeout is prompt" true (Unix.gettimeofday () -. t0 < 3.);
  (match
     run [| "sh"; "-c"; "head -c 2000000 /dev/zero; head -c 2000000 /dev/zero >&2" |]
   with
  | Ok s -> Alcotest.(check int) "both pipes drained" 2_000_000 (String.length s)
  | Error _ -> Alcotest.fail "large output failed");
  (* stderr carrying a token is classified, and the class is all that comes back. *)
  match fails ("HTTP 401 " ^ Fake_forge.fake_token ()) with
  | Error `Auth -> ()
  | _ -> Alcotest.fail "expected a bare auth classification"

(* ------------------------------------------------------------ refresh-one *)

let seed (ctx : Ctx.t) rows =
  ok_or_fail
    (Db.with_txn ctx.db (fun () ->
         List.fold_left
           (fun acc r ->
             Result.bind acc (fun () ->
                 Result.map ignore (Import_raw.upsert_fetched ~id ctx.db r)))
           (Ok ()) rows))

let col (ctx : Ctx.t) key c =
  Option.join
    (Db.query_one ctx.db
       (Printf.sprintf "SELECT %s FROM repos WHERE key = ?" c)
       [ Db.text key ]
       (fun r -> Db.to_string_opt r.(0)))

let contains s sub = Re.execp (Re.compile (Re.str sub)) s
let demo_fn = "example-user/demo"
let demo_key = "github:" ^ demo_fn
let t0 = "2026-09-01T00:00:00Z"
let t1 = "2026-10-01T00:00:00Z"
let t2 = "2026-10-02T00:00:00Z"

let readme_text () =
  "# Demo\n\nA demo library used to exercise the refresh pipeline end to end.\n\nkey: "
  ^ Fake_forge.fake_token () ^ "\n"

let demo_forge forge ~pushed =
  let p s = "repos/" ^ demo_fn ^ s in
  Fake_forge.json forge (p "") (Fake_forge.gh_repo ~pushed demo_fn);
  Fake_forge.ok forge (p "/readme") (readme_text ());
  Fake_forge.json forge (p "/commits?per_page=1")
    (`List
       [
         `Assoc
           [ ("commit", `Assoc [ ("committer", `Assoc [ ("date", `String pushed) ]) ]) ];
       ]);
  Fake_forge.json forge (p "/contents/")
    (`List [ `Assoc [ ("name", `String "demo.opam"); ("type", `String "file") ] ]);
  Fake_forge.ok forge (p "/contents/demo.opam") "synopsis: \"Demo synopsis\"\n"

let refresh ?(full = false) ctx env ident =
  let r = ok_or_fail (Repo_refresh.refresh_one ctx env ~full ident) in
  (r.json, r.complete)

let status_of report k = to_str (member k report)

let refresh_happy () =
  let ctx = empty_ctx () in
  seed ctx [ Option.get (Norm.gh (Fake_forge.gh_repo ~pushed:t0 demo_fn)) ];
  let forge = Fake_forge.create () in
  demo_forge forge ~pushed:t1;
  let env = Fake_forge.env forge in
  let report, complete = refresh ctx env "example-user/demo" in
  Alcotest.(check bool) "complete" true complete;
  Alcotest.(check string) "readme fetched" "fetched" (status_of report "readme");
  let readme = Option.get (col ctx demo_key "readme") in
  Alcotest.(check bool)
    "token never stored" false
    (contains readme (Fake_forge.fake_token ()));
  Alcotest.(check bool) "redacted marker stored" true (contains readme Redact.marker);
  Alcotest.(check (option string))
    "sha of stored text"
    (Some (Sha256.hex readme))
    (col ctx demo_key "readme_sha256");
  Alcotest.(check (option string))
    "synopsis from manifest" (Some "Demo synopsis") (col ctx demo_key "synopsis");
  Alcotest.(check (option string))
    "pushed_at updated" (Some t1)
    (col ctx demo_key "pushed_at");
  Alcotest.(check (option string))
    "last commit" (Some t1)
    (col ctx demo_key "last_commit");
  Alcotest.(check (option string))
    "readme_fetched_at"
    (Some (Ctx.rfc3339 parity_now))
    (col ctx demo_key "readme_fetched_at");
  Alcotest.(check (option string)) "tier recomputed" (Some "1") (col ctx demo_key "tier");
  fts_integrity ctx;
  (* Same pushed_at, fresh detail: no README and no detail call. *)
  Fake_forge.reset_log forge;
  let report, complete = refresh ctx env demo_key in
  Alcotest.(check bool) "complete again" true complete;
  Alcotest.(check string) "unchanged" "unchanged" (status_of report "readme");
  Alcotest.(check bool)
    "no README call" false
    (Fake_forge.called_matching forge "/readme");
  Alcotest.(check bool)
    "no commits call" false
    (Fake_forge.called_matching forge "/commits");
  Alcotest.(check (option string))
    "synopsis kept" (Some "Demo synopsis") (col ctx demo_key "synopsis");
  (* A new push refetches; --full refetches without one. *)
  demo_forge forge ~pushed:t2;
  Fake_forge.reset_log forge;
  ignore (refresh ctx env demo_key);
  Alcotest.(check bool)
    "README call on push" true
    (Fake_forge.called_matching forge "/readme");
  Fake_forge.reset_log forge;
  ignore (refresh ~full:true ctx env demo_key);
  Alcotest.(check bool)
    "README call on --full" true
    (Fake_forge.called_matching forge "/readme")

let refresh_campaign () =
  let ctx = empty_ctx () in
  let fn = "acme-campaigns/c1/scope" in
  let key = "gitlab:" ^ fn in
  seed ctx [ Option.get (Norm.gl (Fake_forge.gl_project ~id:7 ~pushed:t0 fn)) ];
  Db.run ctx.db
    "UPDATE repos SET readme = 'old', readme_sha256 = 'x', synopsis = 's' WHERE key = ?"
    [ Db.text key ];
  let forge = Fake_forge.create () in
  Fake_forge.json forge "projects/7"
    (Fake_forge.gl_project ~id:7 ~pushed:t1 ~readme:"README.md" ~description:"target X" fn);
  let report, _ = refresh ~full:true ctx (Fake_forge.env forge) key in
  Alcotest.(check string) "withheld" "withheld" (status_of report "readme");
  List.iter
    (fun sub ->
      Alcotest.(check bool)
        ("no " ^ sub ^ " command")
        false
        (Fake_forge.called_matching forge sub))
    [ "repository/files"; "repository/tree"; "/readme" ];
  Alcotest.(check (option string)) "README purged (EC-15)" None (col ctx key "readme");
  Alcotest.(check (option string)) "sha purged" None (col ctx key "readme_sha256");
  Alcotest.(check (option string)) "synopsis purged" None (col ctx key "synopsis");
  Alcotest.(check (option string))
    "description withheld" (Some Classify.withheld_description)
    (col ctx key "description");
  fts_integrity ctx

let refresh_failures () =
  let ctx = empty_ctx () in
  seed ctx [ Option.get (Norm.gh (Fake_forge.gh_repo ~pushed:t0 demo_fn)) ];
  let forge = Fake_forge.create () in
  demo_forge forge ~pushed:t1;
  let env = Fake_forge.env forge in
  ignore (refresh ctx env demo_key);
  let readme = col ctx demo_key "readme" in
  (* README due (new push) but rate-limited: previous README kept, classified gap. *)
  demo_forge forge ~pushed:t2;
  Fake_forge.fail forge ("repos/" ^ demo_fn ^ "/readme") `Rate_limit;
  let report, complete = refresh ctx env demo_key in
  Alcotest.(check bool) "partial" false complete;
  Alcotest.(check string) "readme failed" "failed" (status_of report "readme");
  Alcotest.(check bool)
    "classified gap" true
    (contains (Yojson.Safe.to_string report) "readme failed (rate-limit)");
  Alcotest.(check (option string))
    "previous README kept" readme (col ctx demo_key "readme");
  (* Metadata refused: nothing written. *)
  Fake_forge.fail forge ("repos/" ^ demo_fn) `Auth;
  let report, complete = refresh ctx env demo_key in
  Alcotest.(check bool) "partial on auth" false complete;
  Alcotest.(check bool)
    "auth gap" true
    (contains (Yojson.Safe.to_string report) "metadata failed (auth); nothing written");
  Alcotest.(check (option string))
    "pushed_at untouched" (Some t2)
    (col ctx demo_key "pushed_at");
  (* Real subprocess whose stderr carries a token: only the class comes out. *)
  let token = Fake_forge.fake_token () in
  let leaky =
    {
      env with
      Repo_refresh.run =
        (fun ~timeout _ ->
          Proc.run ~timeout
            [| "sh"; "-c"; "echo \"$0\" >&2; exit 1"; "HTTP 401 " ^ token |]);
    }
  in
  let report, _ = refresh ctx leaky demo_key in
  let shown = Yojson.Safe.to_string report in
  Alcotest.(check bool) "no token in report" false (contains shown token);
  Alcotest.(check bool) "auth class" true (contains shown "(auth)");
  (* Usage errors. *)
  let err ident =
    match Repo_refresh.refresh_one ctx env ~full:false ident with
    | Error e -> e
    | Ok _ -> Alcotest.failf "%s: expected an error" ident
  in
  Alcotest.(check bool) "unknown" true (contains (err "nope/nope") "unknown repo")

let refresh_local () =
  let ctx = empty_ctx () in
  let dir = temp_dir () in
  let write n s =
    Out_channel.with_open_bin (Filename.concat dir n) (fun oc -> output_string oc s)
  in
  write "README.md" "# Local\n\nA local-only checkout that the refresh reads from disk.\n";
  write "loc.opam" "synopsis: \"Local synopsis\"\n";
  let key = "local:loc" in
  seed ctx
    [
      `Assoc
        [
          ("key", `String key);
          ("host", `String "local");
          ("owner", `String "(local)");
          ("name", `String "loc");
          ("full_name", `String "loc");
          ("local_paths", `List [ `String dir ]);
          ("pushed_at", `String t0);
          ("last_commit_local", `String t0);
        ];
    ];
  let forge = Fake_forge.create () in
  Fake_forge.ok forge (Fake_forge.key_of (Commands.git_last_commit dir)) (t1 ^ "\n");
  let report, complete = refresh ctx (Fake_forge.env forge) key in
  Alcotest.(check bool) "complete" true complete;
  Alcotest.(check string) "readme from disk" "fetched" (status_of report "readme");
  Alcotest.(check bool)
    "README stored" true
    (contains (Option.get (col ctx key "readme")) "local-only checkout");
  Alcotest.(check (option string))
    "synopsis from opam" (Some "Local synopsis") (col ctx key "synopsis");
  Alcotest.(check (option string))
    "last commit from git" (Some t1) (col ctx key "last_commit");
  Alcotest.(check bool)
    "only read-only git" true
    (List.for_all (fun a -> a.(0) = "git") (Fake_forge.calls forge))

let refresh_one () =
  refresh_happy ();
  refresh_campaign ();
  refresh_failures ();
  refresh_local ()

(* ------------------------------------------------------------ full refresh *)

let gh_user_listing = Fake_forge.gh_user_listing
let gh_affiliation = Fake_forge.gh_affiliation
let commits_page1 = Fake_forge.commits_page1
let prs_page1 = Fake_forge.prs_page1
let mrs_listing = Fake_forge.mrs_listing
let quiet_forge = Fake_forge.quiet

let full_run ?(full = false) ?dev_root ?(workers = 4) ctx forge =
  let env = { (Fake_forge.env forge) with workers } in
  let dev_root = Option.value dev_root ~default:Fake_forge.no_dev_root in
  ok_or_fail (Refresh.run ctx env ~dev_root:(Some dev_root) ~full)

let gaps (ctx : Ctx.t) =
  Db.query ctx.db "SELECT text FROM gaps ORDER BY ord" [] (fun r ->
      Option.value (Db.to_string_opt r.(0)) ~default:"")

let git_answer forge dir argv out = Fake_forge.ok forge (Fake_forge.key_of (argv dir)) out

let make_dev_root forge =
  let root = temp_dir () in
  let mk name =
    let d = Filename.concat root name in
    Unix.mkdir d 0o755;
    Unix.mkdir (Filename.concat d ".git") 0o755;
    d
  in
  let alpha = mk "alpha" and solo = mk "solo" in
  Out_channel.with_open_bin (Filename.concat root "notrepo") (fun _ -> ());
  git_answer forge alpha Commands.git_remotes
    "origin\tgit@github.com:example-user/alpha.git (fetch)\n\
     origin\thttps://u:p@github.com/example-user/alpha.git (push)\n";
  git_answer forge alpha Commands.git_last_commit t1;
  git_answer forge alpha (Commands.git_count_mine ~author:"example") "5";
  git_answer forge alpha Commands.git_count "10";
  git_answer forge solo Commands.git_last_commit t1;
  git_answer forge solo (Commands.git_count_mine ~author:"example") "3";
  git_answer forge solo Commands.git_count "3";
  git_answer forge solo Commands.git_ls_files "a.ml\nb.ml\nREADME.md\n";
  (root, alpha)

let rich_forge () =
  let forge = quiet_forge () in
  let alpha = "example-user/alpha" in
  Fake_forge.json forge gh_user_listing
    (`List
       [
         Fake_forge.gh_repo ~pushed:t1 alpha;
         Fake_forge.gh_repo ~fork:true "example-user/fork-x";
       ]);
  Fake_forge.json forge gh_affiliation (`List [ Fake_forge.gh_repo ~pushed:t2 alpha ]);
  Fake_forge.json forge "orgs/example-org/repos?type=all&per_page=100"
    (`List [ Fake_forge.gh_repo "example-org/tool" ]);
  Fake_forge.json forge "user/memberships/orgs"
    (`List [ `Assoc [ ("organization", `Assoc [ ("login", `String "example-org") ]) ] ]);
  let commit = `Assoc [ ("repository", `Assoc [ ("full_name", `String "Other/Lib") ]) ] in
  Fake_forge.json forge commits_page1 (`Assoc [ ("items", `List [ commit; commit ]) ]);
  Fake_forge.json forge prs_page1
    (`Assoc
       [
         ( "items",
           `List
             [
               `Assoc
                 [ ("repository_url", `String "https://api.github.com/repos/other/lib") ];
             ] );
       ]);
  Fake_forge.json forge "repos/other/lib" (Fake_forge.gh_repo "Other/Lib");
  Fake_forge.json forge "users/5/projects?per_page=100"
    (`List [ Fake_forge.gl_project ~id:11 ~readme:"README.md" "ExampleUser/glp" ]);
  Fake_forge.json forge mrs_listing (`List [ `Assoc [ ("project_id", `Int 99) ] ]);
  Fake_forge.json forge "projects/99" (Fake_forge.gl_project ~id:99 "acme/mr-only");
  Fake_forge.ok forge
    ("repos/" ^ alpha ^ "/readme")
    "# Alpha\n\nAlpha is the first library of this fake forge, for tests.\n";
  forge

let assert_all_get forge = List.iter assert_get (Fake_forge.calls forge)

let full_refresh () =
  let ctx = empty_ctx () in
  let forge = rich_forge () in
  let root, alpha_dir = make_dev_root forge in
  let s = full_run ~dev_root:root ctx forge in
  Alcotest.(check (list string)) "no failure" [] s.failures;
  Alcotest.(check bool) "complete" true s.complete;
  Alcotest.(check int) "listed" 7 s.listed;
  assert_all_get forge;
  let a = "github:example-user/alpha" in
  Alcotest.(check (option string)) "EC-11 max pushed_at" (Some t2) (col ctx a "pushed_at");
  Alcotest.(check (option string))
    "sources merged" (Some {|["gh-affiliation","gh-user"]|}) (col ctx a "sources");
  Alcotest.(check bool) "README stored" true (col ctx a "readme" <> None);
  Alcotest.(check (option string))
    "local path"
    (Some (Printf.sprintf {|["%s"]|} alpha_dir))
    (col ctx a "local_paths");
  Alcotest.(check (option string))
    "local commits" (Some "5")
    (col ctx a "local_my_commits");
  let lib = "github:other/lib" in
  Alcotest.(check (option string))
    "participation-only" (Some {|["gh-participation"]|}) (col ctx lib "sources");
  Alcotest.(check (option string))
    "evidence from search" (Some "2 commits, 1 PRs/MRs") (col ctx lib "evidence");
  Alcotest.(check (option string)) "tier" (Some "1") (col ctx lib "tier");
  let mr = "gitlab:acme/mr-only" in
  Alcotest.(check (option string)) "MR-only project" (Some "1") (col ctx mr "mrs");
  Alcotest.(check (option string))
    "local-only row" (Some {|["local-only"]|})
    (col ctx "local:solo" "sources");
  Alcotest.(check (option string))
    "local language" (Some "OCaml")
    (col ctx "local:solo" "language");
  Alcotest.(check (option string))
    "search global" (Some {|{"other/lib":2}|})
    (Db.get_meta ctx.db "gh_search_commits");
  Alcotest.(check (option string))
    "member orgs" (Some {|["example-org"]|})
    (Db.get_meta ctx.db "gh_member_orgs");
  let g = gaps ctx in
  Alcotest.(check bool)
    "probe 404 gap" true
    (List.mem "GitHub org `probe-org` does not exist (404)." g);
  Alcotest.(check bool)
    "search cap gap" true
    (List.exists (fun l -> contains l "caps at 1000 results") g);
  let checkouts =
    Db.query ctx.db "SELECT remote_url FROM local_checkouts ORDER BY ord" [] (fun r ->
        Db.to_string_opt r.(0))
  in
  Alcotest.(check int) "local checkouts" 2 (List.length checkouts);
  Alcotest.(check bool)
    "remote userinfo dropped" false
    (List.exists (function Some u -> contains u "u:p@" | None -> false) checkouts);
  fts_integrity ctx;
  (* Second run, nothing pushed: no README call at all. *)
  Fake_forge.reset_log forge;
  let s = full_run ~dev_root:root ctx forge in
  Alcotest.(check bool) "complete again" true s.complete;
  Alcotest.(check bool)
    "no README call" false
    (Fake_forge.called_matching forge "/readme");
  Alcotest.(check int) "READMEs skipped" s.listed s.readme_skipped

(* ------------------------------------------------------------ partial-failure (CHECK-18) *)

let partial_failure () =
  let ctx = empty_ctx () in
  let forge = quiet_forge () in
  let a = "example-user/a" and akey = "github:example-user/a" in
  Fake_forge.json forge gh_user_listing (`List [ Fake_forge.gh_repo ~pushed:t1 a ]);
  Fake_forge.ok forge
    ("repos/" ^ a ^ "/readme")
    "# A\n\nThe first README text of repository A, long enough.\n";
  ignore (full_run ctx forge);
  let readme1 = col ctx akey "readme" in
  let synopsis1 = col ctx akey "synopsis" and sha1 = col ctx akey "readme_sha256" in
  Alcotest.(check bool) "first README stored" true (readme1 <> None);
  Alcotest.(check bool) "first synopsis stored" true (synopsis1 <> None);
  seed ctx [ Option.get (Norm.gh (Fake_forge.gh_repo ~pushed:t0 "example-user/gone")) ];
  (* A listing fails and A's README fetch fails on a new push. *)
  Fake_forge.fail forge gh_affiliation `Network;
  Fake_forge.json forge gh_user_listing (`List [ Fake_forge.gh_repo ~pushed:t2 a ]);
  Fake_forge.fail forge ("repos/" ^ a ^ "/readme") `Auth;
  let s = full_run ctx forge in
  Alcotest.(check bool) "partial → exit 2" false s.complete;
  Alcotest.(check (option string)) "previous README kept" readme1 (col ctx akey "readme");
  (* AM-18: the failed README fetch keeps the stored synopsis and digest too. *)
  Alcotest.(check (option string))
    "previous synopsis kept" synopsis1 (col ctx akey "synopsis");
  Alcotest.(check (option string))
    "previous readme_sha256 kept" sha1
    (col ctx akey "readme_sha256");
  Alcotest.(check (option string))
    "retry scheduled" None
    (col ctx akey "readme_fetched_at");
  let g = gaps ctx in
  Alcotest.(check bool)
    "classified per-repo gap" true
    (List.mem (akey ^ ": readme failed (auth); previous data kept") g);
  Alcotest.(check bool)
    "classified listing gap" true
    (List.exists (fun l -> contains l "user/repos` failed (network)") g);
  Alcotest.(check (option string))
    "no missing_since while a listing fails" None
    (col ctx "github:example-user/gone" "missing_since");
  (* Everything answers again: the README is retried and the unseen row is marked. *)
  Fake_forge.json forge gh_affiliation (`List []);
  Fake_forge.ok forge
    ("repos/" ^ a ^ "/readme")
    "# A\n\nThe second README text of repository A, after the push.\n";
  let s = full_run ctx forge in
  Alcotest.(check bool) "complete → exit 0" true s.complete;
  Alcotest.(check bool)
    "README refetched" true
    (contains (Option.value (col ctx akey "readme") ~default:"") "second README");
  Alcotest.(check (option string))
    "missing_since set"
    (Some (Ctx.rfc3339 parity_now))
    (col ctx "github:example-user/gone" "missing_since");
  Alcotest.(check int)
    "never deleted" 1
    (List.length
       (Db.query ctx.db "SELECT 1 FROM repos WHERE key = 'github:example-user/gone'" []
          Fun.id));
  (* README fetched but synopsis discovery failed: README stored, retry still scheduled. *)
  let t3 = "2026-10-03T00:00:00Z" in
  Fake_forge.json forge gh_user_listing (`List [ Fake_forge.gh_repo ~pushed:t3 a ]);
  Fake_forge.ok forge
    ("repos/" ^ a ^ "/readme")
    "# A\n\nThe third README text of repository A, after another push.\n";
  Fake_forge.fail forge ("repos/" ^ a ^ "/contents/") `Network;
  let s = full_run ctx forge in
  Alcotest.(check bool) "root listing failure → partial" false s.complete;
  Alcotest.(check bool)
    "new README stored" true
    (contains (Option.value (col ctx akey "readme") ~default:"") "third README");
  Alcotest.(check (option string))
    "retry scheduled after root-listing failure" None
    (col ctx akey "readme_fetched_at");
  Fake_forge.json forge ("repos/" ^ a ^ "/contents/") (`List []);
  (* AM-12: a rate limit stops every later gh call of the run. *)
  let b = "example-user/b" in
  Fake_forge.json forge gh_user_listing
    (`List [ Fake_forge.gh_repo ~pushed:t2 a; Fake_forge.gh_repo ~pushed:t2 b ]);
  Fake_forge.fail forge ("repos/" ^ a ^ "/commits?per_page=1") `Rate_limit;
  Fake_forge.reset_log forge;
  let s = full_run ~full:true ~workers:1 ctx forge in
  Alcotest.(check bool) "rate limit → partial" false s.complete;
  Alcotest.(check bool)
    "later gh call skipped" false
    (Fake_forge.called forge ("repos/" ^ b ^ "/readme"));
  Alcotest.(check bool)
    "host gap" true
    (List.exists (fun l -> contains l "GitHub rate limit reached") (gaps ctx))

(* ------------------------------------------------------------ redaction / lock *)

let redaction () =
  let ctx = empty_ctx () in
  let forge = quiet_forge () in
  let token = Fake_forge.fake_token () in
  let a = "example-user/a" in
  Fake_forge.json forge gh_user_listing
    (`List [ Fake_forge.gh_repo a; Fake_forge.gh_repo "audit-house/2026-secret-target" ]);
  Fake_forge.ok forge
    ("repos/" ^ a ^ "/readme")
    ("# A\n\nSome text with " ^ token ^ " inside it.\n");
  Fake_forge.fail forge "repos/audit-house/2026-secret-target/contributors?per_page=100"
    `Other;
  let inner = Fake_forge.runner forge in
  let run ~timeout argv =
    if Fake_forge.key_of argv = "users/example-collab/repos?type=owner&per_page=100" then
      Proc.run ~timeout [| "sh"; "-c"; "echo \"$0\" >&2; exit 1"; "HTTP 401 " ^ token |]
    else inner ~timeout argv
  in
  let env = { (Fake_forge.env forge) with run } in
  let s =
    ok_or_fail (Refresh.run ctx env ~dev_root:(Some Fake_forge.no_dev_root) ~full:false)
  in
  Alcotest.(check bool) "partial" false s.complete;
  let readme = Option.value (col ctx ("github:" ^ a) "readme") ~default:"" in
  Alcotest.(check bool) "README token redacted" false (contains readme token);
  Alcotest.(check bool) "marker" true (contains readme Redact.marker);
  let all_gaps = String.concat "\n" (gaps ctx @ s.failures) in
  Alcotest.(check bool) "no token in gaps" false (contains all_gaps token);
  Alcotest.(check bool) "classified" true (contains all_gaps "failed (auth)");
  Alcotest.(check bool) "campaign never named" false (contains all_gaps "secret-target");
  Alcotest.(check bool)
    "campaign gap present" true
    (contains all_gaps "a campaign repository: contributors failed (other)")

let lock () =
  let ctx = empty_ctx () in
  let path = Filename.concat ctx.export_dir Refresh.lock_file in
  let ready = Filename.concat ctx.export_dir "child-ready" in
  match Unix.fork () with
  | 0 ->
      let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
      Unix.lockf fd Unix.F_LOCK 0;
      Out_channel.with_open_bin ready (fun _ -> ());
      Unix.sleepf 1.5;
      Unix._exit 0
  | pid ->
      let rec wait n =
        if (not (Sys.file_exists ready)) && n > 0 then (
          Unix.sleepf 0.05;
          wait (n - 1))
      in
      wait 100;
      let r =
        Refresh.run ctx
          (Fake_forge.env (quiet_forge ()))
          ~dev_root:(Some Fake_forge.no_dev_root) ~full:false
      in
      ignore (Unix.waitpid [] pid);
      (match r with
      | Error e -> Alcotest.(check string) "busy" Refresh.busy_message e
      | Ok _ -> Alcotest.fail "second refresh ran");
      (* Released: a run goes through. *)
      let s =
        ok_or_fail
          (Refresh.run ctx
             (Fake_forge.env (quiet_forge ()))
             ~dev_root:(Some Fake_forge.no_dev_root) ~full:false)
      in
      Alcotest.(check bool) "runs after release" true s.complete

let groups =
  [
    ("classify", [ Alcotest.test_case "golden" `Quick classify ]);
    ("decision", [ Alcotest.test_case "table" `Quick decision ]);
    ("commands", [ Alcotest.test_case "get-only" `Quick commands ]);
    ("fetch", [ Alcotest.test_case "helpers" `Quick fetch_helpers ]);
    ("proc", [ Alcotest.test_case "runner" `Quick proc ]);
    ("refresh-one", [ Alcotest.test_case "single repo" `Quick refresh_one ]);
    ("full-refresh", [ Alcotest.test_case "listings" `Quick full_refresh ]);
    ( "partial-failure",
      [ Alcotest.test_case "keep data, gap, exit 2" `Quick partial_failure ] );
    ("redaction", [ Alcotest.test_case "no secret stored or shown" `Quick redaction ]);
    ("lock", [ Alcotest.test_case "exclusive" `Quick lock ]);
  ]

let () = run "refresh" groups
