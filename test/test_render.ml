(* Rendered views (spec US-6, FR-050..052, AM-7, EC-21..23).
   - confidentiality (CHECK-13, AC-13): campaign rows are absent from the section-1 table
     and section 2 and named only in the workspaces line; no README text reaches either
     view; stubs and missing rows are not rendered; the AM-7 deletion rule.
   - sections: ordering and formatting of the sections on small fixtures.
   - helpers: escaping and code-point cuts. *)

open Library_index
open Test_support.Support

let contains s sub = Re.execp (Re.compile (Re.str sub)) s
let count_occurrences s sub = List.length (Re.all (Re.compile (Re.str sub)) s)
let now = parity_now
let home = "/home/tester"

let row ?(fork = false) ?(archived = false) ?(private_ = false) ?(extra = []) ~host ~owner
    name =
  let full_name = owner ^ "/" ^ name in
  `Assoc
    ( [
        ("key", `String (String.lowercase_ascii (host ^ ":" ^ full_name)));
        ("host", `String host);
        ("owner", `String owner);
        ("name", `String name);
        ("full_name", `String full_name);
        ("url", `String ("https://" ^ host ^ ".com/" ^ full_name));
        ("private", `Bool private_);
        ("fork", `Bool fork);
        ("archived", `Bool archived);
        ("description", `String ("desc of " ^ name));
        ("pushed_at", `String "2026-09-01T00:00:00Z");
      ]
    |> List.filter (fun (k, _) -> not (List.mem_assoc k extra))
    |> fun base -> base @ extra )

let import_rows ?(local = []) ?(gaps = []) ctx rows =
  let raw =
    `Assoc
      [
        ("repos", `Assoc (List.map (fun r -> (to_str (member "key" r), r)) rows));
        ("local", `List local);
        ("gaps", `List (List.map (fun g -> `String g) gaps));
        ("gh_member_orgs", `List [ `String "example-org" ]);
      ]
  in
  ignore (ok_or_fail (Import_raw.import ~id ctx.Ctx.db raw))

let render ?notes_dir ctx =
  let dir = temp_dir () in
  let notes_dir =
    match notes_dir with
    | Some d -> d
    | None ->
        let d = Filename.concat dir "notes" in
        Unix.mkdir d 0o755;
        d
  in
  let out = Filename.concat dir "INDEX.md" in
  let o = ok_or_fail (render ctx ~out ~notes_dir ~now ~home) in
  let html = Option.map read_file o.notes_html in
  (o, read_file out, html)

(* Lines of [md] from the line equal to [start] up to (excluding) the first line that
   starts with [stop]. *)
let between md ~start ~stop =
  let lines = String.split_on_char '\n' md in
  let rec skip = function [] -> [] | l :: tl -> if l = start then tl else skip tl in
  let rec take = function
    | [] -> []
    | l :: tl ->
        if
          String.length l >= String.length stop
          && String.sub l 0 (String.length stop) = stop
        then []
        else l :: take tl
  in
  String.concat "\n" (take (skip lines))

let find_from s sub start =
  let n = String.length s and k = String.length sub in
  let rec go i =
    if i + k > n then None else if String.sub s i k = sub then Some i else go (i + 1)
  in
  go start

let html_table html id =
  let opening = "<table id=\"" ^ id ^ "\">" in
  match find_from html opening 0 with
  | None -> Alcotest.failf "no table %s" id
  | Some i -> (
      match find_from html "</table>" i with
      | None -> Alcotest.failf "unterminated table %s" id
      | Some j -> String.sub html i (j - i))

let html_table_rows html id = count_occurrences (html_table html id) "<tr>" - 1

(* ------------------------------------------------------------ confidentiality *)

let secret_readme = "README-SECRET-TEXT-42"

let confidentiality_fixture () =
  let ctx = empty_ctx () in
  let mine = row ~host:"github" ~owner:"example-user" "lib-a" in
  let camp =
    row ~host:"gitlab" ~owner:"acme-campaigns/c1" "scope"
      ~extra:[ ("mrs", `Int 5); ("description", `String "TARGET-INTERNALS") ]
  in
  let audit =
    row ~host:"github" ~owner:"audit-house" "2026-x-judging"
      ~extra:
        [
          ( "contrib",
            `Assoc
              [
                ("commits", `Int 12);
                ("rank", `Int 1);
                ("share", `Float 0.5);
                ("n_contributors", `Int 2);
              ] );
        ]
  in
  import_rows ctx [ mine; camp; audit ];
  Db.run ctx.db "UPDATE repos SET readme = ?" [ Db.text secret_readme ];
  (* A curated-only stub and a missing row must never render. *)
  Db.run ctx.db
    "INSERT INTO repos(key, host, owner, name, full_name, tier, role, fetched) VALUES \
     ('github:example-user/stub-x', 'github', 'example-user', 'stub-x', \
     'example-user/stub-x', '1', 'owner', 0)"
    [];
  import_rows ctx [ mine; camp; audit; row ~host:"github" ~owner:"example-user" "gone-x" ];
  Db.run ctx.db "UPDATE repos SET missing_since = '2026-09-30' WHERE name = 'gone-x'" [];
  ctx

let check_md md =
  let table =
    between md ~start:"## 1. Repos I participate in" ~stop:"**Security-research"
  in
  let sec2 =
    between md ~start:"## 2. Tier 1 — authored / maintained, by domain" ~stop:"## 3."
  in
  Alcotest.(check bool) "own repo in section 1" true (contains table "example-user/lib-a");
  List.iter
    (fun name ->
      Alcotest.(check bool) (name ^ " not in section-1 table") false (contains table name);
      Alcotest.(check bool) (name ^ " not in section 2") false (contains sec2 name);
      Alcotest.(check int)
        (name ^ " named once (workspaces line)")
        1 (count_occurrences md name))
    [ "acme-campaigns/c1/scope"; "audit-house/2026-x-judging" ];
  Alcotest.(check bool)
    "workspaces line" true
    (contains md
       "**Security-research workspaces (2, private audit mirrors — not reusable \
        libraries):** acme-campaigns/c1/scope, audit-house/2026-x-judging");
  List.iter
    (fun s -> Alcotest.(check bool) (s ^ " absent from INDEX.md") false (contains md s))
    [ secret_readme; "TARGET-INTERNALS"; "stub-x"; "gone-x" ]

let check_html html =
  let part = html_table html "t-part" and t1 = html_table html "t-tier1" in
  Alcotest.(check bool) "own repo in t-part" true (contains part "example-user/lib-a");
  List.iter
    (fun name ->
      Alcotest.(check bool) (name ^ " not in t-part") false (contains part name);
      Alcotest.(check bool) (name ^ " not in t-tier1") false (contains t1 name);
      Alcotest.(check int) (name ^ " named once") 1 (count_occurrences html name))
    [ "acme-campaigns/c1/scope"; "audit-house/2026-x-judging" ];
  Alcotest.(check bool) "title" true (contains html "<title>Library index</title>");
  List.iter
    (fun s -> Alcotest.(check bool) (s ^ " absent from HTML") false (contains html s))
    [ secret_readme; "TARGET-INTERNALS"; "stub-x"; "gone-x" ]

let write path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let check_deletion ctx =
  let notes = temp_dir () in
  let page = "<html><title>Library index</title></html>" in
  write (Filename.concat notes "2026-09-01-library-index.html") page;
  write (Filename.concat notes "2026-09-02-library-index.html") "my own notes, no title";
  write (Filename.concat notes "notes-library-index.html") page;
  write (Filename.concat notes "2026-9-03-library-index.html") page;
  write (Filename.concat notes "2026-10-03-library-index.html") "stale same-day page";
  let o, _, html = render ~notes_dir:notes ctx in
  Alcotest.(check (list string)) "deleted" [ "2026-09-01-library-index.html" ] o.deleted;
  Alcotest.(check (list string))
    "remaining"
    [
      "2026-09-02-library-index.html";
      "2026-10-03-library-index.html";
      "2026-9-03-library-index.html";
      "notes-library-index.html";
    ]
    (List.sort compare (Array.to_list (Sys.readdir notes)));
  Alcotest.(check bool)
    "today's page rewritten" true
    (contains (Option.get html) "<title>Library index</title>");
  Alcotest.(check bool)
    "predicate: name without title kept" false
    (Render.is_stale_page ~name:"2026-09-02-library-index.html" ~content:"x")

(* A stale page that cannot be removed is a warning (exit 2), not a fatal I/O error. *)
let check_undeletable () =
  let notes = temp_dir () in
  let page = "<html><title>Library index</title></html>" in
  write (Filename.concat notes "2026-09-01-library-index.html") page;
  write (Filename.concat notes "2026-09-02-library-index.html") page;
  let remove p =
    if Filename.basename p = "2026-09-01-library-index.html" then
      raise (Sys_error (p ^ ": Permission denied"))
    else Sys.remove p
  in
  let deleted, warnings =
    Render.delete_stale ~remove ~notes_dir:notes ~keep:"2026-10-03-library-index.html" ()
  in
  Alcotest.(check (list string))
    "other page deleted"
    [ "2026-09-02-library-index.html" ]
    deleted;
  Alcotest.(check (list string))
    "warning names the page"
    [ "could not delete the stale notes page 2026-09-01-library-index.html" ]
    warnings;
  Alcotest.(check bool)
    "undeletable page left" true
    (Sys.file_exists (Filename.concat notes "2026-09-01-library-index.html"))

let check_missing_notes ctx =
  let dir = temp_dir () in
  let out = Filename.concat dir "INDEX.md" in
  let o =
    ok_or_fail
      (Test_support.Support.render ctx ~out
         ~notes_dir:(Filename.concat dir "absent")
         ~now ~home)
  in
  Alcotest.(check bool) "warning" true (o.warnings <> []);
  Alcotest.(check (option string)) "no page" None o.notes_html;
  Alcotest.(check bool) "INDEX.md written" true (Sys.file_exists out)

let confidentiality () =
  let ctx = confidentiality_fixture () in
  let _, md, html = render ctx in
  check_md md;
  check_html (Option.get html);
  check_deletion ctx;
  check_undeletable ();
  check_missing_notes ctx

(* ------------------------------------------------------------ sections *)

let contrib commits rank share n =
  ( "contrib",
    `Assoc
      [
        ("commits", `Int commits);
        ("rank", `Int rank);
        ("share", `Float share);
        ("n_contributors", `Int n);
      ] )

let sections_fixture () =
  let ctx = empty_ctx () in
  let gh = row ~host:"github" in
  let date d = ("last_commit", `String d) in
  import_rows ctx
    ~local:
      [
        `Assoc
          [
            ("path", `String (home ^ "/dev/only-local"));
            ("worktree", `Bool false);
            ("remote_url", `Null);
            ("remote_key", `Null);
            ("all_remote_keys", `List []);
            ("last_commit", `String "2026-01-02T00:00:00+01:00");
            ("my_commits", `Int 3);
            ("total_commits", `Int 4);
          ];
      ]
    [
      gh ~owner:"example-user" "old-owned" ~extra:[ date "2020-01-01T00:00:00Z" ];
      gh ~owner:"example-user" "new-owned"
        ~extra:
          [
            date "2026-09-30T00:00:00Z";
            ("local_paths", `List [ `String (home ^ "/dev/new-owned") ]);
          ];
      gh ~owner:"someone" "contributed"
        ~extra:[ contrib 20 2 0.1 5; date "2026-09-29T00:00:00Z" ];
      gh ~owner:"Zeta-Org" "z-tier2";
      gh ~owner:"alpha-org" "a-tier2" ~archived:true;
      gh ~owner:"alpha-org" "b-tier2"
        ~extra:[ ("description", `String "pipe | and\nnewline") ];
    ];
  Db.run ctx.db "UPDATE repos SET domain = 'Legacy domain' WHERE name = 'old-owned'" [];
  Db.run ctx.db
    "UPDATE repos SET domain = 'OCaml libraries', what = 'A lib' WHERE name = 'new-owned'"
    [];
  ctx

let index_of md line =
  let lines = String.split_on_char '\n' md in
  let rec go i = function
    | [] -> Alcotest.failf "line not found: %s" line
    | l :: tl -> if contains l line then i else go (i + 1) tl
  in
  go 0 lines

let sections () =
  let ctx = sections_fixture () in
  let _, md, html = render ctx in
  let html = Option.get html in
  (* Section 1: role rank first (owner < contributor), then newest first. *)
  let s1 = between md ~start:"## 1. Repos I participate in" ~stop:"## 2." in
  Alcotest.(check bool)
    "section 1 order" true
    (index_of s1 "example-user/new-owned" < index_of s1 "example-user/old-owned"
    && index_of s1 "example-user/old-owned" < index_of s1 "someone/contributed");
  Alcotest.(check bool)
    "evidence formatting" true
    (contains s1 "| contributor | 20 commits (rank 2/5, 10%) |");
  Alcotest.(check bool) "local path uses ~" true (contains s1 "`~/dev/new-owned`");
  (* Section 2: domain order, legacy domain under Other (EC-21), curated what. *)
  Alcotest.(check bool)
    "OCaml libraries before Other" true
    (index_of md "### OCaml libraries" < index_of md "### Other");
  Alcotest.(check bool)
    "legacy domain under Other" true
    (contains (between md ~start:"### Other" ~stop:"## 3.") "| [example-user/old-owned]");
  Alcotest.(check bool) "what column" true (contains md "| A lib |");
  (* Section 4 and section 7: org order case-insensitive, archived last, escaping. *)
  Alcotest.(check bool)
    "local-only row" true
    (contains md "| `~/dev/only-local` | local only, no remote | 2026-01-02 | 3 / 4 |");
  Alcotest.(check bool)
    "orgs case-insensitive" true
    (index_of md "### alpha-org (2)" < index_of md "### Zeta-Org (1)");
  Alcotest.(check bool)
    "archived after live" true
    (index_of md "[b-tier2]" < index_of md "[a-tier2]");
  Alcotest.(check bool) "md escaping" true (contains md "pipe \\| and newline");
  (* HTML: same row counts per table as the Markdown. *)
  Alcotest.(check int) "t-part rows" 3 (html_table_rows html "t-part");
  Alcotest.(check int) "t-tier1 rows" 3 (html_table_rows html "t-tier1");
  Alcotest.(check int) "t-tier2 rows" 3 (html_table_rows html "t-tier2");
  Alcotest.(check int) "t-local rows" 1 (html_table_rows html "t-local");
  (* EC-23: a never-refreshed DB renders empty sections and zero counts. *)
  let _, md0, _ = render (empty_ctx ()) in
  Alcotest.(check bool)
    "zero counts" true
    (contains md0
       "**Counts:** github: 0 total, 0 tier-1, 0 tier-2, 0 forks, 0 archived, 0 \
        example-collab; gitlab: 0 total")

(* FR-101: every text naming a person, an organisation or a place comes from
   sources.toml; an empty configuration renders generic texts and no notes page. *)
let config_texts () =
  let ctx = sections_fixture () in
  let _, md, html = render ctx in
  let html = Option.get html in
  List.iter
    (fun s -> Alcotest.(check bool) ("md has " ^ s) true (contains md s))
    [
      "# Ada Example — repository & library index\n";
      "Sources: GitHub user `example-user` + all repos visible to him \
       (owner/collaborator/org member), orgs `example-org`, `example-lab`, \
       `example-builders`, `example-misc`, `other-org`, collaborator `example-collab`; \
       member orgs reached through affiliation (example-org); GitLab user `ExampleUser` \
       (owned, contributed, MRs) and groups `acme`, `acme-campaigns` (with subgroups); \
       local `~/work`.";
      "## 5. example-collab — tools and libraries (collaborator)";
      "| Interest / reuse | Ada contributes |";
      "campaigns (GitLab group `acme-campaigns`, local `~/work/campaigns`). Described";
      "- Whitehat work lives in GitLab group `acme-campaigns`";
      "local ~/work git repos: ";
      "## How to use this index\n\n- Before building infrastructure";
      "/<date>-library-index.html` (written by `library-index render`).";
    ];
  Alcotest.(check bool) "probe org not listed" false (contains md "probe-org");
  Alcotest.(check bool)
    "html title" true
    (contains html "<h1>Ada Example — repository &amp; library index</h1>");
  Alcotest.(check bool) "html column" true (contains html "<th>Ada contributes</th>");
  (* An empty configuration: generic texts, no name, no notes page without a notes dir. *)
  let ctx = { ctx with config = Config.empty; identity = Identity.empty } in
  let dir = temp_dir () in
  let out = Filename.concat dir "INDEX.md" in
  let settings =
    Render_data.settings Config.empty ~identity:Identity.empty ~db_path:ctx.db_path
      ~index_md:out ~notes_dir:None ~home
  in
  let o = ok_or_fail (Render.run ctx.db ~settings ~out ~notes_dir:None ~now ~home) in
  Alcotest.(check (option string)) "no notes page" None o.notes_html;
  Alcotest.(check (list string)) "no warning" [] o.warnings;
  let md = read_file out in
  List.iter
    (fun s -> Alcotest.(check bool) ("generic md has " ^ s) true (contains md s))
    [
      "# Repository & library index\n";
      "Sources: none configured.";
      "## 5. Collaborators — tools and libraries (collaborator)";
      "| Owner contributes |";
      "Tools built inside security-research campaigns. Described";
    ];
  List.iter
    (fun s -> Alcotest.(check bool) ("generic md lacks " ^ s) false (contains md s))
    [ "GitHub user"; "Ada "; "Rendered view"; "~/work"; "GitLab group" ]

let helpers () =
  Alcotest.(check string)
    "html escape" "&lt;a href=&quot;x&quot;&gt;&amp;&#x27;"
    (Render_html.escape {|<a href="x">&'|});
  Alcotest.(check string) "md_esc" "a\\|b c" (Render_md.md_esc "a|b\nc");
  Alcotest.(check string)
    "d10 by code point" "é123456789"
    (Render_data.d10 (Some "é123456789abc"));
  Alcotest.(check string) "take" "日本" (Utf8.take "日本語" 2);
  Alcotest.(check string) "tilde" "~/dev/x" (Render_data.tilde ~home "/home/tester/dev/x")

let () =
  run "render"
    [
      ("confidentiality", [ Alcotest.test_case "campaign" `Quick confidentiality ]);
      ("sections", [ Alcotest.test_case "ordering" `Quick sections ]);
      ( "config-texts",
        [ Alcotest.test_case "texts from sources.toml" `Quick config_texts ] );
      ("helpers", [ Alcotest.test_case "escaping" `Quick helpers ]);
    ]
