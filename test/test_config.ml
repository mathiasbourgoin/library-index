(* Configuration (spec FR-100..FR-103, AC-31): sources.toml parsing, identity built from it
   with empty defaults, and the data-directory layout.
   - parse: the example and test configurations parse; strictness (unknown table, key,
     kind, wrong type, missing name, duplicate user); defaults of a missing file.
   - identity: the empty identity matches nobody; a configuration drives listings,
     classification and campaign rules; the domain vocabulary ends with "Other".
   - data-dir: --data-dir > LIBRARY_INDEX_DATA > directory of an explicit DB > XDG. *)

open Library_index
open Test_support.Support

let contains s sub = Re.execp (Re.compile (Re.str sub)) s

let parse_error label toml expected =
  match Config.of_string toml with
  | Ok _ -> Alcotest.failf "%s: accepted" label
  | Error e -> Alcotest.(check bool) (label ^ ": " ^ e) true (contains e expected)

let parse () =
  let example =
    first_existing "examples/sources.example.toml"
      [
        Filename.concat exe_dir "../examples/sources.example.toml";
        "examples/sources.example.toml";
        "../examples/sources.example.toml";
      ]
  in
  let c = ok_or_fail (Config.load example) in
  Alcotest.(check bool) "example has sources" true (c.sources <> []);
  Alcotest.(check int) "test config sources" 15 (List.length config.sources);
  parse_error "unknown table" "[sources]\n" "unknown key 'sources'";
  parse_error "unknown key" "[owner]\nname = \"x\"\n" "owner: unknown key 'name'";
  parse_error "unknown kind" "[[source]]\nkind = \"bitbucket-user\"\nname = \"x\"\n"
    "unknown source kind 'bitbucket-user'";
  parse_error "missing name" "[[source]]\nkind = \"github-org\"\n" "missing key 'name'";
  parse_error "wrong type" "[[source]]\nkind = \"github-org\"\nname = \"x\"\nown = 1\n"
    "expected a boolean";
  parse_error "two users"
    "[[source]]\n\
     kind = \"github-user\"\n\
     name = \"a\"\n\n\
     [[source]]\n\
     kind = \"github-user\"\n\
     name = \"b\"\n"
    "at most one github-user";
  parse_error "bad regex" "[classify]\nmirror_name_regex = \"(\"\n"
    "invalid regular expression";
  parse_error "bad confidential"
    "[[source]]\nkind = \"github-org\"\nname = \"x\"\nconfidential = \"secret\"\n"
    "generic, exclude-from-page, none";
  parse_error "syntax" "[owner\n" "sources.toml";
  (* A missing file is the empty configuration (FR-101: empty defaults). *)
  let dir = temp_dir () in
  let empty = ok_or_fail (Config.load_dir dir) in
  Alcotest.(check bool) "missing file = empty" true (empty = Config.empty);
  Alcotest.(check (list string)) "empty: no name" [] (Config.names Config.empty);
  (* names: users, orgs, groups, owner names and rule owners, never paths or regexes. *)
  let names = Config.names config in
  List.iter
    (fun n -> Alcotest.(check bool) ("names has " ^ n) true (List.mem n names))
    [ "example-user"; "ExampleUser"; "example-collab"; "acme-campaigns"; "chainx" ];
  Alcotest.(check bool) "no local root" false (List.mem "~/work" names);
  Alcotest.(check bool)
    "expand ~" true
    (Config.expand_home ~home:"/h" "~/x" = "/h/x"
    && Config.expand_home ~home:"/h" "/a" = "/a")

let facts ?(fork = false) ?(desc = "") ?(prs = 0) ~owner name =
  {
    Classify.host = "github";
    owner;
    name;
    full_name = owner ^ "/" ^ name;
    fork;
    description = desc;
    contrib = None;
    search_commits = 0;
    search_prs = prs;
    mrs = 0;
    local_my_commits = 0;
    gl_contributed = false;
  }

let identity () =
  let e = Identity.empty in
  Alcotest.(check bool) "empty: no user" false (Identity.is_user e "anyone");
  Alcotest.(check bool) "empty: no campaign" false (Campaign.of_owner ~id:e "x");
  Alcotest.(check (option string))
    "empty: no derived domain" None
    (Classify.computed_domain ~id:e ~owner:"acme-campaigns");
  Alcotest.(check (list string)) "empty: default domains" Vocab.default_domains e.domains;
  Alcotest.(check (option string)) "empty: no author" None e.author_regex;
  (* The test configuration drives every rule. *)
  Alcotest.(check bool)
    "user (case-insensitive)" true
    (Identity.is_user id "Example-User");
  Alcotest.(check bool) "gitlab user" true (Identity.is_user id "exampleuser");
  Alcotest.(check bool) "collab" true (Identity.is_collab id "example-collab");
  Alcotest.(check (list string))
    "own orgs in order"
    [ "example-org"; "example-lab"; "example-builders"; "example-misc" ]
    (Identity.own_orgs id);
  Alcotest.(check bool)
    "probe not listed" false
    (List.mem "probe-org" (Identity.listed_orgs id));
  Alcotest.(check bool)
    "campaign via source flag" true
    (Campaign.of_owner ~id "acme-campaigns/sub");
  Alcotest.(check bool)
    "campaign via classify list" true
    (Campaign.of_owner ~id "Audit-House");
  Alcotest.(check (option string))
    "derived domain (exact rule)" (Some "Security research")
    (Classify.computed_domain ~id ~owner:"acme/private");
  Alcotest.(check (option string))
    "exact rule is not a prefix" None
    (Classify.computed_domain ~id ~owner:"acme/private-x");
  let role f = fst (Classify.participation ~id f) in
  Alcotest.(check (option string))
    "org member" (Some "org member")
    (role (facts ~owner:"example-org" "x"));
  Alcotest.(check (option string))
    "major contributor" (Some "contributor (major)")
    (role (facts ~prs:20 ~owner:"chainx" "x"));
  Alcotest.(check (option string))
    "mirror name → inherited" (Some "inherited")
    (role { (facts ~owner:"other" "chainx-fork") with search_commits = 3 });
  Alcotest.(check (option string))
    "same repo, empty identity: occasional" (Some "occasional")
    (fst
       (Classify.participation ~id:Identity.empty
          { (facts ~owner:"other" "chainx-fork") with search_commits = 3 }));
  let domains =
    (Identity.of_config
       {
         Config.empty with
         classify = { Config.empty.classify with domains = Some [ "B"; "Other"; "A" ] };
       })
      .domains
  in
  Alcotest.(check (list string)) "Other last, once" [ "B"; "A"; "Other" ] domains

let with_env vars f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) vars in
  let set k = function Some v -> Unix.putenv k v | None -> Unix.putenv k "" in
  List.iter (fun (k, v) -> set k v) vars;
  Fun.protect ~finally:(fun () -> List.iter (fun (k, v) -> set k v) saved) f

let data_dir () =
  let r ?data_dir ?db () = ok_or_fail (Ctx.resolve_data_dir ~data_dir ~db) in
  with_env
    [
      ("LIBRARY_INDEX_DATA", None);
      ("LIBRARY_INDEX_DB", None);
      ("XDG_DATA_HOME", Some "/xdg");
      ("HOME", Some "/home/t");
    ]
    (fun () ->
      Alcotest.(check string) "XDG default" "/xdg/library-index" (r ());
      Alcotest.(check string) "flag wins" "/d" (r ~data_dir:"/d" ~db:"/x/y.db" ());
      Alcotest.(check string) "explicit db's dir" "/x" (r ~db:"/x/y.db" ());
      Alcotest.(check string)
        "db in the data dir" "/d/library-index.db"
        (Ctx.resolve_db_path ~db:None ~data_dir:"/d");
      Alcotest.(check string)
        "--db overrides" "/x/y.db"
        (Ctx.resolve_db_path ~db:(Some "/x/y.db") ~data_dir:"/d");
      with_env
        [ ("XDG_DATA_HOME", None) ]
        (fun () ->
          Alcotest.(check string)
            "HOME fallback" "/home/t/.local/share/library-index" (r ()));
      with_env
        [ ("LIBRARY_INDEX_DATA", Some "/env") ]
        (fun () -> Alcotest.(check string) "env over db dir" "/env" (r ~db:"/x/y.db" ()));
      with_env
        [ ("LIBRARY_INDEX_DB", Some "/z/w.db") ]
        (fun () ->
          Alcotest.(check string) "LIBRARY_INDEX_DB's dir" "/z" (r ());
          Alcotest.(check string)
            "LIBRARY_INDEX_DB path" "/z/w.db"
            (Ctx.resolve_db_path ~db:None ~data_dir:"/d")))

let () =
  run "config"
    [
      ("parse", [ Alcotest.test_case "sources.toml parsing" `Quick parse ]);
      ("identity", [ Alcotest.test_case "identity from config" `Quick identity ]);
      ("data-dir", [ Alcotest.test_case "data-dir resolution" `Quick data_dir ]);
    ]
