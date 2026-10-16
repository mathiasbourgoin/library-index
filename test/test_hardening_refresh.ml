(* Review round 3 hardening, part 3 (CHECK-38, CHECK-40, CHECK-41): the campaign rule of
   every write path (full refresh, refresh --repo, registration) reads every remote of the
   repository's local checkouts under every configured local root and never clears a stored
   flag; a campaign row keeps no pre-filled what/interest; a confidential source is
   enforced; extra curated members survive unregistration; query-string credentials never
   reach a key; the guard knows the names of confidential registrations. Names are
   fictional. *)

open Library_index
open Test_support.Support
open Test_support.Hardening_fixture
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

let fn = "example-user/proj"
let key = "github:" ^ fn

(* A checkout of example-user/proj whose second remote is a campaign repository. *)
let campaign_fork parent =
  let dir = Filename.concat parent "proj" in
  F.git_checkout dir [ ("README.md", "# proj\n") ];
  commit dir ~author:"Ada Example";
  List.iter
    (fun (n, u) ->
      ignore
        (Sys.command
           (Printf.sprintf "git -C %s remote add %s %s" (Filename.quote dir) n u)))
    [
      ("origin", "git@github.com:" ^ fn ^ ".git");
      ("work", "git@gitlab.com:acme-campaigns/proj.git");
    ];
  dir

(* The synthetic configuration with its local roots replaced by [roots]. *)
let config_with_roots roots =
  let base =
    Re.replace_string
      (Re.compile (Re.str "[[source]]\nkind = \"local-root\"\nname = \"~/work\"\n"))
      ~by:""
      (read_file (fixture "sources.test.toml"))
  in
  base
  ^ String.concat ""
      (List.map
         (fun r -> Printf.sprintf "\n[[source]]\nkind = \"local-root\"\nname = %S\n" r)
         roots)

(* The quiet forge plus the user's listing of proj, whose text carries the sentinel. *)
let forge_with_proj () =
  let forge = Fake_forge.quiet () in
  let repo = Fake_forge.gh_repo ~description:sentinel fn in
  Fake_forge.json forge Fake_forge.gh_user_listing (`List [ repo ]);
  Fake_forge.json forge ("repos/" ^ fn) repo;
  Fake_forge.ok forge ("repos/" ^ fn ^ "/readme") ("# proj\n\n" ^ sentinel ^ " notes\n");
  forge

let withheld ctx what =
  Alcotest.(check (option string))
    (what ^ ": campaign") (Some "1")
    (scalar ctx "SELECT campaign FROM repos WHERE key = ?" key);
  Alcotest.(check bool)
    (what ^ ": nothing served") false
    (contains (Yojson.Safe.to_string (get ctx key)) sentinel)

let full_refresh ctx forge ~dev_root =
  ignore (Ops.refresh ctx (F.mixed_env forge) ~dev_root ~allow_full:true (`Assoc []))

(* A registered checkout outside every local root stays a campaign row through a full
   refresh. *)
let refresh_keeps_registered () =
  let roots = temp_dir () in
  let ctx = data_ctx ~config:(config_with_roots [ roots ]) () in
  let dir = campaign_fork (temp_dir ()) in
  let forge = forge_with_proj () in
  ignore
    (ok_or_fail
       (Ops.register_repo ctx (F.mixed_env forge) (`Assoc [ ("target", `String dir) ])));
  withheld ctx "registered";
  full_refresh ctx forge ~dev_root:roots;
  withheld ctx "after a full refresh"

(* A checkout under the second configured root makes the row campaign in a full refresh
   that scans only the first root itself. *)
let refresh_second_root () =
  let root1 = temp_dir () and root2 = temp_dir () in
  ignore (campaign_fork root2);
  let ctx = data_ctx ~config:(config_with_roots [ root1; root2 ]) () in
  full_refresh ctx (forge_with_proj ()) ~dev_root:root1;
  withheld ctx "second root"

(* refresh --repo of a row known only from curated data reads the configured checkouts. *)
let refresh_one_checkout () =
  let root = temp_dir () in
  ignore (campaign_fork root);
  let ctx = data_ctx ~config:(config_with_roots [ root ]) () in
  ignore (F.insert_repo ctx ~owner:"example-user" "proj");
  ignore
    (Ops.refresh ctx
       (F.mixed_env (forge_with_proj ()))
       ~allow_full:false
       (`Assoc [ ("repo", `String key) ]));
  withheld ctx "refresh --repo"

(* A campaign row keeps no pre-filled what/interest; a curated what stays. *)
let auto_what_cleared () =
  let ctx = data_ctx () in
  let k = F.insert_repo ctx ~owner:"outside-dev" ~what:sentinel "known" in
  let other = F.insert_repo ctx ~owner:"outside-dev" ~what:"mine" "kept" in
  let id_of k =
    Option.value ~default:0
      (Option.join
         (Db.query_one ctx.db "SELECT id FROM repos WHERE key = ?"
            [ Db.text k ]
            (fun r -> Db.to_int_opt r.(0))))
  in
  Field_meta.set ctx.db ~repo_id:(id_of k) ~field:"what" Auto ~now:"2026-10-01T00:00:00Z";
  let forge = Fake_forge.create () in
  Fake_forge.fail forge "repos/outside-dev/known" `Network;
  Fake_forge.fail forge "repos/outside-dev/kept" `Network;
  List.iter
    (fun t ->
      ignore
        (Ops.register_repo ctx (F.mixed_env forge)
           (`Assoc
              [
                ("target", `String ("https://github.com/" ^ t)); ("campaign", `Bool true);
              ])))
    [ "outside-dev/known"; "outside-dev/kept" ];
  Alcotest.(check (option string))
    "auto what cleared" None
    (scalar ctx "SELECT what FROM repos WHERE key = ?" k);
  Alcotest.(check (option string))
    "curated what kept" (Some "mine")
    (scalar ctx "SELECT what FROM repos WHERE key = ?" other);
  let s = ok_or_fail (Ops.search ctx (`Assoc [ ("query", `String sentinel) ])) in
  Alcotest.(check bool)
    "not searchable" false
    (contains (Yojson.Safe.to_string (member "hits" s)) sentinel)

(* confidential = "generic" alone on a source withholds its repositories; on a source
   naming no owner it is refused. *)
let generic_source () =
  let ctx = data_ctx () in
  let forge = Fake_forge.create () in
  let r = "sorg/r1" in
  Fake_forge.json forge "orgs/sorg" (`Assoc [ ("login", `String "sorg") ]);
  Fake_forge.json forge "orgs/sorg/repos?type=all&per_page=100"
    (`List [ Fake_forge.gh_repo ~description:sentinel r ]);
  Fake_forge.ok forge ("repos/" ^ r ^ "/readme") ("# r\n\n" ^ sentinel ^ "\n");
  ignore
    (ok_or_fail
       (Ops.register_source ctx (F.mixed_env forge)
          (`Assoc
             [
               ("kind", `String "github-org");
               ("name", `String "sorg");
               ("confidential", `String "generic");
             ])));
  Alcotest.(check bool)
    "nothing of the generic source" false
    (contains (Yojson.Safe.to_string (get ctx ("github:" ^ r))) sentinel);
  Alcotest.(check bool)
    "generic local-root accepted (enforced through its checkouts)" true
    (Result.is_ok
       (Config.of_string
          "[[source]]\nkind = \"local-root\"\nname = \"/x\"\nconfidential = \"generic\"\n"))

(* Extra curated.json members make a row curated: unregister keeps it. *)
let extra_kept () =
  let ctx = data_ctx () in
  let forge = Fake_forge.create () in
  let t = "outside-dev/tagged" in
  Fake_forge.json forge ("repos/" ^ t) (Fake_forge.gh_repo t);
  let target = "https://github.com/" ^ t in
  ignore
    (ok_or_fail
       (Ops.register_repo ctx (F.mixed_env forge) (`Assoc [ ("target", `String target) ])));
  Db.run ctx.db "UPDATE repos SET extra = ? WHERE key = ?"
    [ Db.text {|{"my_tag":"KEEP-ME"}|}; Db.text ("github:" ^ t) ];
  let r = ok_or_fail (Ops.unregister_repo ctx (`Assoc [ ("target", `String target) ])) in
  Alcotest.(check string) "kept" "kept" (to_str (member "row" r))

(* A query string never reaches a key, a stored target or an output. *)
let query_credentials () =
  Alcotest.(check (option string))
    "query dropped from the key" (Some "github:carol/tool2")
    (Remote.norm_remote "https://github.com/carol/tool2.git?private_token=s3cr3t");
  let ctx = data_ctx () in
  let forge = Fake_forge.create () in
  Fake_forge.json forge "repos/carol/tool2" (Fake_forge.gh_repo "carol/tool2");
  ignore
    (ok_or_fail
       (Ops.register_repo ctx (F.mixed_env forge)
          (`Assoc
             [ ("target", `String "https://github.com/carol/tool2?access_token=s3cr3t") ])));
  Alcotest.(check bool)
    "no query in sources.toml" false
    (contains (config_text ctx) "s3cr3t");
  match
    Ops.register_source ctx (F.mixed_env forge)
      (`Assoc
         [
           ("kind", `String "github-user");
           ("name", `String ("u:" ^ Fake_forge.fake_token ()));
         ])
  with
  | Ok _ -> Alcotest.fail "a token-bearing source name was accepted"
  | Error e -> Alcotest.(check bool) "refused" true (contains e "credential")

(* The guard's names include the key owners of a confidential registration. *)
let guard_names () =
  let c =
    ok_or_fail
      (Config.of_string
         "[[repo]]\n\
          target = \"/home/u/dev/localdir\"\n\
          key = \"github:client-eta/hidden-name\"\n\
          campaign = true\n\n\
          [[repo]]\n\
          target = \"https://gitlab.com/client-zeta/proj-q\"\n\
          confidential = \"generic\"\n")
  in
  let n = Config.names c in
  List.iter
    (fun x -> Alcotest.(check bool) x true (List.mem x n))
    [ "client-eta"; "hidden-name"; "client-zeta"; "proj-q" ]

(* ------------------------------------------------------------ fail closed *)

(* An env whose git refuses to read [dir]'s remotes (e.g. git's safe.directory check). *)
let unreadable_env forge dir =
  let env = F.mixed_env forge in
  {
    env with
    run =
      (fun ~timeout argv ->
        match Array.to_list argv with
        | "git" :: "-C" :: d :: rest when d = dir && List.mem "remote" rest ->
            Error `Other
        | _ -> env.run ~timeout argv);
  }

(* git refuses the checkout (renamed, so its name does not match the repository): its
   remotes come from its config file. With no readable config either, nothing is
   fetched. *)
let unreadable_remotes () =
  let root = temp_dir () in
  let dir = campaign_fork root in
  let renamed = Filename.concat root "zz-r1" in
  Unix.rename dir renamed;
  let ctx = data_ctx ~config:(config_with_roots [ root ]) () in
  let register ctx =
    Ops.register_source ctx
      (unreadable_env (forge_with_proj ()) renamed)
      (`Assoc [ ("kind", `String "github-user"); ("name", `String "example-user") ])
  in
  ignore (register ctx);
  withheld ctx "remotes from the config file";
  Sys.remove (Filename.concat renamed ".git/config");
  let ctx = data_ctx ~config:(config_with_roots [ root ]) () in
  match register ctx with
  | Ok _ -> Alcotest.fail "fetched past an unreadable checkout"
  | Error e ->
      Alcotest.(check bool) "names the checkout" true (contains e renamed);
      Alcotest.(check (option string))
        "nothing written" None
        (scalar ctx "SELECT key FROM repos WHERE key = ?" key)

let local_git_not_deadlined () =
  let env = F.mixed_env (Fake_forge.create ()) in
  let call =
    Repo_refresh.call env (Repo_refresh.limiter ~deadline:(Unix.gettimeofday () -. 1.) ())
  in
  Alcotest.(check bool)
    "local git runs after the deadline" true
    (Result.is_ok
       (call { Commands.purpose = Local_git; argv = [| "git"; "--version" |] }))

let import_raw_sticky () =
  let ctx = data_ctx () in
  ignore (F.insert_repo ctx ~owner:"example-user" ~campaign:true "proj");
  let row =
    match Norm.gh (Fake_forge.gh_repo ~description:sentinel fn) with
    | Some r -> r
    | None -> Alcotest.fail "fixture row"
  in
  ignore
    (ok_or_fail
       (Db.with_txn ctx.db (fun () ->
            Import_raw.upsert_fetched ~id:ctx.identity ctx.db row)));
  withheld ctx "import-raw upsert"

let suspicious_source_names () =
  let ctx = data_ctx () in
  let env = F.mixed_env (Fake_forge.create ()) in
  List.iter
    (fun args ->
      Alcotest.(check bool)
        "refused" true
        (Result.is_error (Ops.register_source ctx env (`Assoc args))))
    [
      [ ("kind", `String "github-org"); ("name", `String "org?private_token=zz9") ];
      [
        ("kind", `String "gitlab-group");
        ("name", `String "grp");
        ("host", `String "gitlab.example.com/?private_token=zz9");
      ];
    ];
  Alcotest.(check bool) "nothing written" false (contains (config_text ctx) "zz9")

let analysis_campaign_now () =
  let root = temp_dir () in
  let dir = campaign_fork root in
  let ctx = data_ctx ~config:(config_with_roots [ root ]) () in
  ignore (F.insert_repo ctx ~owner:"example-user" ~local_dir:dir "proj");
  let r = ok_or_fail (Analysis.analyze ctx ~run:(Some Proc.run) ~allow_build:false key) in
  Alcotest.(check string) "skipped" "skipped" (to_str (member "status" r));
  withheld ctx "analysis"

let confidential_root () =
  let root = temp_dir () in
  let dir = Filename.concat root "proj" in
  F.git_checkout dir [ ("README.md", "# p\n") ];
  commit dir ~author:"Ada Example";
  ignore
    (Sys.command
       (Printf.sprintf "git -C %s remote add origin git@github.com:%s.git"
          (Filename.quote dir) fn));
  let config =
    config_with_roots []
    ^ Printf.sprintf
        "\n[[source]]\nkind = \"local-root\"\nname = %S\nconfidential = \"generic\"\n"
        root
  in
  let ctx = data_ctx ~config () in
  full_refresh ctx (forge_with_proj ()) ~dev_root:root;
  withheld ctx "confidential local root";
  Alcotest.(check bool)
    "exclude-from-page source accepted" true
    (Result.is_ok
       (Config.of_string
          "[[source]]\n\
           kind = \"github-org\"\n\
           name = \"o\"\n\
           confidential = \"exclude-from-page\"\n"));
  Alcotest.(check bool)
    "generic search source refused" true
    (Result.is_error
       (Config.of_string
          "[[source]]\nkind = \"github-search\"\nconfidential = \"generic\"\n"))

let () =
  run "hardening-refresh"
    [
      ( "campaign",
        [
          Alcotest.test_case "full refresh keeps a registered checkout" `Quick
            refresh_keeps_registered;
          Alcotest.test_case "second local root" `Quick refresh_second_root;
          Alcotest.test_case "refresh --repo" `Quick refresh_one_checkout;
          Alcotest.test_case "auto what cleared" `Quick auto_what_cleared;
          Alcotest.test_case "generic source" `Quick generic_source;
        ] );
      ( "fail-closed",
        [
          Alcotest.test_case "unreadable remotes" `Quick unreadable_remotes;
          Alcotest.test_case "local git not deadlined" `Quick local_git_not_deadlined;
          Alcotest.test_case "import-raw keeps the flag" `Quick import_raw_sticky;
          Alcotest.test_case "suspicious source names" `Quick suspicious_source_names;
          Alcotest.test_case "analysis applies the rule" `Quick analysis_campaign_now;
          Alcotest.test_case "confidential local root" `Quick confidential_root;
        ] );
      ("curated", [ Alcotest.test_case "extra members kept" `Quick extra_kept ]);
      ( "secrets",
        [
          Alcotest.test_case "query credentials" `Quick query_credentials;
          Alcotest.test_case "guard names" `Quick guard_names;
        ] );
    ]
