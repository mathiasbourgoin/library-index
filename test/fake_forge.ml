(* A fake gh/glab/git for refresh tests: canned outputs keyed by API path (argv.(4)) for
   gh/glab, or by the whole argv for git. Unknown commands answer `Not_found ("no such
   data"), so a test only lists what matters. Every call is logged. Never spawns a
   process, never touches the network or ~/dev. *)

open Library_index

type t = {
  table : (string, (string, Proc.failure) result) Hashtbl.t;
  mutable log : string array list;
  lock : Mutex.t;
}

let create () = { table = Hashtbl.create 64; log = []; lock = Mutex.create () }

let key_of argv =
  match argv.(0) with
  | ("gh" | "glab") when Array.length argv > 4 -> argv.(4)
  | _ -> String.concat " " (Array.to_list argv)

let ok t path out = Mutex.protect t.lock (fun () -> Hashtbl.replace t.table path (Ok out))

let fail t path f =
  Mutex.protect t.lock (fun () -> Hashtbl.replace t.table path (Error f))

let json t path j = ok t path (Yojson.Safe.to_string j)

let runner t : Proc.runner =
 fun ~timeout:_ argv ->
  Mutex.protect t.lock (fun () ->
      t.log <- argv :: t.log;
      Option.value (Hashtbl.find_opt t.table (key_of argv)) ~default:(Error `Not_found))

let calls t = Mutex.protect t.lock (fun () -> List.rev t.log)
let reset_log t = Mutex.protect t.lock (fun () -> t.log <- [])
let called t path = List.exists (fun a -> key_of a = path) (calls t)

let called_matching t sub =
  let re = Re.compile (Re.str sub) in
  List.exists (fun a -> Re.execp re (key_of a)) (calls t)

let no_dev_root = "/nonexistent-library-index-dev-root"

let env t =
  { Repo_refresh.run = runner t; sleep = (fun _ -> ()); workers = 4; analyze = None }

(* ------------------------------------------------------------ JSON builders *)

let gh_repo ?(fork = false) ?(archived = false) ?(pushed = "2026-10-01T00:00:00Z")
    ?(description = "A repository") ?(language = "OCaml") full_name =
  match String.split_on_char '/' full_name with
  | [ owner; name ] ->
      `Assoc
        [
          ("full_name", `String full_name);
          ("owner", `Assoc [ ("login", `String owner) ]);
          ("name", `String name);
          ("html_url", `String ("https://github.com/" ^ full_name));
          ("private", `Bool false);
          ("fork", `Bool fork);
          ("archived", `Bool archived);
          ("language", `String language);
          ("description", `String description);
          ("default_branch", `String "main");
          ("pushed_at", `String pushed);
          ("updated_at", `String pushed);
        ]
  | _ -> invalid_arg "gh_repo"

let gl_project ?(pushed = "2026-10-01T00:00:00Z") ?(description = "A project") ?readme ~id
    full_name =
  let ns = Filename.dirname full_name and path = Filename.basename full_name in
  `Assoc
    ([
       ("id", `Int id);
       ("path_with_namespace", `String full_name);
       ("path", `String path);
       ("namespace", `Assoc [ ("full_path", `String ns) ]);
       ("web_url", `String ("https://gitlab.com/" ^ full_name));
       ("visibility", `String "private");
       ("archived", `Bool false);
       ("description", `String description);
       ("default_branch", `String "main");
       ("last_activity_at", `String pushed);
     ]
    @
    match readme with
    | Some f ->
        [
          ( "readme_url",
            `String (Printf.sprintf "https://gitlab.com/%s/-/blob/main/%s" full_name f) );
        ]
    | None -> [])

(* ------------------------------------------------------------ full-refresh listings *)

let gh_user_listing = "users/example-user/repos?type=all&per_page=100"

let gh_affiliation =
  "user/repos?affiliation=owner,collaborator,organization_member&per_page=100"

let commits_page1 = "search/commits?q=author:example-user&per_page=100&page=1"
let prs_page1 = "search/issues?q=author:example-user+type:pr&per_page=100&page=1"
let mrs_listing = "merge_requests?author_id=5&scope=all&state=all&per_page=100"

(* Every listing answers, empty; probe-org is a 404 probe; GitLab user id 5. *)
let quiet () =
  let forge = create () in
  let empty p = json forge p (`List []) in
  List.iter empty
    [
      gh_user_listing;
      gh_affiliation;
      "users/example-collab/repos?type=owner&per_page=100";
      "user/memberships/orgs";
      "users/5/projects?per_page=100";
      "users/5/contributed_projects?per_page=100";
      "groups/acme/projects?include_subgroups=true&per_page=100";
      "groups/acme-campaigns/projects?include_subgroups=true&per_page=100";
      mrs_listing;
    ];
  List.iter
    (fun (o : Identity.org) ->
      if not o.probe then (
        json forge ("orgs/" ^ o.org) (`Assoc [ ("login", `String o.org) ]);
        empty ("orgs/" ^ o.org ^ "/repos?type=all&per_page=100")))
    Support.id.gh_orgs;
  json forge commits_page1 (`Assoc [ ("items", `List []) ]);
  json forge prs_page1 (`Assoc [ ("items", `List []) ]);
  json forge "users?username=ExampleUser" (`List [ `Assoc [ ("id", `Int 5) ] ]);
  forge

(* A token-shaped string assembled at run time, so no literal token sits in the source. *)
let fake_token () =
  "gh" ^ "p_" ^ String.concat "" (List.init 36 (fun i -> string_of_int (i mod 10)))

(* ------------------------------------------------------------ CHECK-9 audit *)

(* Every command refresh may issue: GET-only gh/glab api, or a read-only git query. *)
let forbidden =
  [
    "clone";
    "push";
    "post";
    "put";
    "patch";
    "delete";
    "--method";
    "-f";
    "-F";
    "--field";
    "--raw-field";
    "--input";
  ]

let assert_get (argv : string array) =
  let l = Array.to_list argv in
  let shown = String.concat " " l in
  (match l with
  | ("gh" | "glab") :: "api" :: "-X" :: "GET" :: path :: rest ->
      Alcotest.(check bool)
        (shown ^ ": path is not a flag")
        false
        (String.length path = 0 || path.[0] = '-');
      Alcotest.(check bool)
        (shown ^ ": no .. segment") false
        (List.mem ".." (String.split_on_char '/' path));
      Alcotest.(check bool)
        (shown ^ ": no whitespace in path")
        false
        (String.exists (fun c -> c = ' ' || c = '\n') path);
      List.iter
        (fun a ->
          Alcotest.(check bool)
            (shown ^ ": allowed trailing arg " ^ a)
            true
            (List.mem a [ "--paginate"; "-H"; "Accept: application/vnd.github.raw+json" ]))
        rest
  | "git" :: "-C" :: _ :: sub ->
      Alcotest.(check bool)
        (shown ^ ": read-only git") true
        (List.mem sub
           [
             [ "remote"; "-v" ];
             [ "log"; "-1"; "--format=%cI" ];
             [ "rev-list"; "--count"; "HEAD" ];
             [ "rev-list"; "--count"; "--regexp-ignore-case"; "--author=example"; "HEAD" ];
             [ "ls-files" ];
             [ "ls-files"; "-z" ];
           ])
  | _ -> Alcotest.failf "not an allowed command: %s" shown);
  List.iter
    (fun a ->
      Alcotest.(check bool)
        (shown ^ ": no " ^ a)
        false
        (List.mem (String.lowercase_ascii a) forbidden || List.mem a forbidden))
    (List.tl l)
