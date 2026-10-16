let ( let* ) = Result.bind
let sprintf = Printf.sprintf

type outcome = Reg_common.outcome = {
  json : Yojson.Safe.t;
  gaps : string list;
  complete : bool;
}

let closed = Reg_common.closed
let parse_kind = Reg_common.parse_kind
let edit_config = Reg_common.edit_config
let created_name = Reg_common.created_name
let home = Reg_common.home
let with_field = Reg_common.with_field
let source_json = Reg_common.source_json

(* ------------------------------------------------------------ register-repo *)

type repo_args = {
  target : string;
  ownership : Config.ownership option;
  confidential : Config.confidential option;
  campaign : bool option;
  deep : bool option;
  curated : (string * Yojson.Safe.t) list;
}

let repo_keys =
  [
    "target"; "ownership"; "confidential"; "campaign"; "domain"; "notes"; "rules"; "deep";
  ]

let repo_args (ctx : Ctx.t) args =
  let* o = Args.object_ ~allowed:repo_keys args in
  let* target = Args.req_string o "target" in
  let* ownership = Args.opt_string o "ownership" in
  let* ownership = closed "ownership" Config.ownership_names ownership in
  let* confidential = Args.opt_string o "confidential" in
  let* confidential = closed "confidential" Config.confidential_names confidential in
  let* campaign = Args.opt_bool o "campaign" in
  let* deep = Args.opt_bool o "deep" in
  let* domain = Args.opt_string o "domain" in
  let* () =
    match domain with
    | Some d when Vocab.canonical ctx.identity.domains d = None ->
        Error (Vocab.invalid_value ~what:"domain" d ctx.identity.domains)
    | _ -> Ok ()
  in
  let* notes = Args.opt_string o "notes" in
  let* rules = Args.opt_string o "rules" in
  let curated =
    List.filter_map
      (fun (k, v) -> Option.map (fun s -> (k, `String s)) v)
      [ ("domain", domain); ("notes", notes); ("rules", rules) ]
  in
  Ok { target; ownership; confidential; campaign; deep; curated }

let same_repo (t : Target.t) (r : Config.repo) =
  (match r.key with Some k -> Key.normalize k = t.key | None -> false)
  || Target.lookup_key r.target = Some t.key
  || r.target = t.target

let merged ~existing (t : Target.t) (a : repo_args) : Config.repo =
  let base : Config.repo =
    match existing with
    | Some r -> r
    | None ->
        {
          target = t.target;
          key = None;
          ownership = None;
          own = false;
          confidential = Not_confidential;
          campaign = false;
          campaign_prefix = false;
          deep = false;
        }
  in
  {
    base with
    target = t.target;
    key = Some t.key;
    ownership = (match a.ownership with Some o -> Some o | None -> base.ownership);
    confidential = Option.value a.confidential ~default:base.confidential;
    campaign = Option.value a.campaign ~default:base.campaign;
    deep = Option.value a.deep ~default:base.deep;
  }

let row_exists db key =
  Db.query_one db "SELECT 1 FROM repos WHERE key = ?" [ Db.text key ] (fun _ -> ())
  <> None

(* A new forge repository must exist: a 404 registers nothing. Other failures (network,
   rate limit) do not block registration; the fetch report carries them. *)
let probe ~call (t : Target.t) =
  let argv =
    if t.host = "github" then Commands.gh (Commands.gh_repo_path t.full_name)
    else Commands.glab (Commands.gl_project_path ~id:None ~full_name:t.full_name)
  in
  match call { Commands.purpose = Metadata; argv } with
  | Error `Not_found ->
      Error (sprintf "repository not found: %s (nothing registered)" (Redact.text t.key))
  | _ -> Ok ()

let split_full_name full_name =
  match String.rindex_opt full_name '/' with
  | Some i ->
      ( String.sub full_name 0 i,
        String.sub full_name (i + 1) (String.length full_name - i - 1) )
  | None -> ("", full_name)

let opt_int = function Some n -> Db.int n | None -> Sqlite3.Data.NULL

(* A checkout without a forge remote has no API language: the prototype's rule over its
   tracked files, as a full refresh would compute it. *)
let local_language ~call (t : Target.t) =
  match (t.host, t.local_dir) with
  | "local", Some dir -> (
      match call { Commands.purpose = Local_git; argv = Commands.git_ls_files dir } with
      | Ok out ->
          Classify.language_of_files
            (List.filter (( <> ) "") (String.split_on_char '\n' out))
      | Error _ -> None)
  | _ -> None

(* The row a registration creates before its first fetch: identity columns only
   ([fetched = 0]), plus the checkout facts of a directory target. Only a row this insert
   creates carries [created_by_registration = 1]: unregistering may delete that row and no
   other (FR-105). *)
let stub_owner (t : Target.t) =
  if t.host = "local" then ("(local)", t.full_name) else split_full_name t.full_name

(* The target as a raw-shape row for {!Campaign.decide}: its key, owner, name and, for a
   directory target, its checkout path. *)
let target_row (t : Target.t) =
  let owner, name = stub_owner t in
  `Assoc
    ([ ("key", `String t.key); ("owner", `String owner); ("name", `String name) ]
    @
    match t.local_dir with
    | Some d -> [ ("local_paths", `List [ `String d ]) ]
    | None -> [])

let insert_stub db ~id (t : Target.t) ~language ~campaign =
  let owner, name = stub_owner t in
  let url =
    match (t.host, t.local_dir) with
    | "github", _ -> Some ("https://github.com/" ^ t.full_name)
    | "gitlab", _ -> Some ("https://gitlab.com/" ^ t.full_name)
    | _, Some d -> Some ("file://" ^ d)
    | _ -> None
  in
  let paths =
    match t.local_dir with
    | Some d -> Db.text (Yojson.Safe.to_string (`List [ `String d ]))
    | None -> Sqlite3.Data.NULL
  in
  let c = t.checkout in
  let count f = match c with Some c -> Db.int (f c) | None -> Sqlite3.Data.NULL in
  Db.run db
    {|INSERT INTO repos(key, host, owner, name, full_name, url, private, local_paths,
  local_worktrees, local_my_commits, local_total_commits, last_commit_local, language,
  campaign, created_by_registration)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1) ON CONFLICT(key) DO NOTHING|}
    [
      Db.text t.key;
      Db.text t.host;
      Db.rtext owner;
      Db.rtext name;
      Db.rtext t.full_name;
      Db.rtext_opt url;
      (if t.host = "local" then Db.int 1 else Sqlite3.Data.NULL);
      paths;
      opt_int (Option.map (fun (c : Local_scan.checkout) -> Bool.to_int c.worktree) c);
      count (fun c -> c.my_commits);
      count (fun c -> c.total_commits);
      (match Option.bind c (fun c -> c.last_commit) with
      | Some l -> Db.text l
      | None -> Sqlite3.Data.NULL);
      Db.rtext_opt language;
      Db.int (Bool.to_int campaign);
    ];
  (* A known row reached through a campaign checkout becomes a campaign row; the flag is
     never cleared here. *)
  if campaign then Campaign.mark db id t.key;
  (* A known row registered through its checkout learns where the checkout is. *)
  Db.run db "UPDATE repos SET local_paths = ? WHERE key = ? AND local_paths IS NULL"
    [ paths; Db.text t.key ]

(* A deep registration runs the arch-index analysis after the fetch, without building;
   its outcome (also when skipped or failed) never fails the registration. *)
let deep_analysis (ctx : Ctx.t) env ?deadline ~(entry : Config.repo) key =
  if not entry.deep then `Assoc [ ("requested", `Bool false) ]
  else if match deadline with Some d -> Unix.gettimeofday () >= d | None -> false then
    `Assoc
      [
        ("requested", `Bool true);
        ("status", `String "deferred");
        ( "cause",
          `String
            "the registration deadline was reached: continue with `library-index analyze \
             <repo>` (or analyze-repo)" );
      ]
  else
    match Analysis.analyze ctx ~run:env.Repo_refresh.analyze ~allow_build:false key with
    | Ok (`Assoc fields) -> `Assoc (("requested", `Bool true) :: fields)
    | Ok _ -> `Assoc [ ("requested", `Bool true) ]
    | Error e ->
        `Assoc
          [
            ("requested", `Bool true); ("status", `String "failed"); ("cause", `String e);
          ]

let repo_result ctx env ?deadline ~outcome ~(entry : Config.repo)
    (rep : Repo_refresh.report) key =
  let deep = deep_analysis ctx env ?deadline ~entry key in
  let* view = Repo_view.get ctx key in
  Ok
    {
      json =
        view
        |> with_field "registered" (`String (created_name outcome))
        |> with_field "fetch" rep.json |> with_field "deep" deep;
      gaps = rep.gaps;
      complete = rep.complete;
    }

let register_locked (ctx : Ctx.t) env ~progress ?deadline (a : repo_args) =
  let* ctx = Ctx.reload ctx in
  let call = Repo_refresh.call env (Repo_refresh.limiter ?deadline ()) in
  let* t = Target.parse ~id:ctx.identity ~call ~home:(home ()) a.target in
  let local = Local_keys.configured ctx ~call in
  let* () = Local_keys.check local in
  let existing = List.find_opt (same_repo t) ctx.config.repos in
  let* () =
    if row_exists ctx.db t.key || t.host = "local" then Ok () else probe ~call t
  in
  let entry = merged ~existing t a in
  let* outcome =
    edit_config ctx Repo
      ~pred:(function `Repo r -> same_repo t r | `Source _ -> false)
      ~block:(Config_edit.render_repo entry)
  in
  let* ctx = Ctx.reload ctx in
  progress 1. "registered";
  let language = local_language ~call t in
  let campaign =
    Campaign.decide ~id:ctx.identity ~call ~local ?checkout:t.checkout (target_row t)
  in
  (* The configuration changed: every stored row is decided again in the same
     transaction (INV-C, immediate effect). *)
  let* () =
    Db.with_txn ctx.db (fun () ->
        insert_stub ctx.db ~id:ctx.identity t ~language ~campaign;
        Registry.sync ctx.db ctx.identity;
        ignore (Campaign.reclassify ctx.db ~id:ctx.identity ~call ~local);
        Ok ())
  in
  let* _ =
    if a.curated = [] then Ok `Null else Curate.annotate ctx ~repo:t.key ~fields:a.curated
  in
  let* rep =
    Repo_refresh.refresh_one ctx env ?deadline
      ~progress:(fun p m -> progress (p +. 1.) m)
      ~full:false t.key
  in
  repo_result ctx env ?deadline ~outcome ~entry rep t.key

let register_repo (ctx : Ctx.t) env ?(progress = fun _ _ -> ()) ?deadline args =
  Errors.guard (fun () ->
      let* a = repo_args ctx args in
      Refresh.with_lock ctx.export_dir (fun () ->
          register_locked ctx env ~progress ?deadline a))

(* ------------------------------------------------------------ list-sources *)

let indexed db key =
  Db.query_one db "SELECT fetched FROM repos WHERE key = ?"
    [ Db.text key ]
    (fun r -> Db.to_bool r.(0))
  = Some true

let repo_json db (r : Config.repo) =
  let key =
    match r.key with
    | Some k -> Some (Key.normalize k)
    | None -> Target.lookup_key r.target
  in
  `Assoc
    [
      ("target", `String r.target);
      ("key", Jsonx.of_string_opt key);
      ("ownership", Jsonx.of_string_opt (Option.map Config.ownership_name r.ownership));
      ("confidential", `String (Config.confidential_name r.confidential));
      ("campaign", `Bool r.campaign);
      ("deep", `Bool r.deep);
      ("indexed", `Bool (match key with Some k -> indexed db k | None -> false));
    ]

let list_sources (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* _ = Args.object_ ~allowed:[] args in
      let* ctx = Ctx.reload ctx in
      Ok
        (Redact.json
           (`Assoc
              [
                ("sources", `List (List.map source_json ctx.config.sources));
                ("repos", `List (List.map (repo_json ctx.db) ctx.config.repos));
              ])))
