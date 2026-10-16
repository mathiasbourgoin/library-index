type repo = {
  key : string;
  host : string;
  owner : string;
  name : string;
  full_name : string;
  url : string option;
  private_ : bool;
  fork : bool;
  archived : bool;
  language : string option;
  description : string;
  synopsis : string option;
  what : string option;
  interest : string option;
  domain : string option;
  last_commit : string option;
  pushed_at : string option;
  status : string;
  tier : string;
  role : string option;
  evidence : string;
  local_paths : string list;
  parent : string option;
  fork_note : string option;
  campaign : bool;
  ownership : string;
  registered : bool;
}

type local = {
  path : string;
  remote_key : string option;
  last_commit : string option;
  my_commits : int;
  total_commits : int;
}

type tool = {
  campaign : string;
  tool_path : string;
  tool_what : string;
  tool_language : string;
  tool_last_commit : string;
  reuse : string;
}

type settings = {
  identity : Identity.t;
  title : string;
  how_to : string list;
  inherited_examples : string;
  contributes_column : string;
  whitehat_where : string;
  fixed_gaps : string list;
  db_display : string;
  index_display : string;
}

type t = {
  settings : settings;
  generated : string;
  repos : repo list;
  section4 : local list;
  tools : tool list;
  gaps : string list;
  counts : (string * string) list;
  member_orgs : string list;
}

(* ------------------------------------------------------------ formatting helpers *)

let d10 s = Utf8.take (Option.value s ~default:"") 10
let first_set l = List.find_map (function Some s when s <> "" -> Some s | _ -> None) l
let last_date (r : repo) = first_set [ r.last_commit; r.pushed_at ]

let date_number s =
  let b = Buffer.create 10 in
  String.iter (fun c -> if c >= '0' && c <= '9' then Buffer.add_char b c) (d10 s);
  Option.value (int_of_string_opt (Buffer.contents b)) ~default:0

let role_rank = function
  | Some "owner" -> 0
  | Some "main author" -> 1
  | Some "contributor (major)" -> 2
  | Some "contributor" -> 3
  | Some "org member" -> 4
  | Some "occasional" -> 5
  | _ -> 9

let is_campaign (r : repo) = r.campaign

let what (r : repo) =
  Classify.what_fallback ~what:r.what ~synopsis:r.synopsis
    ~description:(Some r.description)

let replace_all ~sub ~by s =
  let k = String.length sub and n = String.length s in
  if k = 0 then s
  else
    let b = Buffer.create n in
    let rec go i =
      if i > n - k then Buffer.add_string b (String.sub s i (n - i))
      else if String.sub s i k = sub then (
        Buffer.add_string b by;
        go (i + k))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
    in
    go 0;
    Buffer.contents b

let tilde ~home p = replace_all ~sub:home ~by:"~" p

let stable_sort_by key cmp l =
  List.map (fun x -> (key x, x)) l
  |> List.stable_sort (fun (a, _) (b, _) -> cmp a b)
  |> List.map snd

let local_date t =
  let tm = Unix.localtime (Ptime.to_float_s t) in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

(* ------------------------------------------------------------ loading *)

let repo_sql =
  {|SELECT key, host, owner, name, full_name, url, private, fork, archived, language,
  description, synopsis, what, interest, effective_domain, last_commit, pushed_at,
  status_override, tier, role, evidence, local_paths, parent, maintained_side, sources,
  parent_pushed_at, parent_archived, campaign, ownership, registered
FROM repos WHERE fetched = 1 AND missing_since IS NULL AND host <> 'whitehat'
  AND exclude_from_page = 0
ORDER BY key|}

let repo_of_row ~now (r : Sqlite3.Data.t array) =
  let s i = Db.to_string_opt r.(i) in
  let str i = Option.value (s i) ~default:"" in
  let b i = Db.to_bool r.(i) in
  let fork = b 7 and archived = b 8 in
  let last_commit = s 15 and pushed_at = s 16 in
  ( {
      key = str 0;
      host = str 1;
      owner = str 2;
      name = str 3;
      full_name = str 4;
      url = s 5;
      private_ = b 6;
      fork;
      archived;
      language = s 9;
      description = str 10;
      synopsis = s 11;
      what = s 12;
      interest = s 13;
      domain = s 14;
      last_commit;
      pushed_at;
      status = Status.effective ~now ~override:(s 17) ~archived ~last_commit ~pushed_at;
      tier = str 18;
      role = s 19;
      evidence = str 20;
      local_paths = Jsonx.stored_strings (s 21);
      parent = s 22;
      (* Full maintained-side rule (refresh.py:514-525). The prototype rendered from
         build_json rows that dropped parent_pushed_at/parent_archived, so its column
         always read "fork (his)"; that bug is deliberately not reproduced (owner field
         "maintained side"); the differing lines are listed in scripts/parity-allowlist.txt. *)
      fork_note =
        Classify.fork_note ~fork ~parent:(s 22) ~maintained_side:(s 23) ~last_commit
          ~pushed_at ~parent_pushed_at:(s 25) ~parent_archived:(b 26);
      campaign = b 27;
      ownership = Option.value (s 28) ~default:"external";
      registered = b 29;
    },
    Jsonx.stored_strings (s 24) )

let load_locals db =
  Db.query db
    {|SELECT path, remote_key, last_commit, my_commits, total_commits, worktree
FROM local_checkouts ORDER BY ord|}
    [] (fun r ->
      ( {
          path = Option.value (Db.to_string_opt r.(0)) ~default:"";
          remote_key = Db.to_string_opt r.(1);
          last_commit = Db.to_string_opt r.(2);
          my_commits = Option.value (Db.to_int_opt r.(3)) ~default:0;
          total_commits = Option.value (Db.to_int_opt r.(4)) ~default:0;
        },
        Db.to_bool r.(5) ))

let load_tools db =
  Db.query db
    {|SELECT owner, full_name, what, language, last_commit, interest FROM repos
WHERE host = 'whitehat' ORDER BY ord, key|}
    [] (fun r ->
      let s i = Option.value (Db.to_string_opt r.(i)) ~default:"" in
      {
        campaign = s 0;
        tool_path = s 1;
        tool_what = s 2;
        tool_language = s 3;
        tool_last_commit = s 4;
        reuse = s 5;
      })

(* build_json: local_only then local_other_remotes (refresh.py:539-541). *)
let section4 repos locals =
  let keys = Hashtbl.create 1024 in
  List.iter (fun (r : repo) -> Hashtbl.replace keys r.key ()) repos;
  let local_only = List.filter (fun ((l : local), _) -> l.remote_key = None) locals in
  let unindexed =
    List.filter
      (fun ((l : local), worktree) ->
        match l.remote_key with
        | Some k -> (not (Hashtbl.mem keys k)) && not worktree
        | None -> false)
      locals
  in
  List.map fst (local_only @ unindexed)

(* ------------------------------------------------------------ counts and gaps *)

let count p l = List.length (List.filter p l)

let collab_label (id : Identity.t) =
  match id.gh_collabs with [] -> "collab" | l -> String.concat "/" l

let host_counts ~id repos host =
  let rs = List.filter (fun (r : repo) -> r.host = host) repos in
  Printf.sprintf "%d total, %d tier-1, %d tier-2, %d forks, %d archived, %d %s"
    (List.length rs)
    (count (fun (r : repo) -> r.tier = "1" && r.ownership <> "external") rs)
    (count (fun (r : repo) -> r.tier = "2") rs)
    (count (fun (r : repo) -> r.fork) rs)
    (count (fun (r : repo) -> r.archived) rs)
    (count (fun (r : repo) -> r.tier = "collab") rs)
    (collab_label id)

let local_label (id : Identity.t) =
  match id.local_roots with
  | [] -> "local git repos"
  | roots -> "local " ^ String.concat ", " roots ^ " git repos"

let counts ~id repos locals =
  [
    ("github", host_counts ~id repos "github");
    ("gitlab", host_counts ~id repos "gitlab");
    ( "local-only Tier-1 entries",
      string_of_int (count (fun (r : repo) -> r.host = "local") repos) );
    ( local_label id,
      Printf.sprintf "%d (%d without a GitHub/GitLab remote)" (List.length locals)
        (count (fun ((l : local), _) -> l.remote_key = None) locals) );
  ]

let default_fixed_gaps =
  [
    "GitLab project languages come from the languages API; GitLab contributor stats were \
     queried only for projects with his MRs / contributed-project flag (sparingly), so \
     Tier-2 GitLab rows may hide minor involvement.";
  ]

(* Local repos with an unreadable remote, in local scan order (the prototype's dict
   insertion order for local rows). Gaps are rendered, so a campaign row is never named
   (AM-17). *)
let unreachable_line rows locals =
  let ord_of (r : repo) =
    let path = replace_all ~sub:"file://" ~by:"" (Option.value r.url ~default:"") in
    let rec find i = function
      | [] -> max_int
      | ((l : local), _) :: tl -> if l.path = path then i else find (i + 1) tl
    in
    find 0 locals
  in
  let unreach =
    List.filter
      (fun ((r : repo), sources) ->
        r.host = "local" && List.mem "local-unreachable-remote" sources)
      rows
    |> List.map fst |> stable_sort_by ord_of compare
  in
  if unreach = [] then []
  else
    [
      Printf.sprintf
        "%d local checkouts point at remotes the API could not read (private subgroup / \
         no access): %s — indexed from the local checkout only."
        (List.length unreach)
        (String.concat ", "
           (List.map
              (fun (r : repo) ->
                if r.campaign then "a campaign repository" else r.full_name)
              unreach));
    ]

let default_how_to ~notes_display =
  [
    "- Before building infrastructure (parsers, protocol implementations, test \
     harnesses, CLIs, TUI, crypto/auth, MCP, formal-methods tooling), search for an \
     existing library first: `library-index search <topic>` (or the `search` tool of the \
     `library-index` MCP server), then `library-index get <repo>` / `library-index list \
     --domain <domain>`.";
    "- Prefer reuse; if a capability is missing, extend the library upstream in its own \
     repo (PR), not a local workaround.";
    "- Human fields (what / interest / domain / consumers / status) live in the \
     database: set them with `library-index annotate <repo> --set <field>=<value>` and \
     record an adoption with `library-index record-usage <repo> <consumer>` (MCP tools \
     `annotate` / `record_usage`). `curated.json` and `whitehat_tools.json` are exports \
     rewritten by the tool; do not edit them by hand.";
    "- Refresh the metadata with `library-index refresh`, then regenerate this file and \
     the rendered view with `library-index render`.";
  ]
  @
  match notes_display with
  | None -> []
  | Some d ->
      [
        Printf.sprintf
          "- Rendered view: `%s/<date>-library-index.html` (written by `library-index \
           render`)."
          d;
      ]

let settings (c : Config.t) ~identity ~db_path ~index_md ~notes_dir ~home =
  let display p = if home = "" then p else tilde ~home p in
  {
    identity;
    title = c.render.title;
    how_to =
      (match c.render.how_to with
      | Some l -> l
      | None -> default_how_to ~notes_display:(Option.map display notes_dir));
    inherited_examples = c.render.inherited_examples;
    contributes_column = c.render.contributes_column;
    whitehat_where = c.render.whitehat_where;
    fixed_gaps = Option.value c.render.fixed_gaps ~default:default_fixed_gaps;
    db_display = display db_path;
    index_display = display index_md;
  }

let load db ~settings ~now =
  let rows = Db.query db repo_sql [] (repo_of_row ~now) in
  let repos = List.map fst rows in
  let locals = load_locals db in
  let stored_gaps =
    Db.query db "SELECT text FROM gaps ORDER BY ord" [] (fun r ->
        Option.value (Db.to_string_opt r.(0)) ~default:"")
  in
  let id = settings.identity in
  {
    settings;
    generated = local_date now;
    repos;
    section4 = section4 repos locals;
    tools = load_tools db;
    gaps = stored_gaps @ unreachable_line rows locals @ settings.fixed_gaps;
    counts = counts ~id repos locals;
    member_orgs = Globals.member_orgs db;
  }

let external_registered t =
  List.filter
    (fun r -> r.registered && r.ownership = "external" && not r.campaign)
    t.repos

let in_section7 r = r.tier = "2" && not (r.registered && r.ownership = "external")
