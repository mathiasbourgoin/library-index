type summary = { repos : int; local_checkouts : int; gaps : int }

let ignored_fields =
  [
    "domain";
    "what";
    "interest";
    "consumers";
    "status_override";
    "maintained_side";
    "rules";
    "role";
    "tier";
    "evidence";
    "status";
    "search_commits";
    "search_prs";
  ]

let ( let* ) = Result.bind
let null = Sqlite3.Data.NULL

let bool_value = function
  | Some (`Bool b) -> Db.int (Bool.to_int b)
  | Some (`Int n) -> Db.int (if n <> 0 then 1 else 0)
  | _ -> null

let int_value = function
  | Some (`Int n) -> Db.int n
  | Some (`Bool b) -> Db.int (Bool.to_int b)
  | _ -> null

let json_value = function
  | None | Some `Null -> null
  | Some v -> Db.text (Yojson.Safe.to_string (Redact.json v))

(* Fetched columns, in SQL order. [Rtext] values are redacted, [Verbatim] are timestamps. *)
type kind = Rtext | Verbatim | Bool | Int | Json

let columns =
  [
    ("url", Rtext);
    ("private", Bool);
    ("fork", Bool);
    ("archived", Bool);
    ("language", Rtext);
    ("default_branch", Rtext);
    ("pushed_at", Verbatim);
    ("updated_at", Verbatim);
    ("parent", Rtext);
    ("parent_pushed_at", Verbatim);
    ("parent_archived", Bool);
    ("sources", Json);
    ("gl_id", Int);
    ("contrib", Json);
    ("mrs", Int);
    ("gl_contributed", Bool);
    ("local_paths", Json);
    ("local_worktrees", Int);
    ("local_my_commits", Int);
    ("local_total_commits", Int);
    ("last_commit_local", Verbatim);
    ("remote_url_hidden", Bool);
    ("last_commit", Verbatim);
  ]

let value row (name, kind) =
  let v = Jsonx.member name row in
  match kind with
  | Rtext -> Db.rtext_opt (match v with Some (`String s) -> Some s | _ -> None)
  | Verbatim -> ( match v with Some (`String s) -> Db.text s | _ -> null)
  | Bool -> (
      match (name, bool_value v) with
      | ("fork" | "archived"), Sqlite3.Data.NULL -> Db.int 0
      | _, d -> d)
  | Int -> int_value v
  | Json -> json_value v

let identity_columns =
  [ "key"; "host"; "owner"; "name"; "full_name"; "description"; "synopsis"; "campaign" ]

let all_columns = identity_columns @ List.map fst columns

(* EC-15: a row that is (now) campaign loses any stored README in the same statement;
   this is the only path that clears a stored README without a successful fetch
   (AM-18). *)
let campaign_purge =
  String.concat ", "
    (List.map
       (fun c ->
         Printf.sprintf "%s = CASE WHEN excluded.campaign = 1 THEN NULL ELSE repos.%s END"
           c c)
       [ "readme"; "readme_sha256" ])

let upsert_sql =
  let cols = String.concat ", " all_columns in
  let marks = String.concat ", " (List.map (fun _ -> "?") all_columns) in
  let sets =
    String.concat ", "
      (List.map (fun c -> Printf.sprintf "%s = excluded.%s" c c) (List.tl all_columns))
  in
  Printf.sprintf
    "INSERT INTO repos(%s, fetched, missing_since) VALUES (%s, 1, NULL) ON CONFLICT(key) \
     DO UPDATE SET %s, %s, fetched = 1, missing_since = NULL"
    cols marks sets campaign_purge

let upsert_fetched ~id db row =
  let* key =
    match Jsonx.str "key" row with
    | Some k when String.trim k <> "" -> Ok (Key.normalize k)
    | _ -> Error "raw repo row without a key"
  in
  let* host =
    match Jsonx.str "host" row with
    | Some (("github" | "gitlab" | "local") as h) -> Ok h
    | _ -> Error (Printf.sprintf "raw repo '%s': unknown host" key)
  in
  let owner = Option.value (Jsonx.str "owner" row) ~default:"" in
  let name = Option.value (Jsonx.str "name" row) ~default:"" in
  (* The flag {!Campaign.decide} set on the row, or the stored one: a stored campaign
     flag is never cleared by a write (import-raw, a refresh, a registration);
     declassifying a row needs [init --force] (AM-17, fail closed). *)
  let campaign = Campaign.persisted db row in
  let description, synopsis =
    Classify.campaign_redaction ~id ~campaign ~name
      ~description:(Jsonx.str "description" row) ~synopsis:(Jsonx.str "synopsis" row)
  in
  (* No README-derived text is stored for any campaign row, the exempt names included
     (Clarifications: campaign READMEs). *)
  let synopsis = if campaign then None else synopsis in
  Db.run db upsert_sql
    ([
       Db.text key;
       Db.text host;
       Db.rtext owner;
       Db.rtext name;
       Db.rtext (Option.value (Jsonx.str "full_name" row) ~default:"");
       Db.rtext_opt description;
       Db.rtext_opt synopsis;
       Db.int (Bool.to_int campaign);
     ]
    @ List.map (value row) columns);
  (* A pre-filled what/interest may quote the real description or README (FR-111): a
     campaign row keeps only what a person wrote. *)
  if campaign then Campaign.stored_campaign_row db key;
  Ok key

let decode kind (d : Sqlite3.Data.t) : Yojson.Safe.t =
  match (kind, d) with
  | _, Sqlite3.Data.NULL -> `Null
  | (Rtext | Verbatim), d -> (
      match Db.to_string_opt d with Some s -> `String s | None -> `Null)
  | Bool, d -> `Bool (Db.to_bool d)
  | Int, d -> ( match Db.to_int_opt d with Some n -> `Int n | None -> `Null)
  | Json, d -> (
      match Option.bind (Db.to_string_opt d) Jsonx.parse with
      | Some j -> j
      | None -> `Null)

let row_of_db db key =
  let kind c = if c = "campaign" then Bool else Rtext in
  let kinds = List.map (fun c -> (c, kind c)) identity_columns @ columns in
  let sql =
    Printf.sprintf "SELECT %s FROM repos WHERE key = ?"
      (String.concat ", " (List.map fst kinds))
  in
  Db.query_one db sql
    [ Db.text (Key.normalize key) ]
    (fun r -> `Assoc (List.mapi (fun i (c, k) -> (c, decode k r.(i))) kinds))

(* ------------------------------------------------------------ local / gaps / globals *)

let write_local_checkouts db entries =
  Db.exec db "DELETE FROM local_checkouts";
  List.iteri
    (fun ord e ->
      Db.run db
        {|INSERT INTO local_checkouts(ord, path, worktree, remote_url, remote_key,
  all_remote_keys, last_commit, my_commits, total_commits)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)|}
        [
          Db.int ord;
          Db.rtext (Option.value (Jsonx.str "path" e) ~default:"");
          (match bool_value (Jsonx.member "worktree" e) with
          | Sqlite3.Data.NULL -> Db.int 0
          | d -> d);
          Db.rtext_opt (Jsonx.str "remote_url" e);
          Db.rtext_opt (Jsonx.str "remote_key" e);
          json_value (Jsonx.member "all_remote_keys" e);
          (match Jsonx.str "last_commit" e with Some s -> Db.text s | None -> null);
          int_value (Jsonx.member "my_commits" e);
          int_value (Jsonx.member "total_commits" e);
        ])
    entries;
  List.length entries

(* Stored gaps are rendered into INDEX.md and the notes page: a line naming a campaign
   row is masked ({!Campaign.mask_gaps}, FR-051, FR-071, INV-C). *)
let replace_gaps db gaps =
  let gaps = Campaign.mask_gaps db gaps in
  Db.exec db "DELETE FROM gaps";
  List.iteri
    (fun ord g ->
      Db.run db "INSERT INTO gaps(ord, text) VALUES (?, ?)" [ Db.int ord; Db.rtext g ])
    gaps

(* ------------------------------------------------------------ derived columns *)

let derived_sql =
  {|SELECT id, host, owner, name, full_name, fork, description, contrib, mrs,
  local_my_commits, gl_contributed FROM repos
WHERE fetched = 1 AND host <> 'whitehat' AND (?1 IS NULL OR key = ?1)|}

let facts_of_row ~commits ~prs (r : Sqlite3.Data.t array) =
  let s i = Option.value (Db.to_string_opt r.(i)) ~default:"" in
  let i n = Option.value (Db.to_int_opt r.(n)) ~default:0 in
  let host = s 1 and full_name = s 4 in
  let count tbl =
    if host = "github" then
      Option.value (List.assoc_opt (String.lowercase_ascii full_name) tbl) ~default:0
    else 0
  in
  let contrib =
    Option.bind
      (Option.bind (Db.to_string_opt r.(7)) Jsonx.parse)
      Classify.contrib_of_json
  in
  {
    Classify.host;
    owner = s 2;
    name = s 3;
    full_name;
    fork = Db.to_bool r.(5);
    description = s 6;
    contrib;
    search_commits = count commits;
    search_prs = count prs;
    mrs = i 8;
    local_my_commits = i 9;
    gl_contributed = Db.to_bool r.(10);
  }

let recompute_derived ~id ?key db =
  let commits = Globals.search_counts db "gh_search_commits"
  and prs = Globals.search_counts db "gh_search_prs" in
  let key_param = match key with Some k -> Db.text (Key.normalize k) | None -> null in
  let rows =
    Db.query db derived_sql [ key_param ] (fun r ->
        (Option.value (Db.to_int_opt r.(0)) ~default:0, facts_of_row ~commits ~prs r))
  in
  List.iter
    (fun (rid, f) ->
      let d = Classify.derive ~id f in
      Db.run db
        "UPDATE repos SET role = ?, evidence = ?, tier = ?, computed_domain = ? WHERE id \
         = ?"
        [
          Db.rtext_opt d.role;
          Db.rtext d.evidence;
          Db.text d.tier;
          Db.rtext_opt d.domain;
          Db.int rid;
        ])
    rows;
  List.length rows

(* ------------------------------------------------------------ whole file *)

let list_field k raw =
  match Jsonx.member k raw with Some (`List l) -> Ok l | _ -> Error k

let repos_of raw =
  match Jsonx.member "repos" raw with
  | Some (`Assoc m) -> Ok (List.map snd m)
  | Some (`List l) -> Ok l
  | _ -> Error "raw.json: missing 'repos' object"

let import_body ~id db raw repos local gaps =
  let* n =
    List.fold_left
      (fun acc row ->
        Result.bind acc (fun n -> Result.map (fun _ -> n + 1) (upsert_fetched ~id db row)))
      (Ok 0) repos
  in
  let nl = write_local_checkouts db local in
  replace_gaps db gaps;
  List.iter (fun k -> Option.iter (Globals.set db k) (Jsonx.member k raw)) Globals.keys;
  ignore (recompute_derived ~id db);
  Ok { repos = n; local_checkouts = nl; gaps = List.length gaps }

let import ~id db raw =
  let* repos = repos_of raw in
  let* local =
    Result.map_error
      (fun k -> "raw.json: missing '" ^ k ^ "' array")
      (list_field "local" raw)
  in
  let* gaps =
    Result.map_error
      (fun k -> "raw.json: missing '" ^ k ^ "' array")
      (list_field "gaps" raw)
  in
  let gaps = Jsonx.strings gaps in
  Errors.guard (fun () ->
      Db.with_txn db (fun () ->
          let repos = Campaign.of_raw_cache ~id db ~local repos in
          import_body ~id db raw repos local gaps))

let import_file ~id db ~path =
  let* raw = Curated_io.read_json path in
  import ~id db raw
