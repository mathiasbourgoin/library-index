let version = 4

let fts_columns =
  [
    "name";
    "full_name";
    "aliases";
    "what";
    "synopsis";
    "description";
    "interest";
    "effective_domain";
    "consumers";
    "notes";
    "readme";
  ]

let repos =
  {|CREATE TABLE repos (
  id INTEGER PRIMARY KEY,
  key TEXT UNIQUE NOT NULL,
  host TEXT NOT NULL,
  owner TEXT NOT NULL DEFAULT '',
  name TEXT NOT NULL DEFAULT '',
  full_name TEXT NOT NULL DEFAULT '',
  url TEXT,
  private INTEGER,
  fork INTEGER NOT NULL DEFAULT 0,
  archived INTEGER NOT NULL DEFAULT 0,
  language TEXT,
  description TEXT,
  default_branch TEXT,
  pushed_at TEXT,
  updated_at TEXT,
  parent TEXT,
  parent_pushed_at TEXT,
  parent_archived INTEGER,
  sources TEXT,
  gl_id INTEGER,
  contrib TEXT,
  mrs INTEGER,
  gl_contributed INTEGER,
  local_paths TEXT,
  local_worktrees INTEGER,
  local_my_commits INTEGER,
  local_total_commits INTEGER,
  last_commit_local TEXT,
  remote_url_hidden INTEGER,
  last_commit TEXT,
  synopsis TEXT,
  readme TEXT,
  readme_sha256 TEXT,
  readme_fetched_at TEXT,
  detail_fetched_at TEXT,
  fetched INTEGER NOT NULL DEFAULT 0,
  campaign INTEGER NOT NULL DEFAULT 0,
  missing_since TEXT,
  role TEXT,
  evidence TEXT,
  tier TEXT,
  computed_domain TEXT,
  what TEXT,
  interest TEXT,
  domain TEXT,
  consumers TEXT,
  status_override TEXT,
  maintained_side TEXT,
  notes TEXT,
  rules TEXT,
  extra TEXT,
  docs_pushed_at TEXT,
  aliases TEXT,
  registered INTEGER NOT NULL DEFAULT 0,
  created_by_registration INTEGER NOT NULL DEFAULT 0,
  ownership_override TEXT CHECK (ownership_override IN ('own', 'participant', 'external')),
  exclude_from_page INTEGER NOT NULL DEFAULT 0,
  ord INTEGER,
  effective_domain TEXT GENERATED ALWAYS AS (coalesce(domain, computed_domain)) VIRTUAL,
  ownership TEXT GENERATED ALWAYS AS (coalesce(ownership_override, CASE WHEN role IN
    ('owner', 'main author') THEN 'own' WHEN role IS NOT NULL THEN 'participant' ELSE
    'external' END)) VIRTUAL
)|}

(* Provenance of curated fields (FR-112): [auto] values are regenerable pre-fills
   (FR-111) that refresh may overwrite and the export omits; [curated] values come from
   a person (annotate, record_usage, curated.json). A field without a row is curated. *)
let field_meta =
  {|CREATE TABLE field_meta (
  repo_id INTEGER NOT NULL REFERENCES repos(id) ON DELETE CASCADE,
  field TEXT NOT NULL,
  provenance TEXT NOT NULL CHECK (provenance IN ('auto', 'curated')),
  updated_at TEXT NOT NULL,
  PRIMARY KEY (repo_id, field)
)|}

(* Public API items (spec "Optional arch-index analysis", FR-120): [scan] items come from
   the per-language extractors over the files of [api_files]; [arch-index] items are
   imported from an arch-index comment_db by [Analysis]. *)
let api_items =
  {|CREATE TABLE api_items (
  id INTEGER PRIMARY KEY,
  repo_id INTEGER NOT NULL REFERENCES repos(id) ON DELETE CASCADE,
  module_path TEXT,
  name TEXT NOT NULL,
  qualified_path TEXT,
  kind TEXT,
  signature TEXT,
  doc TEXT,
  file TEXT,
  line INTEGER,
  source TEXT NOT NULL CHECK (source IN ('scan', 'arch-index')),
  analyzed_head TEXT
)|}

(* The files the extractors scanned for [scan] items: an unchanged [sha256] is not
   re-extracted; [blob_sha] (forge blob id) spares an unchanged API file a fetch. *)
let api_files =
  {|CREATE TABLE api_files (
  repo_id INTEGER NOT NULL REFERENCES repos(id) ON DELETE CASCADE,
  path TEXT NOT NULL,
  sha256 TEXT NOT NULL,
  blob_sha TEXT,
  PRIMARY KEY (repo_id, path)
)|}

let item_fts_columns = [ "name"; "module_path"; "signature"; "doc" ]

(* External-content FTS over the API items, kept exact by these triggers. *)
let item_trigger name event rows =
  let cols = String.concat ", " item_fts_columns in
  let vals row =
    String.concat ", " (List.map (fun c -> row ^ "." ^ c) item_fts_columns)
  in
  let stmt = function
    | "old" ->
        Printf.sprintf
          "INSERT INTO api_items_fts(api_items_fts, rowid, %s) VALUES ('delete', old.id, \
           %s);"
          cols (vals "old")
    | _ ->
        Printf.sprintf "INSERT INTO api_items_fts(rowid, %s) VALUES (new.id, %s);" cols
          (vals "new")
  in
  Printf.sprintf "CREATE TRIGGER %s AFTER %s ON api_items BEGIN %s END" name event
    (String.concat " " (List.map stmt rows))

let api_calls =
  {|CREATE TABLE api_calls (
  caller_id INTEGER NOT NULL REFERENCES api_items(id) ON DELETE CASCADE,
  callee_id INTEGER NOT NULL REFERENCES api_items(id) ON DELETE CASCADE,
  PRIMARY KEY (caller_id, callee_id)
)|}

let analysis =
  {|CREATE TABLE analysis (
  repo_id INTEGER PRIMARY KEY REFERENCES repos(id) ON DELETE CASCADE,
  status TEXT NOT NULL CHECK (status IN ('ok', 'partial', 'skipped', 'failed')),
  cause TEXT,
  head_sha TEXT,
  at TEXT NOT NULL,
  tool_version TEXT
)|}

(* Documents of Tier-1 libraries (FR-080). [blob_sha] is the forge's git blob id from the
   tree listing: an API file whose blob id did not change is not fetched again. *)
let documents =
  {|CREATE TABLE documents (
  id INTEGER PRIMARY KEY,
  repo_id INTEGER NOT NULL REFERENCES repos(id) ON DELETE CASCADE,
  path TEXT NOT NULL,
  kind TEXT NOT NULL CHECK (kind IN ('readme', 'doc', 'api')),
  title TEXT,
  source TEXT NOT NULL CHECK (source IN ('local', 'api')),
  fetched_at TEXT NOT NULL,
  sha256 TEXT NOT NULL,
  blob_sha TEXT,
  text TEXT NOT NULL,
  UNIQUE(repo_id, path)
)|}

let doc_chunks =
  {|CREATE TABLE doc_chunks (
  id INTEGER PRIMARY KEY,
  doc_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  ord INTEGER NOT NULL,
  title TEXT NOT NULL,
  text TEXT NOT NULL
)|}

(* External-content FTS over chunk title and text, kept exact by these triggers. *)
let chunk_trigger name event rows =
  let stmt = function
    | "old" ->
        "INSERT INTO doc_chunks_fts(doc_chunks_fts, rowid, title, text) VALUES \
         ('delete', old.id, old.title, old.text);"
    | _ ->
        "INSERT INTO doc_chunks_fts(rowid, title, text) VALUES (new.id, new.title, \
         new.text);"
  in
  Printf.sprintf "CREATE TRIGGER %s AFTER %s ON doc_chunks BEGIN %s END" name event
    (String.concat " " (List.map stmt rows))

let fts_list prefix = String.concat ", " (List.map (fun c -> prefix ^ c) fts_columns)

(* Trigger value lists spell the effective domain as the same coalesce the generated
   column uses, so a 'delete' always removes exactly what was indexed. *)
let trigger_values row =
  String.concat ", "
    (List.map
       (fun c ->
         if c = "effective_domain" then
           Printf.sprintf "coalesce(%s.domain, %s.computed_domain)" row row
         else row ^ "." ^ c)
       fts_columns)

let fts_insert row =
  Printf.sprintf "INSERT INTO repos_fts(rowid, %s) VALUES (%s.id, %s);" (fts_list "") row
    (trigger_values row)

let fts_delete row =
  Printf.sprintf
    "INSERT INTO repos_fts(repos_fts, rowid, %s) VALUES ('delete', %s.id, %s);"
    (fts_list "") row (trigger_values row)

let statements =
  [
    repos;
    "CREATE INDEX repos_full_name ON repos(lower(full_name))";
    "CREATE INDEX repos_name ON repos(lower(name))";
    {|CREATE TABLE usage (
  id INTEGER PRIMARY KEY,
  repo_id INTEGER NOT NULL REFERENCES repos(id) ON DELETE CASCADE,
  consumer TEXT NOT NULL,
  note TEXT,
  at TEXT NOT NULL
)|};
    "CREATE INDEX usage_repo ON usage(repo_id)";
    "CREATE TABLE meta (k TEXT PRIMARY KEY, v TEXT)";
    {|CREATE TABLE local_checkouts (
  ord INTEGER PRIMARY KEY,
  path TEXT NOT NULL,
  worktree INTEGER NOT NULL DEFAULT 0,
  remote_url TEXT,
  remote_key TEXT,
  all_remote_keys TEXT,
  last_commit TEXT,
  my_commits INTEGER,
  total_commits INTEGER
)|};
    "CREATE TABLE gaps (ord INTEGER PRIMARY KEY, text TEXT NOT NULL)";
    field_meta;
    api_items;
    "CREATE INDEX api_items_repo ON api_items(repo_id, source, file)";
    api_files;
    Printf.sprintf
      "CREATE VIRTUAL TABLE api_items_fts USING fts5(%s, content='api_items', \
       content_rowid='id', tokenize='porter unicode61 remove_diacritics 2')"
      (String.concat ", " item_fts_columns);
    item_trigger "api_items_ai" "INSERT" [ "new" ];
    item_trigger "api_items_ad" "DELETE" [ "old" ];
    item_trigger "api_items_au" "UPDATE" [ "old"; "new" ];
    api_calls;
    analysis;
    documents;
    "CREATE INDEX documents_repo ON documents(repo_id)";
    doc_chunks;
    "CREATE INDEX doc_chunks_doc ON doc_chunks(doc_id, ord)";
    "CREATE VIRTUAL TABLE doc_chunks_fts USING fts5(title, text, content='doc_chunks', \
     content_rowid='id', tokenize='porter unicode61 remove_diacritics 2')";
    chunk_trigger "doc_chunks_ai" "INSERT" [ "new" ];
    chunk_trigger "doc_chunks_ad" "DELETE" [ "old" ];
    chunk_trigger "doc_chunks_au" "UPDATE" [ "old"; "new" ];
    Printf.sprintf
      "CREATE VIRTUAL TABLE repos_fts USING fts5(%s, content='repos', \
       content_rowid='id', tokenize='porter unicode61 remove_diacritics 2')"
      (fts_list "");
    Printf.sprintf "CREATE TRIGGER repos_ai AFTER INSERT ON repos BEGIN %s END"
      (fts_insert "new");
    Printf.sprintf "CREATE TRIGGER repos_ad AFTER DELETE ON repos BEGIN %s END"
      (fts_delete "old");
    Printf.sprintf "CREATE TRIGGER repos_au AFTER UPDATE ON repos BEGIN %s %s END"
      (fts_delete "old") (fts_insert "new");
    Printf.sprintf "INSERT INTO meta(k, v) VALUES ('schema_version', '%d')" version;
    "INSERT INTO meta(k, v) VALUES ('export_dirty', '0')";
  ]
