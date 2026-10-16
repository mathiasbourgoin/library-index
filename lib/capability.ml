let default_limit = 10
let max_limit = 50
let excerpt_chars = 300
let eligible = Eligibility.docs_eligible ~alias:"r" ()

(* The library columns shared by both queries, after the source-specific ones. *)
let repo_cols =
  "r.id, r.key, r.host, r.owner, r.name, r.what, r.synopsis, r.description, r.archived, \
   r.status_override, r.last_commit, r.pushed_at"

let item_sql filter_sql =
  Printf.sprintf
    {|SELECT i.kind, i.name, i.qualified_path, i.signature, i.doc, i.file, i.line, i.source,
  -bm25(api_items_fts, 4.0, 2.0, 1.0, 1.0) AS score, %s
FROM api_items_fts JOIN api_items i ON i.id = api_items_fts.rowid
  JOIN repos r ON r.id = i.repo_id
WHERE api_items_fts MATCH ? AND %s%s
ORDER BY score DESC, r.key, i.file, i.line
LIMIT ?|}
    repo_cols eligible filter_sql

let chunk_sql filter_sql =
  Printf.sprintf
    {|SELECT d.kind, c.title, c.title, NULL, c.text, d.path, NULL, 'doc',
  -bm25(doc_chunks_fts, 3.0, 1.0) AS score, %s,
  snippet(doc_chunks_fts, 1, '[', ']', '…', 24)
FROM doc_chunks_fts JOIN doc_chunks c ON c.id = doc_chunks_fts.rowid
  JOIN documents d ON d.id = c.doc_id JOIN repos r ON r.id = d.repo_id
WHERE doc_chunks_fts MATCH ? AND d.kind IN ('doc', 'api') AND %s%s
  AND NOT (d.kind = 'api' AND EXISTS (SELECT 1 FROM api_items i
    WHERE i.repo_id = d.repo_id AND i.file = d.path))
ORDER BY score DESC, r.key, d.path, c.ord
LIMIT ?|}
    repo_cols eligible filter_sql

type result = {
  kind : string;
  qualified_path : string;
  signature : string option;
  doc : string;
  excerpt : string;
  file : string;
  line : int option;
  source : string;
  score : float;
  repo_id : int;
  key : string;
  host : string;
  owner : string;
  name : string;
  what : string option;
  archived : bool;
  status : string;
}

let non_empty d =
  match Db.to_string_opt d with Some s when String.trim s <> "" -> Some s | _ -> None

let str d = Option.value (Db.to_string_opt d) ~default:""

(* An api chunk is titled "<kind> <qualified path>". *)
let split_title kind title =
  if kind <> "api" then ("doc", title)
  else
    match String.index_opt title ' ' with
    | Some i ->
        (String.sub title 0 i, String.sub title (i + 1) (String.length title - i - 1))
    | None -> ("api", title)

let row ~now ~chunk r =
  let source = str r.(7) in
  let raw_kind = str r.(0) in
  let kind, qualified_path =
    if chunk then split_title raw_kind (str r.(1))
    else (raw_kind, Option.value (non_empty r.(2)) ~default:(str r.(1)))
  in
  let archived = Db.to_bool r.(17) in
  let doc = str r.(4) in
  {
    kind;
    qualified_path;
    signature = non_empty r.(3);
    doc;
    excerpt = (if chunk then str r.(21) else Utf8.take doc excerpt_chars);
    file = str r.(5);
    line = Db.to_int_opt r.(6);
    source;
    score = (match r.(8) with Sqlite3.Data.FLOAT f -> f | _ -> 0.);
    repo_id = Option.value (Db.to_int_opt r.(9)) ~default:0;
    key = str r.(10);
    host = str r.(11);
    owner = str r.(12);
    name = str r.(13);
    what = List.find_map non_empty [ r.(14); r.(15); r.(16) ];
    archived;
    status =
      Status.effective ~now
        ~override:(Db.to_string_opt r.(18))
        ~archived
        ~last_commit:(Db.to_string_opt r.(19))
        ~pushed_at:(Db.to_string_opt r.(20));
  }

let order a b =
  match compare b.score a.score with
  | 0 ->
      compare
        (a.key, a.file, a.line, a.qualified_path, a.source)
        (b.key, b.file, b.line, b.qualified_path, b.source)
  | c -> c

let rec take n = function x :: rest when n > 0 -> x :: take (n - 1) rest | _ -> []

let run (ctx : Ctx.t) ~fts ~limit ~filters =
  let filter_sql, params = Filters.where ~alias:"r" filters in
  let now = ctx.now () in
  (* Over-fetch: the post-SQL filters drop rows. *)
  let fetch = (limit * 4) + 20 in
  let q sql ~chunk =
    Db.query ctx.db (sql filter_sql)
      ((Db.text fts :: params) @ [ Db.int fetch ])
      (row ~now ~chunk)
  in
  let copies =
    if Filters.maintained_active filters then Some (Copies.load ctx.db) else None
  in
  let keep r =
    Filters.keep_status filters r.status
    &&
    match copies with
    | None -> true
    | Some idx ->
        Filters.keep_maintained filters ~archived:r.archived
          ~maintained_copy:(Copies.maintained_copy idx r.key)
  in
  q item_sql ~chunk:false @ q chunk_sql ~chunk:true
  |> List.filter keep |> List.sort order |> take limit

let opt_str = function Some s -> `String s | None -> `Null
let opt_int = function Some n -> `Int n | None -> `Null

let doc_uris (ctx : Ctx.t) results =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun r ->
      if not (Hashtbl.mem tbl (r.repo_id, r.file)) then
        let stored =
          Db.query_one ctx.db "SELECT 1 FROM documents WHERE repo_id = ? AND path = ?"
            [ Db.int r.repo_id; Db.text r.file ]
            (fun _ -> ())
          <> None
        in
        Hashtbl.replace tbl (r.repo_id, r.file)
          (if stored then Liburi.doc ~host:r.host ~owner:r.owner ~name:r.name r.file
           else None))
    results;
  fun r -> Option.join (Hashtbl.find_opt tbl (r.repo_id, r.file))

let result_json ~detail ~doc_uri r =
  let base =
    [
      ("kind", `String r.kind);
      ("qualified_path", `String r.qualified_path);
      ("file", `String r.file);
      ("line", opt_int r.line);
      ("source", `String r.source);
    ]
  in
  let item =
    match (detail : Opts.detail) with
    | Brief -> base
    | Standard ->
        base @ [ ("signature", opt_str r.signature); ("doc_excerpt", `String r.excerpt) ]
    | Full ->
        base @ [ ("signature", opt_str r.signature); ("doc_excerpt", `String r.doc) ]
  in
  `Assoc
    [
      ( "library",
        `Assoc
          [
            ("id", `String r.key);
            ("name", `String r.name);
            ("what", opt_str r.what);
            ( "uri",
              Jsonx.of_string_opt (Liburi.repo ~host:r.host ~owner:r.owner ~name:r.name)
            );
          ] );
      ("item", `Assoc item);
      ("score", `Float r.score);
      ("uri", Jsonx.of_string_opt (doc_uri r));
    ]

let find (ctx : Ctx.t) ~query ~limit ~filters ~detail =
  let limit =
    Args.clamp ~lo:1 ~hi:max_limit (Option.value limit ~default:default_limit)
  in
  let mode, results =
    match Query.tokens query with
    | [] -> ("none", [])
    | tokens -> (
        match run ctx ~fts:(Query.and_query tokens) ~limit ~filters with
        | [] when List.length tokens >= 2 ->
            ("or", run ctx ~fts:(Query.or_query tokens) ~limit ~filters)
        | rs -> ("and", rs))
  in
  let doc_uri = doc_uris ctx results in
  `Assoc
    [
      ("query", `String (Utf8.sanitize query));
      ("mode", `String mode);
      ("count", `Int (List.length results));
      ("results", `List (List.map (result_json ~detail ~doc_uri) results));
    ]
