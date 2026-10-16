let weights = [ 10.; 8.; 10.; 6.; 4.; 4.; 3.; 2.; 2.; 2.; 1. ]
let default_limit = 10
let max_limit = 50

let sql filter_sql =
  Printf.sprintf
    {|SELECT r.key, r.full_name, r.host, r.private, r.tier, r.what, r.synopsis, r.description,
  r.url, r.status_override, r.archived, r.last_commit, r.pushed_at,
  -bm25(repos_fts, %s) AS score,
  snippet(repos_fts, -1, '[', ']', '…', 12),
  r.id, r.owner, r.name, r.language, r.effective_domain
FROM repos_fts JOIN repos r ON r.id = repos_fts.rowid
WHERE repos_fts MATCH ?%s
ORDER BY score DESC, CASE WHEN r.tier = '1' THEN 0 ELSE 1 END, r.pushed_at DESC, r.key|}
    (String.concat ", " (List.map (Printf.sprintf "%.1f") weights))
    filter_sql

let non_empty d =
  match Db.to_string_opt d with Some s when String.trim s <> "" -> Some s | _ -> None

let opt_string d = Jsonx.of_string_opt (Db.to_string_opt d)

type hit = {
  id : int;
  key : string;
  archived : bool;
  status : string;
  repo_uri : string option;
  doc_uri : string -> string option;
  row : Sqlite3.Data.t array;
}

let hit ~now r =
  let str i = Option.value (Db.to_string_opt r.(i)) ~default:"" in
  let archived = Db.to_bool r.(10) in
  let status =
    Status.effective ~now
      ~override:(Db.to_string_opt r.(9))
      ~archived
      ~last_commit:(Db.to_string_opt r.(11))
      ~pushed_at:(Db.to_string_opt r.(12))
  in
  let host = str 2 and owner = str 16 and name = str 17 in
  {
    id = Option.value (Db.to_int_opt r.(15)) ~default:0;
    key = str 0;
    archived;
    status;
    repo_uri = Liburi.repo ~host ~owner ~name;
    doc_uri = Liburi.doc ~host ~owner ~name;
    row = r;
  }

let score h = match h.row.(13) with Sqlite3.Data.FLOAT f -> f | _ -> 0.

let what h =
  match List.find_map non_empty [ h.row.(5); h.row.(6); h.row.(7) ] with
  | Some s -> `String s
  | None -> `Null

(* The core hit fields (US-2.1), in their original order, then the URI. *)
let standard h =
  let r = h.row in
  let private_ = match r.(3) with Sqlite3.Data.INT n -> `Bool (n <> 0L) | _ -> `Null in
  [
    ("key", opt_string r.(0));
    ("full_name", opt_string r.(1));
    ("host", opt_string r.(2));
    ("private", private_);
    ("tier", opt_string r.(4));
    ("status", `String h.status);
    ("what", what h);
    ("url", opt_string r.(8));
    ("score", `Float (score h));
    ("snippet", opt_string r.(14));
    ("uri", Jsonx.of_string_opt h.repo_uri);
  ]

let shape (ctx : Ctx.t) ~detail ~fts ~kind ~counts h =
  let excerpt () =
    match Docs_query.best_excerpt ctx.db ~repo_id:h.id ~repo_uri:h.doc_uri ~fts ~kind with
    | Some e -> [ ("excerpt", e) ]
    | None -> []
  in
  match (detail : Opts.detail) with
  | Brief ->
      `Assoc
        [
          ("key", `String h.key);
          ("full_name", opt_string h.row.(1));
          ("what", what h);
          ("score", `Float (score h));
          ("uri", Jsonx.of_string_opt h.repo_uri);
        ]
  | Standard -> `Assoc (standard h @ excerpt ())
  | Full ->
      `Assoc
        (standard h
        @ [
            ("language", opt_string h.row.(18));
            ("domain", opt_string h.row.(19));
            ("pushed_at", opt_string h.row.(12));
            ("last_commit", opt_string h.row.(11));
            ("doc_count", `Int (Option.value (Hashtbl.find_opt counts h.id) ~default:0));
          ]
        @ excerpt ())

let rec take n = function x :: rest when n > 0 -> x :: take (n - 1) rest | _ -> []

let run (ctx : Ctx.t) ~fts ~limit ~filters =
  let filter_sql, params = Filters.where ~alias:"r" filters in
  let now = ctx.now () in
  let copies =
    if Filters.maintained_active filters then Some (Copies.load ctx.db) else None
  in
  let maintained h =
    match copies with
    | None -> true
    | Some idx ->
        Filters.keep_maintained filters ~archived:h.archived
          ~maintained_copy:(Copies.maintained_copy idx h.key)
  in
  Db.query ctx.db (sql filter_sql) (Db.text fts :: params) (hit ~now)
  |> List.filter (fun h -> Filters.keep_status filters h.status && maintained h)
  |> take limit

let search ?(detail = Opts.Standard) ?kind ?budget ctx ~query ~limit ~filters =
  let limit =
    Args.clamp ~lo:1 ~hi:max_limit (Option.value limit ~default:default_limit)
  in
  let tokens = Query.tokens query in
  let mode, fts, hits =
    match tokens with
    | [] -> ("none", "", [])
    | _ -> (
        let a = Query.and_query tokens in
        match run ctx ~fts:a ~limit ~filters with
        | [] when List.length tokens >= 2 ->
            let o = Query.or_query tokens in
            ("or", o, run ctx ~fts:o ~limit ~filters)
        | hits -> ("and", a, hits))
  in
  let counts = if detail = Full then Docs_query.counts ctx.db else Hashtbl.create 1 in
  let hits = List.map (shape ctx ~detail ~fts ~kind ~counts) hits in
  Opts.apply_budget budget ~list_key:"hits"
    (`Assoc
       [
         ("query", `String (Utf8.sanitize query));
         ("mode", `String mode);
         ("count", `Int (List.length hits));
         ("hits", `List hits);
       ])
