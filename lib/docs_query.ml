let ( let* ) = Result.bind
let default_limit = 5
let max_limit = 20

let counts db =
  let tbl = Hashtbl.create 256 in
  Db.query db "SELECT repo_id, count(*) FROM documents GROUP BY repo_id" [] (fun r ->
      match (Db.to_int_opt r.(0), Db.to_int_opt r.(1)) with
      | Some id, Some n -> Hashtbl.replace tbl id n
      | _ -> ())
  |> ignore;
  tbl

(* Only a documentation-eligible row's documents are ever served, even if a document of a
   row that has just become confidential is still stored. *)
let sql kind_sql =
  Printf.sprintf
    {|SELECT c.title, d.path, d.kind, d.source,
  snippet(doc_chunks_fts, 1, '[', ']', '…', 24), c.text,
  -bm25(doc_chunks_fts, 3.0, 1.0) AS score
FROM doc_chunks_fts JOIN doc_chunks c ON c.id = doc_chunks_fts.rowid
  JOIN documents d ON d.id = c.doc_id JOIN repos r ON r.id = d.repo_id
WHERE doc_chunks_fts MATCH ? AND d.repo_id = ? AND %s%s
ORDER BY score DESC, CASE d.kind WHEN 'doc' THEN 0 WHEN 'readme' THEN 1 ELSE 2 END,
  d.path, c.ord
LIMIT ?|}
    (Eligibility.docs_eligible ~alias:"r" ())
    kind_sql

type hit = {
  title : string;
  path : string;
  kind : string;
  source : string;
  snippet : string;
  text : string;
  score : float;
}

let hit r =
  let s i = Option.value (Db.to_string_opt r.(i)) ~default:"" in
  {
    title = s 0;
    path = s 1;
    kind = s 2;
    source = s 3;
    snippet = s 4;
    text = s 5;
    score = (match r.(6) with Sqlite3.Data.FLOAT f -> f | _ -> 0.);
  }

let run db ~repo_id ~fts ~kind ~limit =
  let kind_sql, kind_params =
    match kind with Some k -> (" AND d.kind = ?", [ Db.text k ]) | None -> ("", [])
  in
  Db.query db (sql kind_sql)
    ([ Db.text fts; Db.int repo_id ] @ kind_params @ [ Db.int limit ])
    hit

let uri_json repo_uri path = Jsonx.of_string_opt (repo_uri path)

let best_excerpt db ~repo_id ~repo_uri ~fts ~kind =
  match run db ~repo_id ~fts ~kind ~limit:1 with
  | h :: _ ->
      Some
        (`Assoc
           [
             ("title", `String h.title);
             ("path", `String h.path);
             ("kind", `String h.kind);
             ("snippet", `String h.snippet);
             ("uri", uri_json repo_uri h.path);
           ])
  | [] -> None

let excerpt_json ~detail ~repo_uri h =
  let uri = ("uri", uri_json repo_uri h.path) in
  match (detail : Opts.detail) with
  | Brief -> `Assoc [ ("title", `String h.title); ("path", `String h.path); uri ]
  | Standard | Full ->
      let body =
        if detail = Full then ("text", `String h.text) else ("snippet", `String h.snippet)
      in
      `Assoc
        [
          ("title", `String h.title);
          ("path", `String h.path);
          ("kind", `String h.kind);
          ("source", `String h.source);
          body;
          ("score", `Float h.score);
          uri;
        ]

(* ------------------------------------------------------------ the library *)

let resolve_hint = "call resolve-library-id to find a library id"

let resolve_library db id =
  let id = String.trim id in
  let ident =
    match Liburi.parse id with
    | Some (Liburi.Repo k) -> k
    | _ when String.length id > 1 && id.[0] = '/' -> String.sub id 1 (String.length id - 1)
    | _ -> id
  in
  match Resolve.resolve db ident with
  | Ok r -> Ok (r.id, r.key)
  | Error e when String.starts_with ~prefix:"ambiguous" e ->
      Error (e ^ "; or " ^ resolve_hint)
  | Error _ -> Error (Printf.sprintf "unknown library '%s'; %s" id resolve_hint)

type library = { host : string; owner : string; name : string; rules : string option }

let library_row db id =
  Db.query_one db "SELECT host, owner, name, rules FROM repos WHERE id = ?"
    [ Db.int id ]
    (fun r ->
      let s i = Option.value (Db.to_string_opt r.(i)) ~default:"" in
      {
        host = s 0;
        owner = s 1;
        name = s 2;
        rules =
          (match Db.to_string_opt r.(3) with
          | Some v when String.trim v <> "" -> Some v
          | _ -> None);
      })

(* The library passes the optional filters (SQL part, effective status, maintained). *)
let passes (ctx : Ctx.t) id (filters : Filters.t) =
  if filters = Filters.empty then true
  else
    let filter_sql, params = Filters.where ~alias:"r" filters in
    match
      Db.query_one ctx.db
        ("SELECT r.key, r.archived, r.status_override, r.last_commit, r.pushed_at FROM \
          repos r WHERE r.id = ?" ^ filter_sql) (Db.int id :: params) (fun r -> r)
    with
    | None -> false
    | Some r ->
        let s i = Db.to_string_opt r.(i) in
        let archived = Db.to_bool r.(1) in
        let status =
          Status.effective ~now:(ctx.now ()) ~override:(s 2) ~archived ~last_commit:(s 3)
            ~pushed_at:(s 4)
        in
        Filters.keep_status filters status
        && ((not (Filters.maintained_active filters))
           ||
           let key = Option.value (s 0) ~default:"" in
           Filters.keep_maintained filters ~archived
             ~maintained_copy:(Copies.maintained_copy (Copies.load ctx.db) key))

let search db ~repo_id ~query ~kind ~limit =
  match Query.tokens query with
  | [] -> []
  | tokens -> (
      match run db ~repo_id ~fts:(Query.and_query tokens) ~kind ~limit with
      | [] when List.length tokens >= 2 ->
          run db ~repo_id ~fts:(Query.or_query tokens) ~kind ~limit
      | hits -> hits)

let query (ctx : Ctx.t) ~library ~query ~limit ~kind ~detail ~filters =
  let* id, key = resolve_library ctx.db library in
  let* lib =
    match library_row ctx.db id with
    | Some l -> Ok l
    | None -> Error (Printf.sprintf "unknown library '%s'; %s" library resolve_hint)
  in
  let limit =
    Args.clamp ~lo:1 ~hi:max_limit (Option.value limit ~default:default_limit)
  in
  let repo_uri path = Liburi.doc ~host:lib.host ~owner:lib.owner ~name:lib.name path in
  let allowed = passes ctx id filters in
  let hits = if allowed then search ctx.db ~repo_id:id ~query ~kind ~limit else [] in
  let hint =
    if not allowed then Some "the library does not match the filters"
    else if hits <> [] then None
    else if Doc_store.count ctx.db ~repo_id:id = 0 then
      Some
        (Printf.sprintf
           "no documents are indexed for %s (documents are kept for Tier-1 libraries \
            only); use get for its README"
           key)
    else Some "no excerpt matches the query"
  in
  Ok
    (`Assoc
       ([
          ( "library",
            `Assoc
              [
                ("id", `String key);
                ("name", `String lib.name);
                ( "uri",
                  Jsonx.of_string_opt
                    (Liburi.repo ~host:lib.host ~owner:lib.owner ~name:lib.name) );
              ] );
        ]
       @ (match lib.rules with Some r -> [ ("rules", `String r) ] | None -> [])
       @ [ ("excerpts", `List (List.map (excerpt_json ~detail ~repo_uri) hits)) ]
       @ match hint with Some h -> [ ("hint", `String h) ] | None -> []))
