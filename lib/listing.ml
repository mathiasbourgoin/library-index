let default_limit = 100
let max_limit = 1000

let sql filter_sql =
  Printf.sprintf
    {|SELECT r.key, r.host, r.owner, r.full_name, r.language, r.tier, r.effective_domain,
  r.private, r.what, r.synopsis, r.description, r.url, r.pushed_at, r.fetched,
  r.missing_since, r.status_override, r.archived, r.last_commit, r.name
FROM repos r WHERE 1 = 1%s ORDER BY r.key|}
    filter_sql

let str d = Jsonx.of_string_opt (Db.to_string_opt d)

let non_empty d =
  match Db.to_string_opt d with Some s when String.trim s <> "" -> Some s | _ -> None

let row ~now r =
  let status =
    Status.effective ~now
      ~override:(Db.to_string_opt r.(15))
      ~archived:(Db.to_bool r.(16))
      ~last_commit:(Db.to_string_opt r.(17))
      ~pushed_at:(Db.to_string_opt r.(12))
  in
  let what =
    match List.find_map non_empty [ r.(8); r.(9); r.(10) ] with
    | Some s -> `String s
    | None -> `Null
  in
  let private_ = match r.(7) with Sqlite3.Data.INT n -> `Bool (n <> 0L) | _ -> `Null in
  let uri =
    Liburi.repo
      ~host:(Option.value (Db.to_string_opt r.(1)) ~default:"")
      ~owner:(Option.value (Db.to_string_opt r.(2)) ~default:"")
      ~name:(Option.value (Db.to_string_opt r.(18)) ~default:"")
  in
  ( (status, Db.to_bool r.(16)),
    `Assoc
      [
        ("key", str r.(0));
        ("host", str r.(1));
        ("owner", str r.(2));
        ("full_name", str r.(3));
        ("language", str r.(4));
        ("tier", str r.(5));
        ("status", `String status);
        ("domain", str r.(6));
        ("private", private_);
        ("what", what);
        ("url", str r.(11));
        ("pushed_at", str r.(12));
        ("fetched", `Bool (Db.to_bool r.(13)));
        ("missing_since", str r.(14));
        ("uri", Jsonx.of_string_opt uri);
      ] )

let list (ctx : Ctx.t) ~filters ~limit ~offset =
  let limit =
    Args.clamp ~lo:1 ~hi:max_limit (Option.value limit ~default:default_limit)
  in
  let offset = max 0 (Option.value offset ~default:0) in
  let filter_sql, params = Filters.where ~alias:"r" filters in
  let now = ctx.now () in
  let copies =
    if Filters.maintained_active filters then Some (Copies.load ctx.db) else None
  in
  let maintained archived j =
    match (copies, Jsonx.str "key" j) with
    | Some idx, Some key ->
        Filters.keep_maintained filters ~archived
          ~maintained_copy:(Copies.maintained_copy idx key)
    | _ -> true
  in
  let rows =
    Db.query ctx.db (sql filter_sql) params (row ~now)
    |> List.filter (fun ((s, archived), j) ->
        Filters.keep_status filters s && maintained archived j)
    |> List.map snd
  in
  let page = List.filteri (fun i _ -> i >= offset && i < offset + limit) rows in
  let dirty = if Db.export_dirty ctx.db then [ ("export_dirty", `Bool true) ] else [] in
  `Assoc
    ([
       ("total", `Int (List.length rows));
       ("limit", `Int limit);
       ("offset", `Int offset);
       ("count", `Int (List.length page));
       ("repos", `List page);
     ]
    @ dirty)
