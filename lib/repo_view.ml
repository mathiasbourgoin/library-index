let bool_columns =
  [
    "private";
    "fork";
    "archived";
    "parent_archived";
    "gl_contributed";
    "remote_url_hidden";
    "fetched";
    "campaign";
    "registered";
    "exclude_from_page";
  ]

let json_columns =
  [ "sources"; "contrib"; "local_paths"; "consumers"; "extra"; "aliases" ]

let curated_fields = Curated_io.string_fields @ [ "consumers" ]

(* FR-112: each curated field with a value, and its provenance (no row: curated). *)
let provenance_json db id row =
  let meta = Field_meta.of_repo db ~repo_id:id in
  `Assoc
    (List.filter_map
       (fun f ->
         match List.assoc_opt f row with
         | Some (Sqlite3.Data.NULL | Sqlite3.Data.NONE) | None -> None
         | Some _ ->
             let p =
               match List.find_opt (fun (g, _, _) -> g = f) meta with
               | Some (_, p, _) -> p
               | None -> "curated"
             in
             Some (f, `String p))
       curated_fields)

let column_json name (v : Sqlite3.Data.t) : Yojson.Safe.t =
  match v with
  | NULL | NONE -> `Null
  | INT n when List.mem name bool_columns -> `Bool (n <> 0L)
  | INT n -> `Int (Int64.to_int n)
  | FLOAT f -> `Float f
  | TEXT s when List.mem name json_columns -> (
      match Jsonx.parse s with Some j -> j | None -> `String s)
  | TEXT s | BLOB s -> `String s

let named row k = Option.bind (List.assoc_opt k row) Db.to_string_opt

let effective_status ~now row =
  Status.effective ~now ~override:(named row "status_override")
    ~archived:(Option.fold ~none:false ~some:Db.to_bool (List.assoc_opt "archived" row))
    ~last_commit:(named row "last_commit") ~pushed_at:(named row "pushed_at")

let usage_json db id =
  Db.query db "SELECT consumer, note, at FROM usage WHERE repo_id = ? ORDER BY at, id"
    [ Db.int id ]
    (fun r ->
      `Assoc
        [
          ("consumer", column_json "" r.(0));
          ("note", column_json "" r.(1));
          ("at", column_json "" r.(2));
        ])

let documents_json db id ~doc_uri =
  Db.query db
    ("SELECT path, kind, title, source, fetched_at FROM documents WHERE repo_id = ? AND \
      repo_id IN (SELECT id FROM repos WHERE " ^ Eligibility.docs_eligible ()
   ^ ") ORDER BY CASE kind WHEN 'readme' THEN 0 WHEN 'doc' THEN 1 ELSE 2 END, path")
    [ Db.int id ]
    (fun r ->
      let path = Option.value (Db.to_string_opt r.(0)) ~default:"" in
      `Assoc
        [
          ("path", `String path);
          ("kind", column_json "" r.(1));
          ("title", column_json "" r.(2));
          ("source", column_json "" r.(3));
          ("fetched_at", column_json "" r.(4));
          ("uri", Jsonx.of_string_opt (doc_uri path));
        ])

let get (ctx : Ctx.t) ident =
  match Resolve.resolve ctx.db ident with
  | Error _ as e -> e
  | Ok { id; _ } -> (
      match
        Db.query_named ctx.db
          "SELECT *, effective_domain, ownership FROM repos WHERE id = ?"
          [ Db.int id ]
      with
      | [] -> Error (Printf.sprintf "unknown repo '%s'" ident)
      | row :: _ ->
          let row = List.sort_uniq (fun (a, _) (b, _) -> compare a b) row in
          let host = Option.value (named row "host") ~default:"" in
          let owner = Option.value (named row "owner") ~default:"" in
          let name = Option.value (named row "name") ~default:"" in
          let cols = List.map (fun (k, v) -> (k, column_json k v)) row in
          let dirty =
            if Db.export_dirty ctx.db then [ ("export_dirty", `Bool true) ] else []
          in
          Ok
            (`Assoc
               (cols
               @ [
                   ("status", `String (effective_status ~now:(ctx.now ()) row));
                   ("provenance", provenance_json ctx.db id row);
                   ("usage", `List (usage_json ctx.db id));
                   ("uri", Jsonx.of_string_opt (Liburi.repo ~host ~owner ~name));
                   ( "documents",
                     `List
                       (documents_json ctx.db id ~doc_uri:(Liburi.doc ~host ~owner ~name))
                   );
                 ]
               @ dirty)))
