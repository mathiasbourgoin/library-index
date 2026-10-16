type change = New | Changed | Unchanged

let module_path ~name q =
  let strip sep =
    let suffix = sep ^ name in
    if String.ends_with ~suffix q && String.length q > String.length suffix then
      Some (String.sub q 0 (String.length q - String.length suffix))
    else None
  in
  match strip "::" with
  | Some m -> m
  | None -> ( match strip "." with Some m -> m | None -> "")

let stored db ~repo_id path =
  Db.query_one db "SELECT sha256 FROM api_files WHERE repo_id = ? AND path = ?"
    [ Db.int repo_id; Db.text path ]
    (fun r -> Db.to_string_opt r.(0))

let blob_value = function Some b -> Db.text b | None -> Sqlite3.Data.NULL

let delete_items db ~repo_id path =
  Db.run db "DELETE FROM api_items WHERE repo_id = ? AND file = ? AND source = 'scan'"
    [ Db.int repo_id; Db.text path ]

type row = {
  module_path : string;
  name : string;
  qualified_path : string;
  kind : string;
  signature : string option;
  doc : string option;
  file : string;
  line : int option;
}

let insert db ~repo_id ~source ?analyzed_head (r : row) =
  let cap = Option.map Api_item.cap in
  Db.run db
    {|INSERT INTO api_items(repo_id, module_path, name, qualified_path, kind, signature,
  doc, file, line, source, analyzed_head) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
    [
      Db.int repo_id;
      Db.rtext r.module_path;
      Db.rtext r.name;
      Db.rtext r.qualified_path;
      Db.text r.kind;
      Db.rtext_opt (cap r.signature);
      Db.rtext_opt (cap r.doc);
      Db.text r.file;
      (match r.line with Some n -> Db.int n | None -> Sqlite3.Data.NULL);
      Db.text source;
      (match analyzed_head with Some h -> Db.text h | None -> Sqlite3.Data.NULL);
    ];
  Int64.to_int (Sqlite3.last_insert_rowid db)

let insert_item db ~repo_id ~path (it : Api_item.item) =
  ignore
    (insert db ~repo_id ~source:"scan"
       {
         module_path = module_path ~name:it.name it.qualified_path;
         name = it.name;
         qualified_path = it.qualified_path;
         kind = it.kind;
         signature = Some it.signature;
         doc = Some it.doc;
         file = path;
         line = Some it.line;
       })

let write_file db ~repo_id ~path ~text ~blob_sha =
  let text = Doc_store.prepare text in
  let sha = Sha256.hex text in
  match stored db ~repo_id path with
  | Some (Some s) when s = sha ->
      Db.run db "UPDATE api_files SET blob_sha = ? WHERE repo_id = ? AND path = ?"
        [ blob_value blob_sha; Db.int repo_id; Db.text path ];
      Unchanged
  | existing ->
      delete_items db ~repo_id path;
      List.iter (insert_item db ~repo_id ~path) (Extract.items ~path text);
      Db.run db
        {|INSERT INTO api_files(repo_id, path, sha256, blob_sha) VALUES (?, ?, ?, ?)
ON CONFLICT(repo_id, path) DO UPDATE SET sha256 = excluded.sha256,
  blob_sha = excluded.blob_sha|}
        [ Db.int repo_id; Db.text path; Db.text sha; blob_value blob_sha ];
      if existing = None then New else Changed

let delete_source db ~repo_id ~source =
  Db.run db "DELETE FROM api_items WHERE repo_id = ? AND source = ?"
    [ Db.int repo_id; Db.text source ];
  Sqlite3.changes db

let paths db ~repo_id =
  Db.query db "SELECT path FROM api_files WHERE repo_id = ? ORDER BY path"
    [ Db.int repo_id ]
    (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")

let remove_except db ~repo_id keep =
  List.fold_left
    (fun n p ->
      if List.mem p keep then n
      else (
        delete_items db ~repo_id p;
        Db.run db "DELETE FROM api_files WHERE repo_id = ? AND path = ?"
          [ Db.int repo_id; Db.text p ];
        n + 1))
    0 (paths db ~repo_id)

let blobs db ~repo_id =
  Db.query db
    "SELECT path, blob_sha FROM api_files WHERE repo_id = ? AND blob_sha IS NOT NULL"
    [ Db.int repo_id ]
    (fun r ->
      ( Option.value (Db.to_string_opt r.(0)) ~default:"",
        Option.value (Db.to_string_opt r.(1)) ~default:"" ))

let ineligible = Eligibility.ineligible_repo_ids

let purge_ineligible db =
  let n =
    Option.join
      (Db.query_one db ("SELECT count(*) FROM api_items WHERE " ^ ineligible) [] (fun r ->
           Db.to_int_opt r.(0)))
    |> Option.value ~default:0
  in
  Db.run db ("DELETE FROM api_items WHERE " ^ ineligible) [];
  Db.run db ("DELETE FROM api_files WHERE " ^ ineligible) [];
  n

let counts db ~repo_id =
  Db.query db
    "SELECT source, count(*) FROM api_items WHERE repo_id = ? GROUP BY source ORDER BY \
     source"
    [ Db.int repo_id ]
    (fun r ->
      ( Option.value (Db.to_string_opt r.(0)) ~default:"",
        Option.value (Db.to_int_opt r.(1)) ~default:0 ))
