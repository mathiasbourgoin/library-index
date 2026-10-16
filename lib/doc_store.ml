type source = Local | Api

let source_name = function Local -> "local" | Api -> "api"

type doc = {
  path : string;
  kind : Doc_select.kind;
  source : source;
  text : string;
  blob_sha : string option;
}

let file_cap = 256 * 1024
let marker = "\n\n[document cut at 256 KiB by library-index]\n"

let prepare s =
  let s = Redact.text s in
  if String.length s <= file_cap then s
  else Utf8.truncate_bytes s (file_cap - String.length marker) ^ marker

type change = New | Changed | Unchanged

let delete_chunks db doc_id =
  Db.run db "DELETE FROM doc_chunks WHERE doc_id = ?" [ Db.int doc_id ]

let insert_chunks db doc_id path text =
  List.iteri
    (fun ord (c : Chunk.t) ->
      Db.run db "INSERT INTO doc_chunks(doc_id, ord, title, text) VALUES (?, ?, ?, ?)"
        [ Db.int doc_id; Db.int ord; Db.rtext c.title; Db.rtext c.text ])
    (Doc_select.chunks path text)

let blob_value = function Some b -> Db.text b | None -> Sqlite3.Data.NULL

let stored db ~repo_id path =
  Db.query_one db "SELECT id, sha256 FROM documents WHERE repo_id = ? AND path = ?"
    [ Db.int repo_id; Db.text path ]
    (fun r -> (Option.value (Db.to_int_opt r.(0)) ~default:0, Db.to_string_opt r.(1)))

let write db ~repo_id ~now d =
  let text = prepare d.text in
  let sha = Sha256.hex text in
  let kind = Db.text (Doc_select.kind_name d.kind) in
  let source = Db.text (source_name d.source) in
  match stored db ~repo_id d.path with
  | Some (id, Some s) when s = sha ->
      Db.run db "UPDATE documents SET kind = ?, source = ?, blob_sha = ? WHERE id = ?"
        [ kind; source; blob_value d.blob_sha; Db.int id ];
      Unchanged
  | existing ->
      let title = Db.rtext (Doc_select.title d.path text) in
      let id, change =
        match existing with
        | Some (id, _) ->
            delete_chunks db id;
            Db.run db
              "UPDATE documents SET kind = ?, title = ?, source = ?, fetched_at = ?, \
               sha256 = ?, blob_sha = ?, text = ? WHERE id = ?"
              [
                kind;
                title;
                source;
                Db.text now;
                Db.text sha;
                blob_value d.blob_sha;
                Db.text text;
                Db.int id;
              ];
            (id, Changed)
        | None ->
            Db.run db
              "INSERT INTO documents(repo_id, path, kind, title, source, fetched_at, \
               sha256, blob_sha, text) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
              [
                Db.int repo_id;
                Db.text d.path;
                kind;
                title;
                source;
                Db.text now;
                Db.text sha;
                blob_value d.blob_sha;
                Db.text text;
              ];
            (Int64.to_int (Sqlite3.last_insert_rowid db), New)
      in
      insert_chunks db id d.path text;
      change

let delete_where db where params =
  let ids =
    Db.query db ("SELECT id FROM documents WHERE " ^ where) params (fun r ->
        Option.value (Db.to_int_opt r.(0)) ~default:0)
  in
  List.iter
    (fun id ->
      delete_chunks db id;
      Db.run db "DELETE FROM documents WHERE id = ?" [ Db.int id ])
    ids;
  List.length ids

let paths db ~repo_id =
  Db.query db "SELECT path FROM documents WHERE repo_id = ?"
    [ Db.int repo_id ]
    (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")

let remove_except db ~repo_id keep =
  List.fold_left
    (fun n p ->
      if List.mem p keep then n
      else n + delete_where db "repo_id = ? AND path = ?" [ Db.int repo_id; Db.text p ])
    0 (paths db ~repo_id)

let purge db ~repo_id = delete_where db "repo_id = ?" [ Db.int repo_id ]

let purge_key db key =
  delete_where db "repo_id IN (SELECT id FROM repos WHERE key = ?)"
    [ Db.text (Key.normalize key) ]

let purge_ineligible db = delete_where db Eligibility.ineligible_repo_ids []

let blobs db ~repo_id =
  Db.query db
    "SELECT path, blob_sha FROM documents WHERE repo_id = ? AND blob_sha IS NOT NULL"
    [ Db.int repo_id ]
    (fun r ->
      ( Option.value (Db.to_string_opt r.(0)) ~default:"",
        Option.value (Db.to_string_opt r.(1)) ~default:"" ))

let count db ~repo_id =
  Option.join
    (Db.query_one db "SELECT count(*) FROM documents WHERE repo_id = ?"
       [ Db.int repo_id ]
       (fun r -> Db.to_int_opt r.(0)))
  |> Option.value ~default:0
