type provenance = Auto | Curated

let provenance_name = function Auto -> "auto" | Curated -> "curated"
let of_name = function "auto" -> Some Auto | "curated" -> Some Curated | _ -> None

let set db ~repo_id ~field p ~now =
  Db.run db
    "INSERT INTO field_meta(repo_id, field, provenance, updated_at) VALUES (?, ?, ?, ?) \
     ON CONFLICT(repo_id, field) DO UPDATE SET provenance = excluded.provenance, \
     updated_at = excluded.updated_at"
    [ Db.int repo_id; Db.text field; Db.text (provenance_name p); Db.text now ]

let mark_curated db ~repo_id ~fields ~now =
  List.iter (fun field -> set db ~repo_id ~field Curated ~now) fields

let get db ~repo_id ~field =
  Option.bind
    (Db.query_one db "SELECT provenance FROM field_meta WHERE repo_id = ? AND field = ?"
       [ Db.int repo_id; Db.text field ]
       (fun r -> Option.bind (Db.to_string_opt r.(0)) of_name))
    Fun.id

let of_repo db ~repo_id =
  Db.query db
    "SELECT field, provenance, updated_at FROM field_meta WHERE repo_id = ? ORDER BY \
     field"
    [ Db.int repo_id ]
    (fun r ->
      let s i = Option.value (Db.to_string_opt r.(i)) ~default:"" in
      (s 0, s 1, s 2))

let auto_fields db =
  let tbl = Hashtbl.create 64 in
  Db.query db "SELECT repo_id, field FROM field_meta WHERE provenance = 'auto'" []
    (fun r ->
      match (Db.to_int_opt r.(0), Db.to_string_opt r.(1)) with
      | Some id, Some f -> Hashtbl.replace tbl (id, f) ()
      | _ -> ())
  |> ignore;
  fun ~repo_id ~field -> Hashtbl.mem tbl (repo_id, field)
