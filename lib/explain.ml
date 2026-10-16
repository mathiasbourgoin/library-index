let ( let* ) = Result.bind

let analysis_json db id =
  Db.query_one db
    "SELECT status, cause, head_sha, at, tool_version FROM analysis WHERE repo_id = ?"
    [ Db.int id ]
    (fun r ->
      let s i = Jsonx.of_string_opt (Db.to_string_opt r.(i)) in
      `Assoc
        [
          ("status", s 0);
          ("cause", s 1);
          ("head_sha", s 2);
          ("at", s 3);
          ("tool_version", s 4);
        ])
  |> Option.value ~default:`Null

let explain (ctx : Ctx.t) ident =
  let* { Resolve.id; key } = Resolve.resolve ctx.db ident in
  match
    Db.query_named ctx.db "SELECT *, effective_domain, ownership FROM repos WHERE id = ?"
      [ Db.int id ]
  with
  | [] -> Error (Printf.sprintf "unknown repo '%s'" ident)
  | row :: _ ->
      let named k = Repo_view.named row k in
      let str k = Jsonx.of_string_opt (named k) in
      let col k =
        Repo_view.column_json k
          (Option.value (List.assoc_opt k row) ~default:Sqlite3.Data.NULL)
      in
      let host = Option.value (named "host") ~default:"" in
      let owner = Option.value (named "owner") ~default:"" in
      let name = Option.value (named "name") ~default:"" in
      let idx = Copies.load ctx.db in
      let copies = Copies.copies idx key in
      let counts = Api_store.counts ctx.db ~repo_id:id in
      let count src = Option.value (List.assoc_opt src counts) ~default:0 in
      let what =
        match
          List.find_map
            (fun k ->
              match named k with Some v when String.trim v <> "" -> Some v | _ -> None)
            [ "what"; "synopsis"; "description" ]
        with
        | Some v -> `String v
        | None -> `Null
      in
      Ok
        (`Assoc
           [
             ("key", `String key);
             ("name", `String name);
             ("full_name", str "full_name");
             ("uri", Jsonx.of_string_opt (Liburi.repo ~host ~owner ~name));
             ("what", what);
             ("interest", str "interest");
             ("status", `String (Repo_view.effective_status ~now:(ctx.now ()) row));
             ("status_override", str "status_override");
             ("rules", str "rules");
             ("consumers", col "consumers");
             ("usage", `List (Repo_view.usage_json ctx.db id));
             ("domain", str "effective_domain");
             ("tier", str "tier");
             ("ownership", str "ownership");
             ("role", str "role");
             ("language", str "language");
             ("archived", col "archived");
             ("provenance", Repo_view.provenance_json ctx.db id row);
             ( "copies",
               `Assoc
                 [
                   ("count", `Int (List.length copies));
                   ("maintained", Jsonx.of_string_opt (Copies.canonical idx key));
                   ( "keys",
                     `List (List.map (fun (c : Copies.row) -> `String c.key) copies) );
                 ] );
             ("doc_count", `Int (Doc_store.count ctx.db ~repo_id:id));
             ( "api_item_count",
               `Assoc
                 [
                   ("scan", `Int (count "scan"));
                   ("arch-index", `Int (count "arch-index"));
                   ("total", `Int (count "scan" + count "arch-index"));
                 ] );
             ("last_commit", str "last_commit");
             ("pushed_at", str "pushed_at");
             ("analysis", analysis_json ctx.db id);
           ])
