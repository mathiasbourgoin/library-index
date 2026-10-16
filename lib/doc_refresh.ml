type summary = {
  repos : int;
  local : int;
  api : int;
  not_due : int;
  written : int;
  unchanged : int;
  removed : int;
  scanned : int;
  gaps : string list;
}

let summary_json s =
  `Assoc
    [
      ("repos", `Int s.repos);
      ("local", `Int s.local);
      ("api", `Int s.api);
      ("not_due", `Int s.not_due);
      ("written", `Int s.written);
      ("unchanged", `Int s.unchanged);
      ("removed", `Int s.removed);
      ("api_files_scanned", `Int s.scanned);
      ("gaps", `List (List.map (fun g -> `String g) s.gaps));
    ]

let is_dir p = try Sys.is_directory p with Sys_error _ -> false

let local_dir = function
  | `List l -> List.find_map (function `String d when is_dir d -> Some d | _ -> None) l
  | _ -> None

let eligible = Eligibility.docs_fetchable ()

let targets db ?key () =
  let where, params =
    match key with
    | Some k -> (eligible ^ " AND key = ?", [ Db.text (Key.normalize k) ])
    | None -> (eligible, [])
  in
  Db.query db
    ("SELECT id, key, host, full_name, gl_id, default_branch, local_paths, pushed_at, \
      docs_pushed_at, language FROM repos WHERE " ^ where ^ " ORDER BY key")
    params
    (fun r ->
      let s i = Db.to_string_opt r.(i) in
      let repo_id = Option.value (Db.to_int_opt r.(0)) ~default:0 in
      {
        Doc_fetch.repo_id;
        key = Option.value (s 1) ~default:"";
        host = Option.value (s 2) ~default:"";
        full_name = Option.value (s 3) ~default:"";
        gl_id = Db.to_int_opt r.(4);
        default_branch = s 5;
        local_dir =
          local_dir (Option.value (Option.bind (s 6) Jsonx.parse) ~default:`Null);
        pushed_at = s 7;
        docs_pushed_at = s 8;
        stored_blobs = Doc_store.blobs db ~repo_id;
        stored_scan_blobs = Api_store.blobs db ~repo_id;
        language = s 9;
      })

type counts = {
  mutable local : int;
  mutable api : int;
  mutable not_due : int;
  mutable written : int;
  mutable unchanged : int;
  mutable removed : int;
  mutable scanned : int;
  mutable gaps : string list;  (** Reversed. *)
}

let stamp_docs db (r : Doc_fetch.result) =
  let set v =
    Db.run db "UPDATE repos SET docs_pushed_at = ? WHERE id = ?"
      [ v; Db.int r.target.repo_id ]
  in
  match r.mode with
  | From_local -> set Sqlite3.Data.NULL
  | From_api when r.complete -> (
      match r.target.pushed_at with Some p -> set (Db.text p) | None -> ())
  | _ -> ()

let write_one db ~now c (r : Doc_fetch.result) =
  Db.with_txn db (fun () ->
      List.iter
        (fun d ->
          match Doc_store.write db ~repo_id:r.target.repo_id ~now d with
          | Doc_store.Unchanged -> c.unchanged <- c.unchanged + 1
          | New | Changed -> c.written <- c.written + 1)
        r.docs;
      (match r.keep with
      | Some keep ->
          c.removed <-
            c.removed + Doc_store.remove_except db ~repo_id:r.target.repo_id keep
      | None -> ());
      (match r.scan with
      | Some (files, keep) ->
          List.iter
            (fun (f : Doc_fetch.scan_file) ->
              match
                Api_store.write_file db ~repo_id:r.target.repo_id ~path:f.path
                  ~text:f.text ~blob_sha:f.blob_sha
              with
              | Api_store.Unchanged -> ()
              | New | Changed -> c.scanned <- c.scanned + 1)
            files;
          ignore (Api_store.remove_except db ~repo_id:r.target.repo_id keep)
      | None -> ());
      stamp_docs db r;
      Option.iter
        (fun aliases ->
          Db.run db "UPDATE repos SET aliases = ? WHERE id = ?"
            [
              (if aliases = [] then Sqlite3.Data.NULL
               else
                 Db.rtext
                   (Yojson.Safe.to_string (`List (List.map (fun a -> `String a) aliases))));
              Db.int r.target.repo_id;
            ])
        r.aliases;
      Ok ())

let gap_of (r : Doc_fetch.result) =
  match r.failures with
  | [] -> None
  | (p, f) :: _ ->
      Some
        (Printf.sprintf "%s: docs %s failed (%s); previous documents kept" r.target.key
           (Commands.purpose_name p) (Proc.describe f))

let record db ~now c (r : Doc_fetch.result) =
  (match r.mode with
  | From_local -> c.local <- c.local + 1
  | From_api -> c.api <- c.api + 1
  | Not_due -> c.not_due <- c.not_due + 1
  | No_source -> ());
  Option.iter (fun g -> c.gaps <- g :: c.gaps) (gap_of r);
  match Errors.guard (fun () -> write_one db ~now c r) with
  | Ok () -> ()
  | Error e ->
      c.gaps <-
        Printf.sprintf "%s: docs write failed (%s); previous documents kept" r.target.key
          e
        :: c.gaps

let run db ~call ~now ~full ~workers ~file_workers ?key () =
  let now = Ctx.rfc3339 now in
  let c =
    {
      local = 0;
      api = 0;
      not_due = 0;
      written = 0;
      unchanged = 0;
      removed = 0;
      scanned = 0;
      gaps = [];
    }
  in
  let purge () =
    Db.with_txn db (fun () ->
        ignore (Api_store.purge_ineligible db);
        Ok (Doc_store.purge_ineligible db))
  in
  (match Errors.guard purge with
  | Ok n -> c.removed <- n
  | Error e -> c.gaps <- Printf.sprintf "docs purge failed (%s)" e :: c.gaps);
  let targets =
    match Errors.guard (fun () -> Ok (targets db ?key ())) with
    | Ok t -> t
    | Error e ->
        c.gaps <- Printf.sprintf "docs listing failed (%s)" e :: c.gaps;
        []
  in
  Pool.run ~workers targets (Doc_fetch.run ~call ~full ~workers:file_workers) (function
    | Ok r -> record db ~now c r
    | Error _ -> c.gaps <- "a document fetch failed (internal error)" :: c.gaps);
  {
    repos = List.length targets;
    local = c.local;
    api = c.api;
    not_due = c.not_due;
    written = c.written;
    unchanged = c.unchanged;
    removed = c.removed;
    scanned = c.scanned;
    gaps = List.rev c.gaps;
  }
