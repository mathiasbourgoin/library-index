let ( let* ) = Result.bind

let paths from_dir =
  ( Filename.concat from_dir Export.curated_file,
    Filename.concat from_dir Export.whitehat_file )

(* A fresh data directory holds no export yet: an absent file is an empty one. *)
let read_or path empty =
  if Sys.file_exists path then Curated_io.read_json path else Ok empty

let load from_dir =
  let* () =
    if Sys.file_exists from_dir && Sys.is_directory from_dir then Ok ()
    else Error (Printf.sprintf "the directory %s does not exist" from_dir)
  in
  let c_path, w_path = paths from_dir in
  let* cj = read_or c_path (`Assoc []) in
  let* c = Curated_io.parse_curated cj in
  let* wj = read_or w_path (`Assoc [ ("tools", `List []) ]) in
  let* w = Curated_io.parse_whitehat wj in
  Ok (c, w)

let remove_db db_path =
  List.iter
    (fun suffix ->
      let p = db_path ^ suffix in
      if Sys.file_exists p then Sys.remove p)
    [ ""; "-wal"; "-shm"; "-journal" ]

let create_and_import ~id db_path c w =
  let db = Db.create db_path in
  Fun.protect
    ~finally:(fun () -> Db.close db)
    (fun () ->
      Db.with_txn db (fun () ->
          Db.create_schema db;
          Ok (Curated_io.import ~id db c w)))

(* INV-K: the database holds curated data its exports lack (the last export failed).
   An unreadable file (foreign, corrupt) carries no such flag; a busy one is an error. *)
let rec unexported db_path =
  if not (Sys.file_exists db_path) then Ok false
  else
    match Db.create db_path with
    | exception Db.Db_error m when m = Db.busy_message -> Error (busy db_path)
    | exception Db.Db_error _ -> Ok false
    | db ->
        Fun.protect
          ~finally:(fun () -> Db.close db)
          (fun () ->
            match Db.export_dirty db with
            | d -> Ok d
            | exception Db.Db_error m when m = Db.busy_message -> Error (busy db_path)
            | exception Db.Db_error _ -> Ok false)

and busy db_path =
  Printf.sprintf "%s is busy (another library-index process holds it): retry" db_path

let refuse_discard db_path =
  Printf.sprintf
    "%s holds curated values that were never exported (the last export failed), and \
     recreating it would lose them. Make the data directory writable and run \
     'library-index export' first, or pass --discard-unexported to recreate it anyway \
     and lose those values"
    db_path

let init ~id ~db_path ~from_dir ~force ?(discard_unexported = false) () =
  if Sys.file_exists db_path && not force then
    Error
      (Printf.sprintf "database already exists at %s (use --force to recreate it)" db_path)
  else
    let* dirty = if force then unexported db_path else Ok false in
    if dirty && not discard_unexported then Error (refuse_discard db_path)
    else
      let* () = Ctx.ensure_dir (Filename.dirname db_path) in
      let* c, w = load from_dir in
      let* () = Errors.guard (fun () -> Ok (if force then remove_db db_path)) in
      let result = Errors.guard (fun () -> create_and_import ~id db_path c w) in
      (match result with
      | Error _ -> ( try remove_db db_path with Sys_error _ -> ())
      | Ok _ -> ());
      result

let import ~id db ~from_dir ?prefer_file () =
  let c_path, w_path = paths from_dir in
  Errors.guard (fun () ->
      Curated_io.import_files ~id db ~curated_path:c_path ~whitehat_path:w_path
        ?prefer_file ())
