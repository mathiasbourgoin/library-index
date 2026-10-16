exception Db_error of string

type t = Sqlite3.db

let schema_version = Schema_sql.version
let busy_message = "database busy (retry later)"
let fail rc = raise (Db_error ("database error (" ^ Sqlite3.Rc.to_string rc ^ ")"))

let check = function
  | Sqlite3.Rc.OK | Sqlite3.Rc.DONE | Sqlite3.Rc.ROW -> ()
  | Sqlite3.Rc.BUSY | Sqlite3.Rc.LOCKED -> raise (Db_error busy_message)
  | rc -> fail rc

let exec db sql = check (Sqlite3.exec db sql)

let prepare db sql =
  try Sqlite3.prepare db sql with Sqlite3.Error _ -> fail (Sqlite3.errcode db)

let with_stmt db sql params f =
  let stmt = prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      check (Sqlite3.bind_values stmt params);
      f stmt)

let run db sql params = with_stmt db sql params (fun stmt -> check (Sqlite3.step stmt))

let query db sql params f =
  with_stmt db sql params (fun stmt ->
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> loop (f (Sqlite3.row_data stmt) :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            check rc;
            List.rev acc
      in
      loop [])

let query_named db sql params =
  with_stmt db sql params (fun stmt ->
      let n = Sqlite3.column_count stmt in
      let names = Array.init n (Sqlite3.column_name stmt) in
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let row = Sqlite3.row_data stmt in
            loop (List.init n (fun i -> (names.(i), row.(i))) :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            check rc;
            List.rev acc
      in
      loop [])

let query_one db sql params f =
  match query db sql params f with [] -> None | x :: _ -> Some x

let create path =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    raise
      (Db_error
         (Printf.sprintf
            "the directory %s does not exist (run 'library-index init', which creates it)"
            dir));
  let db = Sqlite3.db_open path in
  (* A configuration failure (a read-only directory, a held lock) closes the handle and
     surfaces as a classified [Db_error]. *)
  match
    Sqlite3.busy_timeout db 5000;
    exec db "PRAGMA journal_mode=WAL";
    exec db "PRAGMA foreign_keys=ON"
  with
  | () -> db
  | exception e ->
      ignore (Sqlite3.db_close db);
      raise e

let close db = ignore (Sqlite3.db_close db)

let with_txn db f =
  exec db "BEGIN IMMEDIATE";
  match f () with
  | Ok _ as ok -> (
      (* A failed COMMIT (busy, I/O) must not leave the transaction open on a long-lived
         connection: roll back, then report. *)
      match exec db "COMMIT" with
      | () -> ok
      | exception e ->
          ignore (Sqlite3.exec db "ROLLBACK");
          raise e)
  | Error _ as err ->
      exec db "ROLLBACK";
      err
  | exception e ->
      ignore (Sqlite3.exec db "ROLLBACK");
      raise e

let text s = Sqlite3.Data.TEXT (Utf8.sanitize s)
let rtext s = Sqlite3.Data.TEXT (Redact.text s)
let rtext_opt = function None -> Sqlite3.Data.NULL | Some s -> rtext s
let int n = Sqlite3.Data.INT (Int64.of_int n)

let to_string_opt = function
  | Sqlite3.Data.NULL | Sqlite3.Data.NONE -> None
  | d -> Some (Sqlite3.Data.to_string_coerce d)

let to_int_opt d = Sqlite3.Data.to_int d
let to_bool d = match to_int_opt d with Some n -> n <> 0 | None -> false

let get_meta db k =
  Option.join
    (query_one db "SELECT v FROM meta WHERE k = ?"
       [ text k ]
       (fun r -> to_string_opt r.(0)))

let set_meta db k v =
  run db
    "INSERT INTO meta(k, v) VALUES (?, ?) ON CONFLICT(k) DO UPDATE SET v = excluded.v"
    [ text k; text v ]

let export_dirty db = get_meta db "export_dirty" = Some "1"

(* [None] when the file has no [meta] table (not a library-index database); a busy or
   locked file raises [Db_error busy_message], so contention is never mistaken for a
   foreign file. *)
let stored_version db =
  match
    query_one db "SELECT v FROM meta WHERE k = 'schema_version'" [] (fun r ->
        to_string_opt r.(0))
  with
  | v -> Option.join v
  | exception Db_error m when m = busy_message -> raise (Db_error m)
  | exception Db_error _ -> None

(* Version 3 -> 4 adds [repos.created_by_registration]; a row that existed before the
   migration counts as not created by a registration, so unregistering never deletes it.
   Serialised (FR-130): [BEGIN IMMEDIATE] takes the write lock first, then the version is
   read again inside the transaction, so a process that lost the race to another migrator
   finds version 4 and does nothing. *)
let migrate_from_3 db =
  with_txn db (fun () ->
      match stored_version db with
      | Some v when v = string_of_int schema_version -> Ok ()
      | Some "3" ->
          exec db
            "ALTER TABLE repos ADD COLUMN created_by_registration INTEGER NOT NULL \
             DEFAULT 0";
          set_meta db "schema_version" (string_of_int schema_version);
          Ok ()
      | v -> Error v)

(* Never advise [init --force] (which deletes the file) while it holds curated data not
   yet exported (INV-K): read the flag when the file has a [meta] table at all. *)
let unexported db =
  match get_meta db "export_dirty" with
  | Some "1" -> true
  | _ -> false
  | exception Db_error _ -> false

let foreign_message path db v =
  let found =
    match v with Some v -> "schema version " ^ v | None -> "no schema version"
  in
  if unexported db then
    Printf.sprintf
      "%s has %s and this binary needs %d; it also holds curated data that was never \
       exported, so do not recreate it: keep the file and export it with a library-index \
       build of its own schema version"
      path found schema_version
  else
    Printf.sprintf
      "%s is not a library-index database of schema version %d (%s); run 'library-index \
       init --force'"
      path schema_version found

let check_version path db =
  match stored_version db with
  | Some v when v = string_of_int schema_version -> Ok ()
  | Some "3" -> (
      match migrate_from_3 db with
      | Ok () -> Ok ()
      | Error v -> Error (foreign_message path db v))
  | v -> Error (foreign_message path db v)

let open_existing path =
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "no database at %s; run 'library-index init' first" path)
  else
    match create path with
    | exception Db_error m -> Error (Printf.sprintf "cannot open %s: %s" path m)
    | db -> (
        let r =
          try check_version path db with
          | Db_error m when m = busy_message ->
              Error
                (Printf.sprintf
                   "%s is busy (another library-index process holds it, for instance \
                    while upgrading its schema): retry"
                   path)
          | Db_error m -> Error (Printf.sprintf "opening %s failed (%s): retry" path m)
        in
        match r with
        | Ok () -> Ok db
        | Error _ as e ->
            close db;
            e)

let create_schema db = List.iter (exec db) Schema_sql.statements
