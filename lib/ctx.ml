type t = {
  db : Db.t;
  db_path : string;
  data_dir : string;
  export_dir : string;
  config : Config.t;
  identity : Identity.t;
  now : unit -> Ptime.t;
}

let env_non_empty k =
  match Sys.getenv_opt k with Some v when String.trim v <> "" -> Some v | _ -> None

let home () =
  match env_non_empty "HOME" with Some h -> Ok h | None -> Error "HOME is not set"

let db_file = "library-index.db"

let rec ensure_dir dir =
  if Sys.file_exists dir then
    if Sys.is_directory dir then Ok ()
    else Error (Printf.sprintf "%s exists and is not a directory" dir)
  else
    let parent = Filename.dirname dir in
    let ( let* ) = Result.bind in
    let* () = if parent = dir then Ok () else ensure_dir parent in
    match Unix.mkdir dir 0o700 with
    | () | (exception Unix.Unix_error (Unix.EEXIST, _, _)) -> Ok ()
    | exception Unix.Unix_error (e, _, _) ->
        Error
          (Printf.sprintf "cannot create the directory %s (%s)" dir (Unix.error_message e))

let xdg_data_dir () =
  match env_non_empty "XDG_DATA_HOME" with
  | Some d -> Ok (Filename.concat d "library-index")
  | None -> (
      match home () with
      | Ok h -> Ok (Filename.concat h ".local/share/library-index")
      | Error _ ->
          Error
            "cannot locate the data directory: set --data-dir, LIBRARY_INDEX_DATA, \
             XDG_DATA_HOME or HOME")

let resolve_data_dir ~data_dir ~db =
  match data_dir with
  | Some d -> Ok d
  | None -> (
      match env_non_empty "LIBRARY_INDEX_DATA" with
      | Some d -> Ok d
      | None -> (
          match (db, env_non_empty "LIBRARY_INDEX_DB") with
          | Some p, _ | None, Some p -> Ok (Filename.dirname p)
          | None, None -> xdg_data_dir ()))

let resolve_db_path ~db ~data_dir =
  match db with
  | Some p -> p
  | None -> (
      match env_non_empty "LIBRARY_INDEX_DB" with
      | Some p -> p
      | None -> Filename.concat data_dir db_file)

let load_config data_dir =
  Result.map (fun c -> (c, Identity.of_config c)) (Config.load_dir data_dir)

let system_clock () = Ptime_clock.now ()
let rfc3339 t = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 t

let reload t =
  if not (Sys.file_exists (Filename.concat t.data_dir Config.file_name)) then Ok t
  else
    Result.map
      (fun (config, identity) -> { t with config; identity })
      (load_config t.data_dir)
