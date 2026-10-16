let ( let* ) = Result.bind

type imported = { items : int; calls : int; total : int; edges : int; tool : string }

(* Statements only (no exec callbacks): rows as text options, [Error] on any failure. *)
let rows sdb sql =
  match Sqlite3.prepare sdb sql with
  | exception Sqlite3.Error _ -> Error "unreadable comment_db"
  | stmt ->
      let text = function
        | Sqlite3.Data.NULL | Sqlite3.Data.NONE -> None
        | d -> Some (Sqlite3.Data.to_string_coerce d)
      in
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> loop (Array.map text (Sqlite3.row_data stmt) :: acc)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | _ -> Error "unreadable comment_db"
      in
      let r = try loop [] with Sqlite3.Error _ -> Error "unreadable comment_db" in
      ignore (Sqlite3.finalize stmt);
      r

let columns sdb table =
  match rows sdb (Printf.sprintf "PRAGMA table_info(%s)" table) with
  | Ok rs -> List.filter_map (fun r -> if Array.length r > 1 then r.(1) else None) rs
  | Error _ -> []

let meta sdb key =
  match
    rows sdb (Printf.sprintf "SELECT value FROM comment_db_meta WHERE key = '%s'" key)
  with
  | Ok ([| Some v |] :: _) -> Some v
  | _ -> None

let module_of_file ~language file =
  let base = Filename.remove_extension (Filename.basename file) in
  if language = "ocaml" then String.capitalize_ascii base else base

let item_fields ~language ~name ~file =
  if String.contains name '.' then
    ( Api_store.module_path
        ~name:(List.nth (List.rev (String.split_on_char '.' name)) 0)
        name,
      name )
  else
    let m = module_of_file ~language file in
    (m, m ^ "." ^ name)

let exported_col cols = List.find_opt (fun c -> List.mem c cols) [ "exported"; "exposed" ]

let check_version sdb =
  match meta sdb "schema_version" with
  | Some v when v = "1" || String.starts_with ~prefix:"1." v -> Ok v
  | Some v -> Error (Printf.sprintf "unsupported comment_db schema_version %s" v)
  | None -> Error "arch-index output has no comment_db schema_version"

(* The exported functions: [id, name, file_path, line_start, signature, summary]. *)
let read_functions sdb =
  let fcols = columns sdb "functions" in
  let* () =
    if List.mem "name" fcols && List.mem "file_path" fcols then Ok ()
    else Error "arch-index output has no functions table"
  in
  let opt c = if List.mem c fcols then c else "NULL" in
  let where =
    match exported_col fcols with Some c -> " WHERE " ^ c ^ " = 1" | None -> ""
  in
  let* total =
    match rows sdb "SELECT count(*) FROM functions" with
    | Ok ([| Some n |] :: _) -> Ok (int_of_string n)
    | _ -> Error "unreadable comment_db"
  in
  let* fns =
    rows sdb
      (Printf.sprintf
         "SELECT id, name, file_path, %s, %s, %s FROM functions%s ORDER BY file_path, \
          %s, name"
         (opt "line_start") (opt "signature") (opt "summary") where (opt "line_start"))
  in
  Ok (total, fns)

(* The call edges: [caller_id, callee_id, caller_name, caller_file, callee_name,
   callee_file], keyed by ids (contract) or by names (flat schema). *)
let read_edges sdb =
  let ccols = columns sdb "calls" in
  let has = List.for_all (fun c -> List.mem c ccols) in
  if ccols = [] then Ok []
  else if has [ "caller_id"; "callee_id" ] then
    rows sdb
      "SELECT caller_id, callee_id, NULL, NULL, NULL, NULL FROM calls WHERE callee_id IS \
       NOT NULL"
  else if has [ "caller_name"; "caller_file"; "callee_name" ] then
    rows sdb
      (Printf.sprintf
         "SELECT NULL, NULL, caller_name, caller_file, callee_name, %s FROM calls"
         (if List.mem "callee_file" ccols then "callee_file" else "NULL"))
  else Ok []

let tool_name sdb ~version =
  let producer =
    Option.value ~default:"arch-index"
      (match meta sdb "built_by" with Some b -> Some b | None -> meta sdb "producer")
  in
  Printf.sprintf "%s schema %s" producer version

(* Imported ids by comment_db id, by (name, file) and by name alone. *)
type ids = {
  by_id : (string, int) Hashtbl.t;
  by_name_file : (string * string, int) Hashtbl.t;
  by_name : (string, int list) Hashtbl.t;
}

let write_items db ~repo_id ~head ~language fns =
  let ids =
    {
      by_id = Hashtbl.create 256;
      by_name_file = Hashtbl.create 256;
      by_name = Hashtbl.create 256;
    }
  in
  List.iter
    (fun r ->
      let s i = Option.value r.(i) ~default:"" in
      let name = s 1 and file = s 2 in
      let module_path, qualified_path = item_fields ~language ~name ~file in
      let id =
        Api_store.insert db ~repo_id ~source:"arch-index" ?analyzed_head:head
          {
            module_path;
            name;
            qualified_path;
            kind = "function";
            signature = r.(4);
            doc = r.(5);
            file;
            line = Option.bind r.(3) int_of_string_opt;
          }
      in
      Hashtbl.replace ids.by_id (s 0) id;
      Hashtbl.replace ids.by_name_file (name, file) id;
      Hashtbl.replace ids.by_name name
        (id :: Option.value (Hashtbl.find_opt ids.by_name name) ~default:[]))
    fns;
  ids

(* An edge end: by id, by (name, file), or by a name imported exactly once. *)
let resolve ids r =
  let find name file =
    match (name, file) with
    | Some n, Some f -> Hashtbl.find_opt ids.by_name_file (n, f)
    | Some n, None -> (
        match Hashtbl.find_opt ids.by_name n with Some [ id ] -> Some id | _ -> None)
    | None, _ -> None
  in
  match (r.(0), r.(1)) with
  | Some a, Some b -> (Hashtbl.find_opt ids.by_id a, Hashtbl.find_opt ids.by_id b)
  | _ -> (find r.(2) r.(3), find r.(4) r.(5))

let write_edges db ids edges =
  List.fold_left
    (fun n r ->
      match resolve ids r with
      | Some a, Some b ->
          Db.run db "INSERT OR IGNORE INTO api_calls(caller_id, callee_id) VALUES (?, ?)"
            [ Db.int a; Db.int b ];
          if Sqlite3.changes db = 1 then n + 1 else n
      | _ -> n)
    0 edges

let replace (ctx : Ctx.t) ~repo_id ~head ~language ~fns ~edges =
  Db.with_txn ctx.db (fun () ->
      ignore (Api_store.delete_source ctx.db ~repo_id ~source:"arch-index");
      let ids = write_items ctx.db ~repo_id ~head ~language fns in
      Ok (write_edges ctx.db ids edges))

let read_and_write (ctx : Ctx.t) ~repo_id ~head ~language sdb =
  let* version = check_version sdb in
  let* total, fns = read_functions sdb in
  let* edges = read_edges sdb in
  let tool = tool_name sdb ~version in
  (* Nothing indexed: the previous arch-index items stay. *)
  if total = 0 then Ok { items = 0; calls = 0; total; edges = 0; tool }
  else
    let* calls = replace ctx ~repo_id ~head ~language ~fns ~edges in
    Ok { items = List.length fns; calls; total; edges = List.length edges; tool }

let import (ctx : Ctx.t) ~repo_id ~head ~language path =
  match Sqlite3.db_open ~mode:`READONLY path with
  | exception Sqlite3.Error _ -> Error "arch-index output unreadable"
  | sdb ->
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close sdb))
        (fun () -> read_and_write ctx ~repo_id ~head ~language sdb)
