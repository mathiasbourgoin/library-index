type usage = { consumer : string; note : string option; at : string }

type entry = {
  key : string;
  stub : Key.stub;
  strings : (string * string) list;
  consumers : string list option;
  usage : usage list;
  extra : (string * Yojson.Safe.t) list;
}

type curated = { doc : Yojson.Safe.t option; entries : entry list }

type tool = {
  campaign : string;
  path : string;
  fields : (string * string option) list;
  tool_extra : (string * Yojson.Safe.t) list;
}

type whitehat = { policy : Yojson.Safe.t option; tools : tool list }
type summary = { curated : int; whitehat : int; usage_added : int; usage_removed : int }

let string_fields =
  [ "domain"; "what"; "interest"; "status_override"; "maintained_side"; "notes"; "rules" ]

let tool_fields = [ "what"; "language"; "last_commit"; "reuse" ]
let ( let* ) = Result.bind

let read_json path =
  match Yojson.Safe.from_file path with
  | v -> Ok v
  | exception Sys_error _ -> Error (Printf.sprintf "cannot read %s" path)
  | exception Yojson.Json_error _ -> Error (Printf.sprintf "%s is not valid JSON" path)

let rec all_ok f = function
  | [] -> Ok []
  | x :: rest ->
      let* y = f x in
      let* ys = all_ok f rest in
      Ok (y :: ys)

let parse_usage key = function
  | `Assoc m as o -> (
      let str k = Jsonx.str k o in
      let unknown =
        List.filter (fun (k, _) -> not (List.mem k [ "consumer"; "note"; "at" ])) m
      in
      match (str "consumer", str "at", unknown) with
      | Some consumer, Some at, [] -> Ok { consumer; note = str "note"; at }
      | _ -> Error (Printf.sprintf "curated key '%s': malformed usage record" key))
  | _ -> Error (Printf.sprintf "curated key '%s': usage records must be objects" key)

let string_list = function
  | `List l ->
      List.fold_right
        (fun x acc ->
          match (x, acc) with `String s, Some l -> Some (s :: l) | _ -> None)
        l (Some [])
  | _ -> None

(* Classify one field: recognised and well-typed fields go to columns; anything else is
   kept verbatim in [extra]. *)
let add_field key e (name, v) =
  match (name, v) with
  | _, `Null -> Ok e
  | n, `String s when List.mem n string_fields ->
      Ok { e with strings = e.strings @ [ (n, s) ] }
  | "consumers", v when string_list v <> None -> Ok { e with consumers = string_list v }
  | "usage", `List l ->
      let* usage = all_ok (parse_usage key) l in
      Ok { e with usage }
  | n, v -> Ok { e with extra = e.extra @ [ (n, v) ] }

let parse_entry (k, v) =
  let* stub = Key.stub_of_curated_key k in
  let key = Key.normalize k in
  match v with
  | `Assoc fields ->
      let empty = { key; stub; strings = []; consumers = None; usage = []; extra = [] } in
      List.fold_left
        (fun acc f -> Result.bind acc (fun e -> add_field key e f))
        (Ok empty) fields
  | _ -> Error (Printf.sprintf "curated key '%s': entry must be an object" k)

let check_unique what keys =
  let sorted = List.sort compare keys in
  let rec dup = function
    | a :: (b :: _ as rest) -> if a = b then Some a else dup rest
    | _ -> None
  in
  match dup sorted with
  | Some k -> Error (Printf.sprintf "duplicate %s key '%s'" what k)
  | None -> Ok ()

let parse_curated = function
  | `Assoc members ->
      let doc = List.assoc_opt "_doc" members in
      let underscore, keyed =
        List.partition (fun (k, _) -> String.length k > 0 && k.[0] = '_') members
      in
      let* () =
        match List.find_opt (fun (k, _) -> k <> "_doc") underscore with
        | Some (k, _) ->
            Error (Printf.sprintf "curated.json: unsupported top-level key '%s'" k)
        | None -> Ok ()
      in
      let* entries = all_ok parse_entry keyed in
      let* () = check_unique "curated" (List.map (fun e -> e.key) entries) in
      Ok { doc; entries }
  | _ -> Error "curated.json must be a JSON object"

let parse_tool i = function
  | `Assoc m as o -> (
      let str k = Jsonx.str k o in
      let known = "campaign" :: "path" :: tool_fields in
      let typed k =
        match List.assoc_opt k m with
        | Some (`String _ | `Null) | None -> true
        | _ -> false
      in
      match (str "campaign", str "path") with
      | Some campaign, Some path when List.for_all typed tool_fields ->
          Ok
            {
              campaign;
              path;
              fields = List.map (fun f -> (f, str f)) tool_fields;
              tool_extra = List.filter (fun (k, _) -> not (List.mem k known)) m;
            }
      | _ ->
          Error
            (Printf.sprintf "whitehat_tools.json: tool #%d needs string campaign/path" i))
  | _ -> Error (Printf.sprintf "whitehat_tools.json: tool #%d must be an object" i)

let parse_whitehat = function
  | `Assoc members -> (
      let unknown = List.filter (fun (k, _) -> k <> "_policy" && k <> "tools") members in
      match (unknown, List.assoc_opt "tools" members) with
      | (k, _) :: _, _ ->
          Error (Printf.sprintf "whitehat_tools.json: unsupported top-level key '%s'" k)
      | [], Some (`List l) ->
          let* tools = all_ok Fun.id (List.mapi parse_tool l) in
          let keys =
            List.map (fun t -> Key.whitehat ~campaign:t.campaign ~path:t.path) tools
          in
          let* () = check_unique "whitehat" keys in
          Ok { policy = List.assoc_opt "_policy" members; tools }
      | [], _ -> Error "whitehat_tools.json: missing 'tools' array")
  | _ -> Error "whitehat_tools.json must be a JSON object"

(* ---------------------------------------------------------------- import *)

let json_text v = Yojson.Safe.to_string (Redact.json v)

let opt_json = function
  | None | Some [] -> Sqlite3.Data.NULL
  | Some l -> Db.text (json_text (`Assoc l))

let consumers_value = function
  | None -> Sqlite3.Data.NULL
  | Some l -> Db.text (json_text (`List (List.map (fun s -> `String s) l)))

let curated_columns =
  [
    "domain";
    "what";
    "interest";
    "status_override";
    "maintained_side";
    "notes";
    "rules";
    "consumers";
    "extra";
  ]

(* A listed entry's curated fields are replaced by the file's: curated.json is the source
   of truth. Under [--prefer-file] (INV-K, FR-129: the DB holds values the files lack) the
   upsert coalesces instead, so a field the file does not carry keeps the DB value. *)
let upsert_entry_sql ~coalesce =
  let set c =
    if coalesce then Printf.sprintf "%s = coalesce(excluded.%s, repos.%s)" c c c
    else Printf.sprintf "%s = excluded.%s" c c
  in
  Printf.sprintf
    {|INSERT INTO repos(key, host, owner, name, full_name, url, private, campaign,
  domain, what, interest, status_override, maintained_side, notes, rules, consumers, extra)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT(key) DO UPDATE SET %s|}
    (String.concat ", " (List.map set curated_columns))

let repo_id db key =
  match
    Db.query_one db "SELECT id FROM repos WHERE key = ?" [ Db.text key ] (fun r -> r.(0))
  with
  | Some (Sqlite3.Data.INT id) -> Int64.to_int id
  | _ -> raise (Db.Db_error ("database error (missing row for " ^ key ^ ")"))

(* A usage record as stored: every text redacted, so file and DB values compare equal. *)
let stored u =
  {
    consumer = Redact.text u.consumer;
    note = Option.map Redact.text u.note;
    at = Redact.text u.at;
  }

let usage_rows db repo_id =
  Db.query db "SELECT id, consumer, note, at FROM usage WHERE repo_id = ?"
    [ Db.int repo_id ]
    (fun r ->
      let text d = Option.value ~default:"" (Db.to_string_opt d) in
      (r.(0), { consumer = text r.(1); note = Db.to_string_opt r.(2); at = text r.(3) }))

(* Make the usage rows of [repo_id] equal, as a multiset, to [records] (the entry's
   [usage] list): a row matching a record is kept, every other row deleted, every
   unmatched record inserted. Returns [(added, removed)]. *)
let sync_usage db repo_id records =
  let unmatched = Hashtbl.create 8 in
  List.iter (fun (row_id, u) -> Hashtbl.add unmatched u row_id) (usage_rows db repo_id);
  let missing =
    List.filter
      (fun u ->
        if Hashtbl.mem unmatched u then (
          Hashtbl.remove unmatched u;
          false)
        else true)
      (List.map stored records)
  in
  let removed = Hashtbl.length unmatched in
  Hashtbl.iter
    (fun _ row_id -> Db.run db "DELETE FROM usage WHERE id = ?" [ row_id ])
    unmatched;
  List.iter
    (fun u ->
      let note = match u.note with None -> Sqlite3.Data.NULL | Some n -> Db.text n in
      Db.run db "INSERT INTO usage(repo_id, consumer, note, at) VALUES (?, ?, ?, ?)"
        [ Db.int repo_id; Db.text u.consumer; note; Db.text u.at ])
    missing;
  (List.length missing, removed)

let import_entry ~id ~coalesce db (e : entry) =
  let s = e.stub in
  let field f = Db.rtext_opt (List.assoc_opt f e.strings) in
  let private_ =
    match s.private_ with None -> Sqlite3.Data.NULL | Some b -> Db.int (Bool.to_int b)
  in
  Db.run db (upsert_entry_sql ~coalesce)
    ([
       Db.text e.key;
       Db.text s.host;
       Db.text s.owner;
       Db.text s.name;
       Db.text s.full_name;
       Db.rtext_opt s.url;
       private_;
       (* A stub carries nothing but its owner ({!Campaign.of_owner}); the next write
          applies the whole decision, and an existing row keeps its flag. *)
       Db.int (Bool.to_int (Campaign.of_owner ~id s.owner));
     ]
    @ List.map field string_fields
    @ [
        consumers_value (Option.map (List.map Redact.text) e.consumers);
        opt_json (Some e.extra);
      ]);
  let rid = repo_id db e.key in
  (* FR-112: an imported value is a person's: curated. *)
  let now = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 (Ptime_clock.now ()) in
  Field_meta.mark_curated db ~repo_id:rid ~now
    ~fields:(List.map fst e.strings @ if e.consumers = None then [] else [ "consumers" ]);
  sync_usage db rid e.usage

let upsert_tool_sql =
  {|INSERT INTO repos(key, host, owner, name, full_name, what, language, last_commit,
  interest, tier, ord, extra)
VALUES (?, 'whitehat', ?, ?, ?, ?, ?, ?, ?, 'whitehat', ?, ?)
ON CONFLICT(key) DO UPDATE SET owner = excluded.owner, name = excluded.name,
  full_name = excluded.full_name, what = excluded.what, language = excluded.language,
  last_commit = excluded.last_commit, interest = excluded.interest, tier = 'whitehat',
  ord = excluded.ord, extra = excluded.extra|}

let strip_slash p =
  let n = String.length p in
  if n > 1 && p.[n - 1] = '/' then String.sub p 0 (n - 1) else p

let import_tool db ord t =
  let f k = Db.rtext_opt (Option.join (List.assoc_opt k t.fields)) in
  Db.run db upsert_tool_sql
    [
      Db.text (Key.whitehat ~campaign:t.campaign ~path:t.path);
      Db.rtext t.campaign;
      Db.rtext (strip_slash t.path);
      Db.rtext t.path;
      f "what";
      f "language";
      f "last_commit";
      f "reuse";
      Db.int ord;
      opt_json (Some t.tool_extra);
    ]

let set_meta_json db k = function
  | None -> Db.run db "DELETE FROM meta WHERE k = ?" [ Db.text k ]
  | Some v -> Db.set_meta db k (json_text v)

let import ~id ?(prefer_file = false) db c w =
  set_meta_json db "_doc" c.doc;
  set_meta_json db "_policy" w.policy;
  let usage_added, usage_removed =
    List.fold_left
      (fun (added, removed) e ->
        let a, r = import_entry ~id ~coalesce:prefer_file db e in
        (added + a, removed + r))
      (0, 0) c.entries
  in
  List.iteri (import_tool db) w.tools;
  {
    curated = List.length c.entries;
    whitehat = List.length w.tools;
    usage_added;
    usage_removed;
  }

let refuse_dirty_import =
  "import refused: the last export of curated.json failed, so the database may hold \
   curated values and usage the files lack, which importing would overwrite or delete. \
   Make the data directory writable and run 'library-index export' first, or pass \
   --prefer-file to let the files win (curated fields the files do not carry are kept)"

let import_files ~id db ~curated_path ~whitehat_path ?(prefer_file = false) () =
  let* cj = read_json curated_path in
  let* c = parse_curated cj in
  let* wj = read_json whitehat_path in
  let* w = parse_whitehat wj in
  Db.with_txn db (fun () ->
      (* Checked inside the transaction, so no export can fail in between. *)
      if Db.export_dirty db && not prefer_file then Error refuse_dirty_import
      else Ok (import ~id ~prefer_file db c w))

(* ---------------------------------------------------------------- export *)

let meta_json db k = Option.bind (Db.get_meta db k) Jsonx.parse

let usage_by_repo db =
  let tbl = Hashtbl.create 16 in
  Db.query db "SELECT repo_id, consumer, note, at FROM usage ORDER BY repo_id, at, id" []
    (fun r ->
      let id = Option.value ~default:0 (Db.to_int_opt r.(0)) in
      let str i = Option.value ~default:"" (Db.to_string_opt r.(i)) in
      let note =
        match Db.to_string_opt r.(2) with Some n -> [ ("note", `String n) ] | None -> []
      in
      let u =
        `Assoc ([ ("consumer", `String (str 1)) ] @ note @ [ ("at", `String (str 3)) ])
      in
      Hashtbl.replace tbl id (u :: Option.value ~default:[] (Hashtbl.find_opt tbl id)))
  |> ignore;
  fun id -> List.rev (Option.value ~default:[] (Hashtbl.find_opt tbl id))

(* Columns: 0 id, 1 key, 2 consumers, 3 extra, then [string_fields] from index 4. An
   [auto] value (FR-111/FR-112) is regenerable and never exported. *)
let entry_json usage ~auto r =
  let id = Option.value ~default:0 (Db.to_int_opt r.(0)) in
  let str i name =
    if auto ~repo_id:id ~field:name then None
    else Option.map (fun s -> (name, `String s)) (Db.to_string_opt r.(i))
  in
  let consumers =
    Option.bind (Db.to_string_opt r.(2)) Jsonx.parse
    |> Option.map (fun v -> ("consumers", v))
  in
  let strings = List.mapi (fun i n -> str (4 + i) n) string_fields in
  (* Export order: domain, what, interest, consumers, status_override, maintained_side,
     notes, rules, usage, then extra fields. *)
  let head = List.filteri (fun i _ -> i < 3) strings in
  let tail = List.filteri (fun i _ -> i >= 3) strings in
  let u = match usage id with [] -> [] | l -> [ ("usage", `List l) ] in
  let all =
    List.filter_map Fun.id (head @ [ consumers ] @ tail)
    @ u
    @ Jsonx.stored_assoc (Db.to_string_opt r.(3))
  in
  if all = [] then None
  else Some (Option.value ~default:"" (Db.to_string_opt r.(1)), `Assoc all)

let curated_json db =
  let usage = usage_by_repo db in
  let sql =
    Printf.sprintf
      "SELECT id, key, consumers, extra, %s FROM repos WHERE host <> 'whitehat' ORDER BY \
       key"
      (String.concat ", " string_fields)
  in
  let rows = Db.query db sql [] (entry_json usage ~auto:(Field_meta.auto_fields db)) in
  let doc = match meta_json db "_doc" with Some d -> [ ("_doc", d) ] | None -> [] in
  `Assoc (doc @ List.filter_map Fun.id rows)

let tool_json r =
  let v i = match Db.to_string_opt r.(i) with Some s -> `String s | None -> `Null in
  `Assoc
    ([
       ("campaign", v 0);
       ("path", v 1);
       ("what", v 2);
       ("language", v 3);
       ("last_commit", v 4);
       ("reuse", v 5);
     ]
    @ Jsonx.stored_assoc (Db.to_string_opt r.(6)))

let whitehat_json db =
  let tools =
    Db.query db
      {|SELECT owner, full_name, what, language, last_commit, interest, extra FROM repos
WHERE host = 'whitehat' ORDER BY ord, key|}
      [] tool_json
  in
  let policy =
    match meta_json db "_policy" with Some p -> [ ("_policy", p) ] | None -> []
  in
  `Assoc (policy @ [ ("tools", `List tools) ])
