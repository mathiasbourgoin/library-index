let default_limit = 20
let max_limit = 100

let parse_since s =
  match Status.parse_time (String.trim s) with
  | Some t -> Ok t
  | None ->
      Error (Printf.sprintf "invalid since '%s' (expected RFC 3339 or YYYY-MM-DD)" s)

let at_or_after ~since = function
  | Some s -> (
      match Status.parse_time s with
      | Some t when Ptime.compare t since >= 0 -> Some t
      | _ -> None)
  | None -> None

type entry = { latest : Ptime.t; key : string; json : Yojson.Safe.t }

let docs_since (ctx : Ctx.t) ~since =
  let tbl = Hashtbl.create 32 in
  Db.query ctx.db
    ("SELECT repo_id, path, fetched_at FROM documents WHERE repo_id IN (SELECT id FROM \
      repos WHERE " ^ Eligibility.docs_eligible () ^ ") ORDER BY repo_id, path")
    []
    (fun r ->
      match (Db.to_int_opt r.(0), Db.to_string_opt r.(1)) with
      | Some id, Some path -> (
          match at_or_after ~since (Db.to_string_opt r.(2)) with
          | Some t ->
              let paths, latest =
                Option.value (Hashtbl.find_opt tbl id) ~default:([], t)
              in
              Hashtbl.replace tbl id
                (path :: paths, if Ptime.compare t latest > 0 then t else latest)
          | None -> ())
      | _ -> ())
  |> ignore;
  tbl

let rec take n = function x :: rest when n > 0 -> x :: take (n - 1) rest | _ -> []

let changes_json ~str ~pushed ~commit doc =
  let metadata =
    if pushed = None && commit = None then []
    else
      [
        ( "metadata",
          `Assoc
            [
              ("pushed_at", if pushed = None then `Null else `String (str 8));
              ("last_commit", if commit = None then `Null else `String (str 9));
            ] );
      ]
  in
  let docs_change =
    match doc with
    | None -> []
    | Some (paths, _) ->
        let paths = List.sort compare paths in
        [
          ( "docs",
            `Assoc
              [
                ("count", `Int (List.length paths));
                ("paths", `List (List.map (fun p -> `String p) (take 10 paths)));
              ] );
        ]
  in
  `Assoc (metadata @ docs_change)

(* One row of the query below as an entry, when something changed since [since]. *)
let entry_of_row ~since ~docs r =
  let s i = Db.to_string_opt r.(i) in
  let str i = Option.value (s i) ~default:"" in
  let id = Option.value (Db.to_int_opt r.(0)) ~default:0 in
  let pushed = at_or_after ~since (s 8) and commit = at_or_after ~since (s 9) in
  let doc = Hashtbl.find_opt docs id in
  let times = List.filter_map Fun.id [ pushed; commit; Option.map snd doc ] in
  match List.sort (fun a b -> Ptime.compare b a) times with
  | [] -> None
  | latest :: _ ->
      let what =
        List.find_map
          (fun i ->
            match s i with Some v when String.trim v <> "" -> Some v | _ -> None)
          [ 5; 6; 7 ]
      in
      let key = str 1 in
      Some
        {
          latest;
          key;
          json =
            `Assoc
              [
                ("key", `String key);
                ("name", `String (str 4));
                ("what", Jsonx.of_string_opt what);
                ( "uri",
                  Jsonx.of_string_opt
                    (Liburi.repo ~host:(str 2) ~owner:(str 3) ~name:(str 4)) );
                ("latest", `String (Ctx.rfc3339 latest));
                ("changes", changes_json ~str ~pushed ~commit doc);
              ];
        }

let whats_new (ctx : Ctx.t) ?ownership ~since ~limit () =
  let limit =
    Args.clamp ~lo:1 ~hi:max_limit (Option.value limit ~default:default_limit)
  in
  let docs = docs_since ctx ~since in
  let entries =
    Db.query ctx.db
      ({|SELECT id, key, host, owner, name, what, synopsis, description, pushed_at,
  last_commit FROM repos WHERE host <> 'whitehat'|}
      ^ (if ownership = None then "" else " AND ownership = ?")
      ^ " ORDER BY key")
      (match ownership with Some o -> [ Db.text o ] | None -> [])
      (entry_of_row ~since ~docs)
    |> List.filter_map Fun.id
    |> List.sort (fun a b ->
        match Ptime.compare b.latest a.latest with 0 -> compare a.key b.key | c -> c)
  in
  let shown = take limit entries in
  `Assoc
    [
      ("since", `String (Ctx.rfc3339 since));
      ("total", `Int (List.length entries));
      ("count", `Int (List.length shown));
      ("repos", `List (List.map (fun e -> e.json) shown));
    ]
