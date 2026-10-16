let seeds db name =
  let n = String.lowercase_ascii (String.trim name) in
  Db.query db
    {|SELECT key FROM repos WHERE host <> 'whitehat' AND (lower(name) = ? OR key = ?
  OR lower(full_name) = ? OR (aliases IS NOT NULL AND json_valid(aliases)
  AND EXISTS (SELECT 1 FROM json_each(repos.aliases) WHERE lower(value) = ?)))
ORDER BY key|}
    [ Db.text n; Db.text (Key.normalize n); Db.text n; Db.text n ]
    (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")

let member_json (ctx : Ctx.t) ~maintained (r : Copies.row) =
  let extra =
    Db.query_one ctx.db
      "SELECT last_commit, pushed_at, local_paths FROM repos WHERE id = ?"
      [ Db.int r.id ]
      (fun row ->
        let s i = Jsonx.of_string_opt (Db.to_string_opt row.(i)) in
        let paths =
          match Option.bind (Db.to_string_opt row.(2)) Jsonx.parse with
          | Some (`List l) -> `List l
          | _ -> `List []
        in
        [ ("last_commit", s 0); ("pushed_at", s 1); ("local_paths", paths) ])
  in
  `Assoc
    ([ ("key", `String r.key); ("host", `String r.host); ("archived", `Bool r.archived) ]
    @ Option.value extra
        ~default:
          [ ("last_commit", `Null); ("pushed_at", `Null); ("local_paths", `List []) ]
    @ [
        ("maintained", `Bool (Some r.key = maintained));
        ("uri", Jsonx.of_string_opt (Liburi.repo ~host:r.host ~owner:r.owner ~name:r.name));
      ])

let hint = "no library of that name in the index; try search or resolve-library-id"

(* The keys whose ownership is [o]: members of another ownership are left out. *)
let owned db = function
  | None -> fun _ -> true
  | Some o ->
      let keys =
        Db.query db "SELECT key FROM repos WHERE ownership = ?"
          [ Db.text o ]
          (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")
      in
      let tbl = Hashtbl.create 64 in
      List.iter (fun k -> Hashtbl.replace tbl k ()) keys;
      fun k -> Hashtbl.mem tbl k

let copies (ctx : Ctx.t) ?ownership name =
  let idx = Copies.load ctx.db in
  let keep = owned ctx.db ownership in
  let groups =
    List.fold_left
      (fun acc seed ->
        if
          List.exists
            (fun (_, members) ->
              List.exists (fun (m : Copies.row) -> m.key = seed) members)
            acc
        then acc
        else (Copies.canonical idx seed, Copies.group idx seed) :: acc)
      [] (seeds ctx.db name)
    |> List.filter_map (fun (maintained, members) ->
        match List.filter (fun (m : Copies.row) -> keep m.key) members with
        | [] -> None
        | members -> Some (maintained, members))
  in
  let groups =
    List.map
      (fun (maintained, members) ->
        let rank (m : Copies.row) = if Some m.key = maintained then 0 else 1 in
        ( maintained,
          List.sort
            (fun (a : Copies.row) b -> compare (rank a, a.key) (rank b, b.key))
            members ))
      groups
    |> List.sort compare
  in
  `Assoc
    ([
       ("name", `String (Utf8.sanitize name));
       ("count", `Int (List.fold_left (fun n (_, m) -> n + List.length m) 0 groups));
       ( "groups",
         `List
           (List.map
              (fun (maintained, members) ->
                `Assoc
                  [
                    ("maintained", Jsonx.of_string_opt maintained);
                    ("members", `List (List.map (member_json ctx ~maintained) members));
                  ])
              groups) );
     ]
    @ if groups = [] then [ ("hint", `String hint) ] else [])
