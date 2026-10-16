let keys (id : Identity.t) =
  List.map (fun (r : Identity.registration) -> r.key) id.registrations

let sync db (id : Identity.t) =
  Db.run db
    "UPDATE repos SET registered = 0, ownership_override = NULL, exclude_from_page = 0 \
     WHERE registered <> 0 OR ownership_override IS NOT NULL OR exclude_from_page <> 0"
    [];
  List.iter
    (fun (r : Identity.registration) ->
      Db.run db
        "UPDATE repos SET registered = 1, ownership_override = ?, exclude_from_page = ? \
         WHERE key = ?"
        [
          (match r.ownership with Some o -> Db.text o | None -> Sqlite3.Data.NULL);
          Db.int (Bool.to_int r.exclude_from_page);
          Db.text r.key;
        ])
    id.registrations;
  (* A registration that makes a row a campaign repository takes effect at once, even
     before (or without) a fetch. *)
  List.iter
    (fun (r : Identity.registration) -> if r.campaign then Campaign.mark db id r.key)
    id.registrations

let missing_entries db ~id ~call ~listed =
  let listed = Hashtbl.of_seq (List.to_seq (List.map (fun k -> (k, ())) listed)) in
  List.fold_left
    (fun (entries, gaps) key ->
      if Hashtbl.mem listed key then (entries, gaps)
      else
        match Import_raw.row_of_db db key with
        | None -> (entries, gaps)
        | Some stored -> (
            match Repo_refresh.current_metadata ~call stored with
            | Ok (row, readme_url) ->
                ( { Sources.row = Jsonx.set "sources" (`List []) row; readme_url }
                  :: entries,
                  gaps )
            | Error f ->
                let name = Campaign.gap_name db ~id key in
                ( entries,
                  Printf.sprintf
                    "%s: registered repository metadata failed (%s); previous data kept"
                    name (Proc.describe f)
                  :: gaps )))
    ([], []) (keys id)
  |> fun (e, g) -> (List.rev e, List.rev g)
