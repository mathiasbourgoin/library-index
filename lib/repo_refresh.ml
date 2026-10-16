let ( let* ) = Result.bind

type env = {
  run : Proc.runner;
  sleep : float -> unit;
  workers : int;
  analyze : Proc.runner option;
}

let default_env () =
  { run = Proc.run; sleep = Unix.sleepf; workers = 8; analyze = Some Proc.run }

(* ------------------------------------------------------------ limiter *)

type limiter = {
  lock : Mutex.t;
  stopped : (string, unit) Hashtbl.t;
  deadline : float option;
}

let limiter ?deadline () =
  { lock = Mutex.create (); stopped = Hashtbl.create 2; deadline }

let locked l f = Mutex.protect l.lock f

let stopped_hosts l =
  locked l (fun () ->
      List.sort compare (Hashtbl.fold (fun k () a -> k :: a) l.stopped []))

let call env lim (c : Commands.t) =
  let prog = if Array.length c.argv > 0 then c.argv.(0) else "" in
  (* Local read-only git is never cut by the forge deadline: the campaign rule needs
     every checkout's remotes (AM-17). *)
  let timeout =
    match lim.deadline with
    | _ when c.purpose = Commands.Local_git -> Proc.default_timeout
    | None -> Proc.default_timeout
    | Some d -> Float.min Proc.default_timeout (d -. Unix.gettimeofday ())
  in
  if locked lim (fun () -> Hashtbl.mem lim.stopped prog) then Error `Rate_limit
  else if timeout <= 0. then Error `Network
  else
    match env.run ~timeout c.argv with
    | Error `Rate_limit ->
        locked lim (fun () -> Hashtbl.replace lim.stopped prog ());
        Error `Rate_limit
    | r -> r

(* ------------------------------------------------------------ stored state *)

type stored = {
  row : Yojson.Safe.t option;
  readme_fetched_at : string option;
  detail_fetched_at : string option;
}

let load_stored db key =
  let times =
    Db.query_one db "SELECT readme_fetched_at, detail_fetched_at FROM repos WHERE key = ?"
      [ Db.text (Key.normalize key) ]
      (fun r -> (Db.to_string_opt r.(0), Db.to_string_opt r.(1)))
  in
  let readme_fetched_at, detail_fetched_at = Option.value times ~default:(None, None) in
  { row = Import_raw.row_of_db db key; readme_fetched_at; detail_fetched_at }

let merge_listing ~stored listed =
  match stored with
  | None -> listed
  | Some s ->
      List.fold_left
        (fun row k ->
          match (Jsonx.get k listed, Jsonx.get k s) with
          | `Null, `Null -> row
          | `Null, v -> Jsonx.set k v row
          | _ -> row)
        listed Fetch.detail_fields

(* ------------------------------------------------------------ processing *)

type result = {
  key : string;
  row : Yojson.Safe.t;
  decision : Decision.t;
  outcome : Fetch.outcome;
  campaign : bool;
}

let search_for (commits, prs) row =
  if Jsonx.str "host" row <> Some "github" then (0, 0)
  else
    let fn =
      String.lowercase_ascii (Option.value (Jsonx.str "full_name" row) ~default:"")
    in
    let find tbl = Option.value (List.assoc_opt fn tbl) ~default:0 in
    (find commits, find prs)

let process ~id ~call ~now ~full ~search ~(stored : stored) ?(local = Local_keys.empty)
    ?readme_url listed =
  let row = merge_listing ~stored:stored.row listed in
  (* The one campaign decision of every write path ({!Campaign.decide}, INV-D). *)
  let campaign = Campaign.decide ~id ~call ~local ?stored:stored.row row in
  let row = if campaign then Jsonx.set "campaign" (`Bool true) row else row in
  let decision =
    Decision.decide
      ~stored_pushed_at:(Option.bind stored.row (Jsonx.str "pushed_at"))
      ~listed_pushed_at:(Jsonx.str "pushed_at" listed)
      ~readme_stored:(campaign || stored.readme_fetched_at <> None)
      ~detail_age_days:(Decision.age_days ~now stored.detail_fetched_at)
      ~full
  in
  let outcome = Fetch.run ~id ~call ~row ?readme_url ~search decision in
  let row = List.fold_left (fun r (k, v) -> Jsonx.set k v r) row outcome.fields in
  { key = Option.value (Norm.key row) ~default:""; row; decision; outcome; campaign }

let content_purposes = Commands.[ Readme; Root_listing; Manifest ]

(* A fetch that was due but failed clears its timestamp, so that the next run retries it
   even though the listing already advanced the stored pushed_at (FR-034). *)
let stamps db ~stamp ~key r =
  let content_failed =
    List.exists (fun (p, _) -> List.mem p content_purposes) r.outcome.failures
  in
  let readme_stamp = if content_failed then Sqlite3.Data.NULL else stamp in
  (match r.outcome.readme with
  | Some text when not r.campaign ->
      let text = Option.map Redact.text text in
      Db.run db
        "UPDATE repos SET readme = ?, readme_sha256 = ?, readme_fetched_at = ? WHERE key \
         = ?"
        [
          Db.rtext_opt text;
          (match text with Some t -> Db.text (Sha256.hex t) | None -> Sqlite3.Data.NULL);
          readme_stamp;
          key;
        ]
  | _ when r.decision.readme && content_failed ->
      Db.run db "UPDATE repos SET readme_fetched_at = NULL WHERE key = ?" [ key ]
  | _ -> ());
  if r.outcome.detail_done then
    Db.run db "UPDATE repos SET detail_fetched_at = ? WHERE key = ?" [ stamp; key ]
  else if r.decision.detail then
    Db.run db "UPDATE repos SET detail_fetched_at = NULL WHERE key = ?" [ key ]

let write_body ~id db ~now ~recompute r =
  let* key = Import_raw.upsert_fetched ~id db r.row in
  let k = Db.text key in
  stamps db ~stamp:(Db.text (Ctx.rfc3339 now)) ~key:k r;
  (* EC-15 / FR-085: a campaign row keeps no document. *)
  if r.campaign then ignore (Doc_store.purge_key db key);
  if recompute then (
    ignore (Import_raw.recompute_derived ~id ~key db);
    (* FR-111: deterministic pre-fill of empty or auto curated fields. *)
    ignore (Prefill.apply db ~now:(Ctx.rfc3339 now) ~key ()));
  Ok ()

let write ~id db ~now ?(recompute = false) r =
  Errors.guard (fun () -> Db.with_txn db (fun () -> write_body ~id db ~now ~recompute r))

(* Stored gaps are rendered into INDEX.md and the notes page, so a campaign repo is
   never named in them (FR-051, FR-071). *)
let gap_name r = if r.campaign then "a campaign repository" else r.key

let gap_lines r =
  List.map
    (fun (p, f) ->
      Printf.sprintf "%s: %s failed (%s); previous data kept" (gap_name r)
        (Commands.purpose_name p) (Proc.describe f))
    r.outcome.failures

(* ------------------------------------------------------------ refresh --repo *)

(* A [local:] row: the checkout's last commit stands for the push (read-only git). *)
let local_metadata ~call row =
  match Jsonx.get "local_paths" row with
  | `List (`String dir :: _) -> (
      match
        call { Commands.purpose = Local_git; argv = Commands.git_last_commit dir }
      with
      | Ok out when String.trim out <> "" ->
          let t = `String (String.trim out) in
          List.fold_left
            (fun r k -> Jsonx.set k t r)
            row
            [ "pushed_at"; "updated_at"; "last_commit_local" ]
      | _ -> row)
  | _ -> row

(* The current listing-equivalent row of a stored repo (stored row overlaid with fresh
   metadata), plus the GitLab readme_url. *)
let current_metadata ~call stored_row =
  let overlay = function
    | `Assoc m -> List.fold_left (fun r (k, v) -> Jsonx.set k v r) stored_row m
    | fresh -> fresh
  in
  let full_name = Option.value (Jsonx.str "full_name" stored_row) ~default:"" in
  let fetch argv norm =
    match call { Commands.purpose = Metadata; argv } with
    | Error f -> Error f
    | Ok out -> (
        match Jsonx.parse out with
        | None -> Error `Other
        | Some j -> (
            match norm j with
            | Some r -> Ok (overlay r, Norm.gl_readme_url j)
            | None -> Error `Other))
  in
  match Jsonx.str "host" stored_row with
  | Some "github" -> fetch (Commands.gh (Commands.gh_repo_path full_name)) Norm.gh
  | Some "gitlab" ->
      let id = Jsonx.int_opt "gl_id" stored_row in
      fetch (Commands.glab (Commands.gl_project_path ~id ~full_name)) Norm.gl
  | _ -> Ok (local_metadata ~call stored_row, None)

let readme_status r =
  if r.campaign then "withheld"
  else
    match r.outcome.readme with
    | Some (Some _) -> "fetched"
    | Some None -> "absent"
    | None when List.mem_assoc Commands.Readme r.outcome.failures -> "failed"
    | None -> "unchanged"

let detail_status r =
  if r.outcome.detail_done then "fetched"
  else if r.decision.detail then "partial"
  else "unchanged"

type report = { json : Yojson.Safe.t; gaps : string list; complete : bool }

let strings l = `List (List.map (fun g -> `String g) l)

let report ~key ~gaps ~warnings extra =
  let json =
    `Assoc
      ([ ("key", `String key); ("complete", `Bool (gaps = [])) ]
      @ extra
      @ [ ("gaps", strings gaps) ]
      @ if warnings = [] then [] else [ ("warnings", strings warnings) ])
  in
  { json; gaps; complete = gaps = [] }

let result_report ~warnings r =
  let sha =
    match r.outcome.readme with Some (Some t) -> `String (Sha256.hex t) | _ -> `Null
  in
  let synopsis = if r.campaign then `Null else Jsonx.get "synopsis" r.row in
  report ~key:r.key ~gaps:(gap_lines r) ~warnings
    [
      ("readme", `String (readme_status r));
      ("readme_sha256", sha);
      ("detail", `String (detail_status r));
      ("synopsis", synopsis);
    ]

let stored_search db =
  (Globals.search_counts db "gh_search_commits", Globals.search_counts db "gh_search_prs")

let docs_step (ctx : Ctx.t) env ~call ~now ~full key =
  Doc_refresh.run ctx.db ~call ~now ~full ~workers:1 ~file_workers:env.workers ~key ()

let with_docs (rep : report) (d : Doc_refresh.summary) =
  let gaps = rep.gaps @ d.gaps in
  let json =
    match rep.json with
    | `Assoc m ->
        let m = List.filter (fun (k, _) -> k <> "complete" && k <> "gaps") m in
        `Assoc
          ([ ("complete", `Bool (gaps = [])) ]
          @ m
          @ [ ("docs", Doc_refresh.summary_json d); ("gaps", strings gaps) ])
    | j -> j
  in
  { json; gaps; complete = gaps = [] }

let refresh_one (ctx : Ctx.t) env ?deadline ?(progress = fun _ _ -> ()) ~full ident =
  Errors.guard (fun () ->
      let* r = Resolve.resolve ctx.db ident in
      let (stored : stored) = load_stored ctx.db r.key in
      let* row =
        match stored.row with
        | Some row when Jsonx.str "host" row <> Some "whitehat" -> Ok row
        | _ ->
            Error
              "whitehat entries are curated in whitehat_tools.json; nothing to refresh"
      in
      (* AM-10: every refresh first retries a dirty export. *)
      let warnings =
        match Export.retry_if_dirty ctx with Ok () -> [] | Error e -> [ e ]
      in
      let call = call env (limiter ?deadline ()) in
      progress 1. "metadata";
      match current_metadata ~call row with
      | Error f ->
          let gap =
            Printf.sprintf "%s: metadata failed (%s); nothing written" r.key
              (Proc.describe f)
          in
          Ok (report ~key:r.key ~gaps:[ gap ] ~warnings [])
      | Ok (listed, readme_url) ->
          let now = ctx.now () in
          let search = search_for (stored_search ctx.db) listed in
          let local = Local_keys.configured ctx ~call in
          let* () = Local_keys.check local in
          let res =
            process ~id:ctx.identity ~call ~now ~full ~search ~stored ~local ?readme_url
              listed
          in
          let* () = write ~id:ctx.identity ctx.db ~now ~recompute:true res in
          progress 2. "detail written";
          let docs = docs_step ctx env ~call ~now ~full r.key in
          progress 3. "documents";
          Ok (with_docs (result_report ~warnings res) docs))
