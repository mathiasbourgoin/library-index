let ( let* ) = Result.bind
let sprintf = Printf.sprintf

type summary = {
  listed : int;
  readme_fetched : int;
  readme_skipped : int;
  detail_fetched : int;
  detail_skipped : int;
  repos_with_failures : int;
  missing_marked : int;
  gaps : string list;
  failures : string list;
  warnings : string list;
  docs : Doc_refresh.summary;
  analysis : Analysis.summary option;
  complete : bool;
}

let lock_file = ".refresh.lock"
let busy_message = "another refresh is running (" ^ lock_file ^ " is locked)"

let with_lock dir f =
  Lockfile.with_lock ~blocking:false ~busy:busy_message (Filename.concat dir lock_file) f

(* ------------------------------------------------------------ per-repo results *)

type stats = {
  mutable readme_fetched : int;
  mutable readme_skipped : int;
  mutable detail_fetched : int;
  mutable detail_skipped : int;
  mutable failed : Repo_refresh.result list;  (** Reversed. *)
  mutable errors : string list;  (** Reversed. *)
  mutable seen : string list;
}

let record ~id st db ~now (r : Repo_refresh.result) =
  st.seen <- r.key :: st.seen;
  if r.outcome.readme <> None then st.readme_fetched <- st.readme_fetched + 1
  else if (not r.decision.readme) || r.campaign then
    st.readme_skipped <- st.readme_skipped + 1;
  if r.outcome.detail_done then st.detail_fetched <- st.detail_fetched + 1
  else if not r.decision.detail then st.detail_skipped <- st.detail_skipped + 1;
  if r.outcome.failures <> [] then st.failed <- r :: st.failed;
  match Repo_refresh.write ~id db ~now r with
  | Ok () -> ()
  | Error e ->
      let name = if r.campaign then "a campaign repository" else r.key in
      st.errors <- sprintf "%s: write failed (%s); previous data kept" name e :: st.errors

let host_cmd (r : Repo_refresh.result) =
  match Jsonx.get "host" r.row with
  | `String "github" -> "gh"
  | `String "gitlab" -> "glab"
  | _ -> ""

let max_gap_lines = 50

(* Repos that failed only because their host was stopped by a rate limit are counted in
   one line per host (AM-12); every other failure gets its own classified line. *)
let detail_gaps ~stopped failed =
  let rate_only (r : Repo_refresh.result) =
    List.mem (host_cmd r) stopped
    && List.for_all (fun (_, f) -> f = `Rate_limit) r.outcome.failures
  in
  let limited, others = List.partition rate_only failed in
  let lines = List.concat_map Repo_refresh.gap_lines others in
  let shown = List.filteri (fun i _ -> i < max_gap_lines) lines in
  let more = List.length lines - List.length shown in
  shown
  @ (if more > 0 then [ sprintf "… and %d more failed detail fetches." more ] else [])
  @ List.map
      (fun cmd ->
        let n = List.length (List.filter (fun r -> host_cmd r = cmd) limited) in
        sprintf
          "%s rate limit reached: further %s calls were skipped and %d repositories keep \
           their previous data."
          (if cmd = "gh" then "GitHub" else "GitLab")
          cmd n)
      stopped

(* ------------------------------------------------------------ final transaction *)

let mark_missing db ~now ~seen =
  let seen = Hashtbl.of_seq (List.to_seq (List.map (fun k -> (k, ())) seen)) in
  let unseen =
    Db.query db "SELECT key FROM repos WHERE host <> 'whitehat' AND missing_since IS NULL"
      [] (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")
    |> List.filter (fun k -> not (Hashtbl.mem seen k))
  in
  List.iter
    (fun k ->
      Db.run db "UPDATE repos SET missing_since = ? WHERE key = ?"
        [ Db.text (Ctx.rfc3339 now); Db.text k ])
    unseen;
  List.length unseen

let finish ~id db ~call ~local ~now ~(src : Sources.t) ~gaps ~seen =
  Errors.guard (fun () ->
      Db.with_txn db (fun () ->
          List.iter (fun (k, v) -> Globals.set db k v) src.globals;
          (* Rows no listing reached are decided too: the configuration may have changed
             since they were stored (INV-C). Before the gaps, so that they are masked. *)
          ignore (Campaign.reclassify db ~id ~call ~local);
          Import_raw.replace_gaps db gaps;
          ignore (Import_raw.recompute_derived ~id db);
          Registry.sync db id;
          (* FR-111: deterministic pre-fill of empty or auto curated fields. *)
          ignore (Prefill.apply db ~now:(Ctx.rfc3339 now) ());
          let missing = if src.listings_ok then mark_missing db ~now ~seen else 0 in
          ignore (Import_raw.write_local_checkouts db src.local);
          Db.set_meta db "last_refresh_at" (Ctx.rfc3339 now);
          Ok missing))

(* Document gaps arrive after the final transaction; they join the stored gaps. *)
let append_gaps db gaps = function
  | [] -> Ok ()
  | doc_gaps ->
      Errors.guard (fun () ->
          Db.with_txn db (fun () ->
              Import_raw.replace_gaps db (gaps @ doc_gaps);
              Ok ()))

(* ------------------------------------------------------------ the run *)

let counts_of globals db k =
  match List.assoc_opt k globals with
  | Some v -> Globals.counts_of_json v
  | None -> Globals.search_counts db k

let process_all (ctx : Ctx.t) env ~call ~full ~now ~local (src : Sources.t) =
  let search =
    ( counts_of src.globals ctx.db "gh_search_commits",
      counts_of src.globals ctx.db "gh_search_prs" )
  in
  let jobs =
    List.map
      (fun (e : Sources.entry) ->
        (e, Repo_refresh.load_stored ctx.db (Option.value (Norm.key e.row) ~default:"")))
      src.entries
  in
  let st =
    {
      readme_fetched = 0;
      readme_skipped = 0;
      detail_fetched = 0;
      detail_skipped = 0;
      failed = [];
      errors = [];
      seen = [];
    }
  in
  let job ((e : Sources.entry), stored) =
    Repo_refresh.process ~id:ctx.identity ~call ~now ~full
      ~search:(Repo_refresh.search_for search e.row)
      ~stored ~local ?readme_url:e.readme_url e.row
  in
  Pool.run ~workers:env.Repo_refresh.workers jobs job (function
    | Ok r -> record ~id:ctx.identity st ctx.db ~now r
    | Error ex ->
        st.errors <-
          sprintf "a detail fetch failed (%s)" (Errors.classify ex) :: st.errors);
  (* Listed repositories are seen even when their processing failed. *)
  st.seen <-
    List.filter_map (fun (e : Sources.entry) -> Norm.key e.row) src.entries @ st.seen;
  st

let run (ctx : Ctx.t) env ~dev_root ~full =
  with_lock ctx.export_dir (fun () ->
      let warnings =
        match Export.retry_if_dirty ctx with Ok () -> [] | Error e -> [ e ]
      in
      let lim = Repo_refresh.limiter () in
      let call = Repo_refresh.call env lim in
      let now = ctx.now () in
      let local = Local_keys.configured ctx ~call in
      let* () = Local_keys.check local in
      let src = Sources.collect ~id:ctx.identity ~call ~sleep:env.sleep ~dev_root in
      (* Registered repositories no source lists are refreshed as well. *)
      let reg_entries, reg_gaps =
        Registry.missing_entries ctx.db ~id:ctx.identity ~call
          ~listed:
            (List.filter_map (fun (e : Sources.entry) -> Norm.key e.row) src.entries)
      in
      let src =
        {
          src with
          entries = src.entries @ reg_entries;
          gaps = src.gaps @ reg_gaps;
          failures = src.failures @ reg_gaps;
        }
      in
      let st = process_all ctx env ~call ~full ~now ~local src in
      (* A registered repository is never marked missing. *)
      st.seen <- Registry.keys ctx.identity @ st.seen;
      let detail =
        detail_gaps ~stopped:(Repo_refresh.stopped_hosts lim) (List.rev st.failed)
      in
      let failures = src.failures @ detail @ List.rev st.errors in
      let gaps = src.gaps @ detail @ List.rev st.errors in
      let* missing =
        finish ~id:ctx.identity ctx.db ~call ~local ~now ~src ~gaps ~seen:st.seen
      in
      let docs =
        Doc_refresh.run ctx.db ~call ~now ~full ~workers:env.workers ~file_workers:1 ()
      in
      let* () = append_gaps ctx.db gaps docs.gaps in
      let failures = failures @ docs.gaps in
      let gaps = gaps @ docs.gaps in
      (* The optional arch-index analysis never makes the run partial. *)
      let analysis =
        Option.map (fun run -> Analysis.refresh_hook ctx ~run) env.Repo_refresh.analyze
      in
      Ok
        {
          listed = List.length src.entries;
          readme_fetched = st.readme_fetched;
          readme_skipped = st.readme_skipped;
          detail_fetched = st.detail_fetched;
          detail_skipped = st.detail_skipped;
          repos_with_failures = List.length st.failed;
          missing_marked = missing;
          gaps;
          failures;
          warnings;
          docs;
          analysis;
          complete = failures = [] && warnings = [];
        })

let strings l = `List (List.map (fun s -> `String s) l)

let summary_json s =
  `Assoc
    [
      ("complete", `Bool s.complete);
      ("listed", `Int s.listed);
      ("readme_fetched", `Int s.readme_fetched);
      ("readme_skipped", `Int s.readme_skipped);
      ("detail_fetched", `Int s.detail_fetched);
      ("detail_skipped", `Int s.detail_skipped);
      ("repos_with_failures", `Int s.repos_with_failures);
      ("missing_marked", `Int s.missing_marked);
      ("gaps", strings s.gaps);
      ("failures", strings s.failures);
      ("warnings", strings s.warnings);
      ("docs", Doc_refresh.summary_json s.docs);
      ( "analysis",
        match s.analysis with Some a -> Analysis.summary_json a | None -> `Null );
    ]

let summary_lines s =
  sprintf
    "refresh: %d repositories listed; README %d fetched / %d skipped; detail %d fetched \
     / %d skipped; %d repositories with failures; %d newly missing; %d gaps; documents \
     %d written / %d unchanged / %d removed"
    s.listed s.readme_fetched s.readme_skipped s.detail_fetched s.detail_skipped
    s.repos_with_failures s.missing_marked (List.length s.gaps) s.docs.written
    s.docs.unchanged s.docs.removed
  ::
  (match s.analysis with
  | Some a when a.considered > 0 ->
      [
        sprintf
          "refresh: arch-index analysis: %d analysed of %d (ok %d, partial %d, skipped \
           %d, failed %d)"
          a.analysed a.considered a.ok a.partial a.skipped a.failed;
      ]
  | _ -> [])
  @ List.map (fun f -> "refresh: failure: " ^ f) s.failures
  @ List.map (fun w -> "refresh: warning: " ^ w) s.warnings
