(** Refreshing one repository: decision, detail fetch and its single write transaction
    (spec US-4, FR-031, FR-034, FR-071, EC-12, EC-15, AM-9, AM-12). Used by
    [refresh --repo], by the full refresh for every listed repository, and by the MCP
    [refresh] tool. *)

(** {2 Running commands} *)

type env = {
  run : Proc.runner;  (** {!Proc.run} in production, a fake in tests. *)
  sleep : float -> unit;  (** Pause between the two GitHub searches. *)
  workers : int;  (** Detail-fetch parallelism (AM-12: 8). *)
  analyze : Proc.runner option;
      (** Runs the optional arch-index analysis ([git], [dune], arch-index: {!Analysis});
          [None] disables it (the refresh hook does nothing, [analyze-repo] and a deep
          registration record [skipped]). *)
}

val default_env : unit -> env
(** [default_env ()] uses {!Proc.run}, [Unix.sleepf], 8 workers and {!Proc.run} for the
    analysis. *)

type limiter
(** Per-run state shared by all calls of one refresh: the hosts stopped by a rate limit
    (AM-12) and an optional overall deadline. Thread-safe. *)

val limiter : ?deadline:float -> unit -> limiter
(** [limiter ?deadline ()] starts with no stopped host. [deadline] is an absolute
    [Unix.gettimeofday] time. *)

val call : env -> limiter -> Fetch.call
(** [call env lim] runs a command with a timeout of [min 30 s (deadline - now)]. After a
    [`Rate_limit] failure on [gh] (or [glab]), every later [gh] (or [glab]) command of the
    same limiter fails as [`Rate_limit] without running. Past the deadline, commands fail
    as [`Network] without running. *)

val stopped_hosts : limiter -> string list
(** [stopped_hosts lim] is the sorted list of commands ([gh], [glab]) stopped by a rate
    limit. *)

(** {2 One repository} *)

type stored = {
  row : Yojson.Safe.t option;  (** {!Import_raw.row_of_db}, [None] for a new repo. *)
  readme_fetched_at : string option;
  detail_fetched_at : string option;
}

val load_stored : Db.t -> string -> stored
(** [load_stored db key] reads what the decision needs ([None] row for an unknown key). *)

val merge_listing : stored:Yojson.Safe.t option -> Yojson.Safe.t -> Yojson.Safe.t
(** [merge_listing ~stored listed] is [listed], except that each {!Fetch.detail_fields}
    member that [listed] lacks or has as [null] takes the stored value. Listings never
    carry detail, so previously fetched detail survives a run that does not refetch it. *)

type result = {
  key : string;
  row : Yojson.Safe.t;  (** The row to upsert: merged listing + fetched detail. *)
  decision : Decision.t;
  outcome : Fetch.outcome;
  campaign : bool;  (** {!Classify.row_is_campaign} of the merged row (AM-17). *)
}

val process :
  id:Identity.t ->
  call:Fetch.call ->
  now:Ptime.t ->
  full:bool ->
  search:int * int ->
  stored:stored ->
  ?local:Local_keys.t ->
  ?readme_url:string ->
  Yojson.Safe.t ->
  result
(** [process ~id ~call ~now ~full ~search ~stored ?local ?readme_url listed] decides
    ({!Decision.decide}, with [readme_stored] true for campaign repos) and fetches. It
    does not touch the DB. The row is a campaign row (its [campaign] member set, so that
    {!write} stores it as such) when {!Classify.row_is_campaign} holds, when the stored
    row is flagged campaign (never cleared by a fetch), or when a remote of one of its
    local checkouts ([local], plus the checkouts at its [local_paths]) is under a campaign
    prefix. *)

val write :
  id:Identity.t ->
  Db.t ->
  now:Ptime.t ->
  ?recompute:bool ->
  result ->
  (unit, string) Stdlib.result
(** [write ~id db ~now ?recompute r] runs one transaction: upsert the row; store a fetched
    README with [readme_sha256] (of the stored text) and [readme_fetched_at] (the upsert
    itself purges [readme], [readme_sha256] and [synopsis] of a campaign repo, EC-15); set
    [detail_fetched_at] when detail completed; and, with [~recompute:true], recompute the
    repo's derived columns from the stored globals (AM-9) and pre-fill its empty or [auto]
    curated fields ({!Prefill}, FR-111).

    A fetch that was due but failed clears its timestamp ([readme_fetched_at] when a
    README, root-listing or manifest command failed; [detail_fetched_at] when a detail
    command failed), so the next run retries it although the listing already advanced the
    stored [pushed_at] (FR-034). The previously stored README, synopsis and
    [readme_sha256] are kept (AM-18). *)

val gap_lines : result -> string list
(** [gap_lines r] is one classified line per failed command:
    ["<key>: <purpose> failed (<class>); previous data kept"], where a campaign repo is
    named only ["a campaign repository"] (gaps are rendered). No command line, stderr or
    environment text is included (FR-072). *)

val search_for : (string * int) list * (string * int) list -> Yojson.Safe.t -> int * int
(** [search_for (commits, prs) row] is the (commit, PR) search hits of a GitHub row (by
    lowercased [full_name]), [(0, 0)] for other hosts. *)

val current_metadata :
  call:Fetch.call ->
  Yojson.Safe.t ->
  (Yojson.Safe.t * string option, Proc.failure) Stdlib.result
(** [current_metadata ~call stored_row] is the listing-equivalent row of a stored repo
    (the stored row overlaid with fresh metadata: [repos/o/r] or [projects/<id|path>]; for
    a [local:] row the checkout's last commit) and the GitLab [readme_url]. *)

(** {2 The [refresh --repo] / MCP entry point} *)

type report = {
  json : Yojson.Safe.t;
      (** [{complete, key, readme, readme_sha256, detail, synopsis, warnings?, docs?,
           gaps}]. *)
  gaps : string list;  (** The classified gap lines (empty when complete). *)
  complete : bool;  (** No command failed. *)
}
(** The outcome of one single-repository refresh, shared by the CLI and MCP front ends. *)

val refresh_one :
  Ctx.t ->
  env ->
  ?deadline:float ->
  ?progress:(float -> string -> unit) ->
  full:bool ->
  string ->
  (report, string) Stdlib.result
(** [refresh_one ctx env ?deadline ~full ident] resolves [ident] ({!Resolve.resolve}),
    fetches its current metadata ([repos/o/r] or [projects/<id|path>]; for a [local:] row,
    the checkout's last commit), then processes and writes it with the derived columns
    recomputed from the {e stored} global inputs (AM-9). A dirty export is retried first
    (AM-10); its failure is reported under [warnings] without making the refresh
    incomplete. [Error] is for usage problems (unknown or ambiguous repo, whitehat entry,
    DB error); a metadata failure is an incomplete report with nothing written. A campaign
    row's report never carries a synopsis. After the write, an eligible row's documents
    are ingested ({!Doc_refresh.run}, API files on [env.workers] threads); the report
    gains a [docs] summary and the document gap lines. [progress] is called with [1.]
    (metadata), [2.] (detail written) and [3.] (documents). *)
