(** The full refresh (spec US-4, FR-032..FR-036, C-21, EC-11, EC-14, EC-16, AM-10..AM-12).

    Sequence: take an exclusive [lockf] on [<export dir>/.refresh.lock] (a second run
    fails at once, AM-11); retry a dirty export (AM-10); run every global listing
    ({!Sources.collect}), plus the metadata of every registered repository no listing
    returned ({!Registry.missing_entries}); process each listed repository on
    {!Repo_refresh.env.workers} threads ({!Pool}), writing each one in its own short
    transaction on the calling thread; then one final transaction stores the global
    inputs, replaces the [gaps] table, recomputes the derived columns of every row,
    mirrors the registrations ({!Registry.sync}), pre-fills empty or [auto] curated fields
    ({!Prefill}), marks unseen rows [missing_since] (only when every global listing
    succeeded, FR-035; rows are never deleted; registered rows are always seen) and
    rewrites [local_checkouts]. Last, the documents of Tier-1 libraries are ingested
    ({!Doc_refresh.run}, FR-081..FR-085); their gap lines are appended to the [gaps] table
    and make the run partial. Finally, when the environment carries an analysis runner,
    the Tier-1 [own] repositories with a local checkout whose HEAD changed are analysed by
    arch-index ({!Analysis.refresh_hook}); that never makes the run partial. *)

type summary = {
  listed : int;  (** Repositories listed this run. *)
  readme_fetched : int;  (** README fetch outcomes recorded (incl. "no README"). *)
  readme_skipped : int;  (** README not due (or campaign). *)
  detail_fetched : int;  (** Detail completed. *)
  detail_skipped : int;  (** Detail not due. *)
  repos_with_failures : int;  (** Repositories with at least one failed command. *)
  missing_marked : int;  (** Rows newly marked [missing_since]. *)
  gaps : string list;  (** Lines stored in the [gaps] table. *)
  failures : string list;  (** The failure lines among [gaps] (classified only). *)
  warnings : string list;  (** Problems outside the fetch (e.g. the export retry). *)
  docs : Doc_refresh.summary;  (** Document ingestion. *)
  analysis : Analysis.summary option;
      (** The arch-index refresh hook ({!Analysis.refresh_hook}), [None] when the
          environment disables it; it never makes the run partial. *)
  complete : bool;  (** No failure and no warning: exit 0; otherwise exit 2. *)
}

val lock_file : string
(** [".refresh.lock"]. *)

val busy_message : string
(** The error returned when another refresh holds the lock. *)

val with_lock : string -> (unit -> ('a, string) result) -> ('a, string) result
(** [with_lock dir f] runs [f] under the non-blocking refresh lock of [dir]
    ([Error busy_message] at once when it is held). Registration takes it too. *)

val run :
  Ctx.t ->
  Repo_refresh.env ->
  dev_root:string option ->
  full:bool ->
  (summary, string) result
(** [run ctx env ~dev_root ~full] performs a full refresh of the sources declared in
    [ctx.identity], scanning [dev_root] for local checkouts (no scan when [None]). [Error]
    only for the lock and for problems that stop the run before any fetch (classified
    messages). *)

val summary_json : summary -> Yojson.Safe.t
(** [summary_json s] is the CLI's JSON report of [s]. *)

val summary_lines : summary -> string list
(** [summary_lines s] is the human summary printed on stderr: counts, then each failure
    and warning. Nothing secret: only classified messages. *)
