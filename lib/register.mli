(** Registration: one entry point per kind, upsert + immediate fetch (spec "Registration",
    "Ownership kind", FR-110..FR-112, AC-32).

    Every operation edits the data directory's [sources.toml] atomically ({!Config_edit}:
    temp file + rename, other entries and comments byte-preserved) under the refresh lock
    ({!Refresh.with_lock}: a running refresh makes it fail at once) and the data
    directory's [sources.toml] lock ({!Config_edit.with_lock}), then works on the reloaded
    configuration ({!Ctx.reload}). Fetching uses the same GET-only / read-only-git paths
    as refresh; nothing is cloned and no model is called. *)

type outcome = Reg_common.outcome = {
  json : Yojson.Safe.t;  (** The structured result. *)
  gaps : string list;  (** Classified fetch failures (empty when complete). *)
  complete : bool;  (** Every fetch succeeded: exit 0 / no [isError]. *)
}

val register_repo :
  Ctx.t ->
  Repo_refresh.env ->
  ?progress:(float -> string -> unit) ->
  ?deadline:float ->
  Yojson.Safe.t ->
  (outcome, string) result
(** [register_repo ctx env ?progress ?deadline {"target", "ownership"?, "confidential"?,
     "campaign"?, "domain"?, "notes"?, "rules"?, "deep"?}] registers one repository.

    [target] is a GitHub/GitLab URL or a local checkout ({!Target.parse}). The [[[repo]]]
    entry for its key is created, or updated with the given flags (absent flags keep their
    stored values). A new forge repository is probed first: a 404 registers nothing. The
    row is created as a stub when unknown (with the checkout's path and commit counts for
    a directory target; only a row so created is marked [created_by_registration], which
    lets {!Unregister} delete it later), the registration columns are mirrored
    ({!Registry.sync}), [domain] / [notes] / [rules] are set as curated values
    ({!Curate.annotate}, which rewrites [curated.json]), and the repository is fetched at
    once ({!Repo_refresh.refresh_one}: metadata, README, documents, deterministic
    pre-fill). With [deep], the arch-index analysis then runs without building
    ({!Analysis.analyze} with [env.analyze]), or is reported [deferred] when [deadline]
    has passed; its outcome, recorded in [analysis] whatever it is, never fails the
    registration. The result is the {!Repo_view.get} entry plus [registered] (["created"]
    | ["updated"]), [fetch] (the refresh report) and [deep] ([{requested}], plus the
    analysis result when requested). [progress] receives [1.] (registered), then [2.]–[4.]
    from the fetch. *)

val list_sources : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [list_sources ctx {}] is [{sources, repos}]: every [[[source]]] entry (kind, name,
    host and flags) and every [[[repo]]] entry (target, key, ownership, confidential,
    campaign, deep, and [indexed]: whether its row has been fetched), every string value
    passed through {!Redact.json}. Read-only. *)

val source_json : Config.source -> Yojson.Safe.t
(** [source_json s] is the JSON view of a source entry used in results. *)

val parse_kind : string -> (Config.kind, string) result
(** [parse_kind s] is the source kind spelled [s] (case-insensitive), or the uniform
    invalid-value message listing the kinds. *)
