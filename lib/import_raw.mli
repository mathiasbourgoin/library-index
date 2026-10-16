(** Offline import of the prototype cache [cache/raw.json] into an initialised DB, and the
    row-level writers a live refresh reuses (spec C-25, FR-033, FR-070, FR-071, AM-2,
    AM-3).

    Raw repo rows are upserted by key as fetched rows ([fetched = 1],
    [missing_since = NULL]); curated columns are never written here (AM-2: raw curated and
    derived fields are ignored, curated data comes from [curated.json]). Every row gets
    the stored [campaign] flag (AM-17, {!Classify.campaign_flag}); campaign descriptions
    and synopses are withheld at ingest ({!Classify.campaign_redaction}), a campaign row's
    stored README is purged (EC-15), and every text goes through {!Redact}. Derived
    columns ([role], [evidence], [tier], [computed_domain]) are recomputed from the stored
    rows and the stored global inputs. *)

type summary = { repos : int; local_checkouts : int; gaps : int }
(** Counts written by {!import}. *)

val ignored_fields : string list
(** Raw fields that are never stored: the curated
    [domain, what, interest, consumers, status_override, maintained_side], the derived
    [role, tier, evidence, status] and the per-repo search counts
    [search_commits, search_prs] (recomputed from the globals). *)

val upsert_fetched : id:Identity.t -> Db.t -> Yojson.Safe.t -> (string, string) result
(** [upsert_fetched ~id db row] writes one raw repo row (the prototype's
    [gh_norm]/[gl_norm] shape plus enrichment fields) and returns its key. Every fetched
    column is replaced (an absent field becomes NULL); [campaign] is
    {!Campaign.persisted}: the flag {!Campaign.decide} set on [row], or the stored flag
    (never cleared). [readme] and [readme_sha256] are left untouched, except that a
    campaign row's are cleared (EC-15, the only purge, AM-18), as are its pre-filled
    ([auto] provenance) [what] and [interest], and every stored gap line naming it is
    masked ({!Campaign.stored_campaign_row}); curated columns, usage and derived columns
    are left untouched. Errors name the problem, never the content. Must run inside a
    transaction. *)

val write_local_checkouts : Db.t -> Yojson.Safe.t list -> int
(** [write_local_checkouts db entries] replaces [local_checkouts] with [entries] (the
    prototype's [fetch_local] records), [ord] = list position (scan order, AM-3). Remote
    URLs are redacted (userinfo dropped). Returns the count. *)

val row_of_db : Db.t -> string -> Yojson.Safe.t option
(** [row_of_db db key] reads the stored fetched columns of [key] back into the raw-row
    shape {!upsert_fetched} accepts
    ([key, host, owner, name, full_name, description, synopsis, campaign] and every
    fetched column; JSON columns parsed, booleans as [`Bool], NULL as [`Null]), so that
    upserting it again rewrites the same values. [None] for an unknown key. *)

val replace_gaps : Db.t -> string list -> unit
(** [replace_gaps db gaps] replaces the [gaps] table, keeping the order; every line goes
    through {!Campaign.mask_gaps} (no campaign row is named, INV-C). *)

val recompute_derived : id:Identity.t -> ?key:string -> Db.t -> int
(** [recompute_derived ~id ?key db] recomputes [role], [evidence], [tier] and
    [computed_domain] with {!Classify.derive} for every fetched non-whitehat row (or only
    [key]) from the stored columns and stored search counts (AM-9), and returns the number
    of rows updated. Must run inside a transaction. *)

val import : id:Identity.t -> Db.t -> Yojson.Safe.t -> (summary, string) result
(** [import ~id db raw] imports a parsed [raw.json] ([repos], [local], [gaps] and the
    global keys, {!Globals}) in one transaction, then recomputes the derived columns. Each
    repo row's campaign flag is {!Campaign.of_raw_cache}: its own rule, the stored flag,
    and the remotes of the [local] checkouts listed in its [local_paths] (AM-17). *)

val import_file : id:Identity.t -> Db.t -> path:string -> (summary, string) result
(** [import_file ~id db ~path] reads [path] and runs {!import}. *)
