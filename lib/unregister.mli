(** Unregistration (spec "Ownership kind": [unregister-repo] / [unregister-source],
    AC-33). Both run under the refresh lock and the data directory's [sources.toml] lock
    ({!Config_edit.with_lock}), and rewrite the curated exports afterwards.

    The database change and the [sources.toml] edit are one unit: the DB change runs in a
    transaction whose last step writes the edited file atomically, so a failed write rolls
    the DB back, and a failed commit after the write restores the previous file.

    A row is deleted only when the removed entry is all that keeps it and nobody curated
    it: for a repository registration, the row was created by a registration
    ([created_by_registration]), no source lists it, and it carries no curated data (no
    usage row, no [curated] provenance, no curated column value that a pre-fill did not
    produce, no extra [curated.json] member); for a source, no other label or registration
    keeps the row and it carries no curated data either. A deleted row goes with
    everything that hangs on it: documents and chunks (and their FTS entries), usage,
    field provenance and analysis records; no tombstone is kept and [curated.json] no
    longer lists it. Every other row keeps its row and curated data; only the registration
    (and its ownership / confidentiality overrides) or the source's provenance labels are
    removed. *)

val unregister_repo : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [unregister_repo ctx {"target"}] removes the [[[repo]]] entry of [target] (a URL, a
    key or a checkout directory) and returns [{key, row, exported}] where [row] is
    ["deleted"], ["kept"] or ["absent"]. An unregistered target is an error. *)

val unregister_source : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [unregister_source ctx {"kind", "name"?}] removes the matching [[[source]]] entry and
    the provenance labels only it wrote ({!Sources.labels}): rows left with no label, not
    registered and not curated are deleted, the others keep their row. Returns
    [{source, repos_removed, repos_kept, exported}]. An unknown source is an error. *)
