(** DDL of schema version 3 (brief "Schema guidance", spec FR-001, C-36, FR-080, FR-112,
    "Ownership kind", "Optional arch-index analysis"). No migration: a database of another
    version is refused by {!Db.open_existing}. *)

val version : int
(** Schema version written to [meta.schema_version]. *)

val fts_columns : string list
(** FTS5 columns in C-7 weight order: name, full_name, aliases (manifest package names,
    weighted like name), what, synopsis, description, interest, effective_domain,
    consumers, notes, readme. [effective_domain] is the generated column
    [coalesce(domain, computed_domain)]. *)

val statements : string list
(** The DDL statements, in execution order: [repos] (stable [id INTEGER PRIMARY KEY],
    [readme_sha256], fetched, curated and whitehat columns, [aliases], [registered],
    [ownership_override], [exclude_from_page], generated [effective_domain] and
    [ownership]), [usage], [meta], [local_checkouts], [gaps], [field_meta], the API items
    ([api_items] with its external-content [api_items_fts] over name, module path,
    signature and doc and its triggers, [api_files], [api_calls]) and [analysis] (version
    3), the documents store, the external-content [repos_fts] table with its AFTER
    INSERT/DELETE/UPDATE triggers, and the meta seed rows. *)
