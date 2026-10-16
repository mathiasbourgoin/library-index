(** Ranked full-text search (spec US-2, FR-010..014, C-7, C-9).

    Ranking: [score = -bm25(repos_fts, 10, 8, 10, 6, 4, 4, 3, 2, 2, 2, 1)] over the
    columns of {!Schema_sql.fts_columns} (higher is better); ties go to tier ["1"] first,
    then [pushed_at] descending, then [key]. The AND query runs first; the OR query only
    when AND yields no hit (after filters) and there are at least two tokens. *)

val weights : float list
(** The bm25 column weights, in {!Schema_sql.fts_columns} order. *)

val default_limit : int
(** 10. *)

val max_limit : int
(** 50. *)

val search :
  ?detail:Opts.detail ->
  ?kind:string ->
  ?budget:int ->
  Ctx.t ->
  query:string ->
  limit:int option ->
  filters:Filters.t ->
  Yojson.Safe.t
(** [search ?detail ?kind ?budget ctx ~query ~limit ~filters] is
    [{"query", "mode": "and"|"or"|"none", "count", "hits": [...]}], each hit carrying
    [key, full_name, host, private, tier, status, what, url, score, snippet, uri] and,
    when one of the repository's document chunks matches the same FTS expression, an
    [excerpt] [{title, path, kind, snippet, uri}] (FR-089; only chunks of kind [kind] when
    given). [what] falls back to synopsis then description; [uri] is [null] for whitehat
    rows. [limit] is clamped to [1..50].

    Optional (US-11): [detail = Brief] keeps [key, full_name, what, score, uri]; [Full]
    adds [language, domain, pushed_at, last_commit, doc_count]. [budget] applies
    {!Opts.apply_budget} to [hits]. [filters.maintained_only] and [filters.has_docs]
    restrict the hits ({!Filters}). Without these, the result is the core one. *)
