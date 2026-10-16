(** [resolve-library-id]: candidate libraries for a name (spec FR-086, FR-087, US-10).

    Candidates are the non-whitehat rows whose name, [owner/name], key or one of whose
    package [aliases] ({!Manifest.aliases}) equals [libraryName] (case-insensitive, a
    leading [/] ignored), plus the rows whose [owner/name] contains every word of
    [libraryName] (at most 25). Descriptions are never matched: a name that only appears
    in some description (say, a framework the repo uses) is "not in the index". Each
    candidate's [score] is [100] for an exact match plus the bm25 relevance (search
    weights) of the words of [libraryName] and [query] together.

    Ordering: candidates sharing a name form a group, ranked by its best score. Inside a
    group the maintained copy comes first ({!Copies}), then GitHub, GitLab, local, then
    score and key, so a GitHub repository ranks above its GitLab mirror. At most
    {!max_candidates} are returned. When nothing matches, the result carries the exact
    {!no_match_hint}. Nothing here contacts any network service. *)

val fallback_service : string
(** The external documentation service named in the hint. This is the only place in the
    code that names it (FR-096 scan). *)

val index_label : display_name:string -> string
(** ["<first word of display_name>'s index"], or ["the library index"] when the configured
    owner display name is empty. *)

val no_match_hint : ?display_name:string -> string -> string
(** [no_match_hint ~display_name name] is
    ["not in <index_label> — use <service> resolve-library-id with libraryName=<name>"]
    (US-10.2; with the owner's data config this reads "not in <First>'s index — ..."). *)

val max_candidates : int
(** [10]. *)

val resolve :
  Ctx.t ->
  library_name:string ->
  query:string option ->
  filters:Filters.t ->
  detail:Opts.detail ->
  (Yojson.Safe.t, string) result
(** [resolve ctx ~library_name ~query ~filters ~detail] is
    [{candidates: [{id, name, full_name, what, maintained_copy, score, doc_count, uri}],
     hint?}]. [id] is the index key (accepted by [query-docs]). [detail = Brief] keeps
    [id, name, maintained_copy, uri]; [Full] adds
    [host, tier, status, language, description, pushed_at]. A blank [library_name] is an
    error. *)
