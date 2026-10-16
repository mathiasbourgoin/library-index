(** Ranked document excerpts (spec FR-086, FR-088, FR-089, US-10.4, US-11).

    Chunks are ranked by [score = -bm25(doc_chunks_fts, 3, 1)] (the chunk title weighs
    three times its text); ties go to kind [doc], then [readme], then [api], then path and
    chunk order. As for repository search, the AND query runs first and the OR query only
    when AND matches nothing and there are at least two tokens ({!Query}). *)

val default_limit : int
(** [5] excerpts. *)

val max_limit : int
(** [20] excerpts. *)

val counts : Db.t -> (int, int) Hashtbl.t
(** [counts db] maps a repository id to its number of documents (absent = 0). *)

val best_excerpt :
  Db.t ->
  repo_id:int ->
  repo_uri:(string -> string option) ->
  fts:string ->
  kind:string option ->
  Yojson.Safe.t option
(** [best_excerpt db ~repo_id ~repo_uri ~fts ~kind] is the best matching chunk of the
    repository for the FTS expression [fts] as [{title, path, kind, snippet, uri}], or
    [None] when no chunk matches. [repo_uri path] builds the document URI. *)

val resolve_library : Db.t -> string -> (int * string, string) result
(** [resolve_library db id] accepts an index key, [/owner/name], [owner/name], a bare name
    or a [libindex://repo/...] URI, and returns the row id and key. Unknown or ambiguous
    ids are errors that name [resolve-library-id]. *)

val query :
  Ctx.t ->
  library:string ->
  query:string ->
  limit:int option ->
  kind:string option ->
  detail:Opts.detail ->
  filters:Filters.t ->
  (Yojson.Safe.t, string) result
(** [query ctx ~library ~query ~limit ~kind ~detail ~filters] is
    [{library: {id, name, uri}, rules?, excerpts: [...], hint?}] with excerpts from that
    library only. [rules] (the curated usage rules, FR-088) comes first when set. An
    excerpt is [{title, path, kind, source, snippet, score, uri}] ([detail = Brief]:
    [{title, path, uri}]; [Full]: [text] instead of [snippet]). [hint] explains an empty
    result: the library has no documents, or the filters exclude it. [limit] defaults to
    {!default_limit}, clamped to [1..max_limit]. *)
