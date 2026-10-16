(** [find-capability]: which library already does this, and with which item (spec US-12,
    FR-091, AC-28).

    Two sources are ranked together, across every repository eligible for documents (tier
    ["1"] or registered, not campaign, not whitehat):
    - the API items ([api_items_fts], [score = -bm25] with weights name 4, module path 2,
      signature 1, doc 1), of source [scan] or [arch-index];
    - the document chunks of kind [doc] and [api] ([doc_chunks_fts], title 3, text 1); an
      [api] chunk is left out when its file already has API items (the same item twice).

    Results are ordered by score (higher is better), then library key, file path, line and
    qualified path, so the order is deterministic. As for search, the AND query runs first
    and the OR query only when AND matches nothing and there are at least two tokens
    ({!Query}). Filters apply to the library ({!Filters}; [status] and [maintained_only]
    after the SQL query). No network. *)

val default_limit : int
(** [10] results. *)

val max_limit : int
(** [50] results. *)

val find :
  Ctx.t ->
  query:string ->
  limit:int option ->
  filters:Filters.t ->
  detail:Opts.detail ->
  Yojson.Safe.t
(** [find ctx ~query ~limit ~filters ~detail] is [{query, mode, count, results}], each
    result
    [{library: {id, name, what, uri}, item: {kind, qualified_path, signature, doc_excerpt,
     file, line, source}, score, uri}]. [source] is [scan], [arch-index] or [doc] (a
    markdown or interface chunk: [kind] is [doc] or the item keyword of its title,
    [signature] and [line] are [null]). [doc_excerpt] is the doc cut to 300 characters
    (the chunk's snippet for a chunk); [detail = Full] gives the whole doc (chunk text);
    [Brief] drops [signature] and [doc_excerpt]. [uri] is the URI of the document holding
    the item when that file is a stored document, else [null]. *)
