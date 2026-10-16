(** Document ingestion inside a refresh (spec FR-081..FR-085, US-9).

    Eligible rows are fetched (non-missing) repositories of tier ["1"] or explicitly
    registered ([registered = 1], any ownership) that are not campaign rows (AM-17) and
    not whitehat rows. Documents of rows that are not eligible any more (campaign,
    whitehat, neither tier ["1"] nor registered) are deleted first, with their API items.
    A fetch that read the root manifests also stores the repository's package [aliases]
    ({!Manifest}). Each eligible row is fetched by {!Doc_fetch.run} on the worker pool,
    and its writes run on the calling thread in one transaction: changed documents,
    removal of documents no longer selected, the scanned API files and their [scan] items
    ({!Api_store}, every file the extractors select, whatever the documentation gating),
    and [docs_pushed_at] (set to [pushed_at] after a complete API fetch, cleared after a
    local read). *)

type summary = {
  repos : int;  (** Eligible rows considered. *)
  local : int;  (** Rows read from a local checkout. *)
  api : int;  (** Rows whose documents were fetched from the forge. *)
  not_due : int;  (** Rows whose API documents were current. *)
  written : int;  (** Documents new or changed (re-chunked). *)
  unchanged : int;  (** Documents read again with the same content hash. *)
  removed : int;  (** Documents deleted (no longer selected, or ineligible row). *)
  scanned : int;  (** API files (re-)extracted into [scan] items ({!Api_store}). *)
  gaps : string list;  (** Classified failure lines. *)
}

val summary_json : summary -> Yojson.Safe.t
(** [summary_json s] is the summary as a JSON object. *)

val local_dir : Yojson.Safe.t -> string option
(** [local_dir local_paths] is the first entry of a [local_paths] JSON array that is an
    existing directory. *)

val run :
  Db.t ->
  call:Fetch.call ->
  now:Ptime.t ->
  full:bool ->
  workers:int ->
  file_workers:int ->
  ?key:string ->
  unit ->
  summary
(** [run db ~call ~now ~full ~workers ~file_workers ?key ()] ingests the documents of
    every eligible row (only of [key] when given). Repositories run on [workers] threads,
    API file fetches of one repository on [file_workers] threads. A failure keeps the
    repository's previous documents and adds a gap line
    ["<key>: docs <purpose> failed (<class>); previous documents kept"]. Never raises: a
    DB error becomes a gap line. *)
