(** The documents store: [documents] and [doc_chunks] with their FTS index (spec FR-080,
    FR-084, FR-085, EC-15).

    Every text is redacted and made valid UTF-8 ({!Redact.text}) and capped at {!file_cap}
    bytes before it is hashed, stored and chunked ({!Doc_select.chunks}). A document whose
    stored [sha256] equals the new one is not re-chunked. [fetched_at] is the time its
    content last changed. Callers run these inside their own transaction. *)

type source = Local | Api

val source_name : source -> string
(** ["local"] or ["api"]. *)

type doc = {
  path : string;  (** Repository-relative path. *)
  kind : Doc_select.kind;
  source : source;
  text : string;  (** Raw text; {!write} prepares it. *)
  blob_sha : string option;  (** Forge blob id from the tree listing (API only). *)
}

val file_cap : int
(** [262144] bytes (256 KiB) per document, marker included. *)

val prepare : string -> string
(** [prepare s] is [s] redacted and sanitised, cut on a UTF-8 boundary with a marker when
    longer than {!file_cap}. *)

type change = New | Changed | Unchanged

val write : Db.t -> repo_id:int -> now:string -> doc -> change
(** [write db ~repo_id ~now d] upserts document [d.path] of the repository. A new or
    changed text replaces its chunks and sets [fetched_at = now]; an unchanged one only
    updates [kind], [source] and [blob_sha]. *)

val remove_except : Db.t -> repo_id:int -> string list -> int
(** [remove_except db ~repo_id keep] deletes the repository's documents whose path is not
    in [keep], with their chunks, and returns how many were deleted. *)

val purge : Db.t -> repo_id:int -> int
(** [purge db ~repo_id] deletes every document of the repository (EC-15). *)

val purge_key : Db.t -> string -> int
(** [purge_key db key] is {!purge} for the row whose key is [key] (EC-15 purge of a row
    that became a campaign row). *)

val purge_ineligible : Db.t -> int
(** [purge_ineligible db] deletes the documents of every repository that may not have any:
    campaign rows (AM-17), whitehat rows and rows whose tier is not ["1"] and that are not
    explicitly registered (FR-081, "Ownership kind"). *)

val blobs : Db.t -> repo_id:int -> (string * string) list
(** [blobs db ~repo_id] is the stored [(path, blob_sha)] of the repository's documents. *)

val count : Db.t -> repo_id:int -> int
(** [count db ~repo_id] is the number of documents of the repository. *)
