(** Fetching one repository's documents (spec FR-081, FR-082, FR-084). Does not touch the
    DB: the caller writes the result ({!Doc_refresh}).

    A local checkout is preferred: its tracked files come from [git ls-files -z] and are
    read from disk (regular files only, never through a symbolic link) on every refresh.
    Without a checkout, documents come from the forge through GET-only commands
    ({!Commands.gh_doc_tree} / {!Commands.gl_doc_tree} and the raw file commands), and
    only when they are due ({!api_due}). A file whose forge blob id equals the stored one
    is not fetched again (unless [full]).

    The files the API extractors select are read too, for the scanned API items
    ({!Api_store}), even when the documents are markdown: locally on every refresh, from
    the forge when their blob id changed (after the documents, in the same caps).

    Caps: {!Doc_select.max_files} files and {!max_repo_bytes} bytes per repository (for
    documents and for scanned files each), {!max_api_fetches} file fetches per repository
    and run (shared). Hitting a cap makes the result [capped] (and so not [complete]). The
    root manifests ({!Api_item.manifest_files}) are read on every local refresh and on
    every due API fetch (outside the fetch cap): they choose the API extractors and give
    the package aliases. A [`Not_found] answer (empty repository, missing file) means "no
    such document" and is not a failure; it removes nothing. *)

type target = {
  repo_id : int;
  key : string;
  host : string;  (** [github], [gitlab] or [local]. *)
  full_name : string;
  gl_id : int option;
  default_branch : string option;
  local_dir : string option;  (** An existing checkout directory, if any. *)
  pushed_at : string option;
  docs_pushed_at : string option;  (** [pushed_at] at the last complete API fetch. *)
  stored_blobs : (string * string) list;  (** {!Doc_store.blobs}. *)
  stored_scan_blobs : (string * string) list;  (** {!Api_store.blobs}. *)
  language : string option;  (** Primary language, for extractor detection. *)
}

type mode =
  | From_local
  | From_api
  | Not_due  (** API documents are current (pushed_at unchanged). *)
  | No_source  (** A [local:] row without a checkout. *)

val mode_name : mode -> string
(** ["local"], ["api"], ["not-due"] or ["no-source"]. *)

type scan_file = {
  path : string;
  text : string;  (** Raw text; {!Api_store.write_file} prepares it. *)
  blob_sha : string option;  (** Forge blob id (API only). *)
}
(** A file read for the scanned API items. *)

type result = {
  target : target;
  mode : mode;
  docs : Doc_store.doc list;  (** Documents read or fetched in this run. *)
  keep : string list option;
      (** The selected paths when the listing succeeded: stored documents outside it are
          removed. [None] when nothing may be removed. *)
  complete : bool;  (** Listing and every due fetch succeeded, no cap was hit. *)
  capped : bool;
  failures : (Commands.purpose * Proc.failure) list;
  aliases : string list option;
      (** Package names of the root manifests ({!Manifest.aliases}) when the listing and
          every manifest read succeeded; [None] keeps the stored ones. *)
  scan : (scan_file list * string list) option;
      (** The files read for the scanned API items ({!Doc_select.api_paths}, whatever the
          documentation gating) and every selected API path (scanned files outside it are
          removed); [None] when the listing failed. A local read takes them all; an API
          fetch takes those whose blob id changed, after the documents and within the same
          fetch and byte caps. *)
}

val max_repo_bytes : int
(** [4194304] bytes (4 MiB) per repository and run. *)

val max_api_fetches : int
(** [60] file fetches per repository and run. *)

val api_due : full:bool -> target -> bool
(** [api_due ~full t] is the README rule applied to documents (FR-084): [full], no
    complete API fetch yet, or a [pushed_at] that differs from [docs_pushed_at]. *)

val run : call:Fetch.call -> full:bool -> workers:int -> target -> result
(** [run ~call ~full ~workers t] lists and reads [t]'s documents. API file fetches run on
    at most [workers] threads. Never raises. *)
