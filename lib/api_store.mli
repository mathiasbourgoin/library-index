(** The API items store: [api_items], [api_files] and their FTS index (spec FR-120,
    "Optional arch-index analysis", US-12 [find-capability]).

    Items of source [scan] are written here from the files the per-language extractors
    select ({!Doc_select.api_paths}), for every repository eligible for documents (tier
    ["1"] or registered, not campaign, not whitehat), whatever the documentation gating. A
    file is re-extracted only when its prepared text ({!Doc_store.prepare}: redacted,
    sanitised, capped) has a new SHA-256. Items of source [arch-index] are written by
    {!Analysis}. Callers run these inside their own transaction. *)

type change = New | Changed | Unchanged

type row = {
  module_path : string;
  name : string;
  qualified_path : string;
  kind : string;
  signature : string option;  (** Capped ({!Api_item.cap}) and redacted on insert. *)
  doc : string option;  (** Capped and redacted on insert. *)
  file : string;
  line : int option;
}
(** One [api_items] row to insert. *)

val insert : Db.t -> repo_id:int -> source:string -> ?analyzed_head:string -> row -> int
(** [insert db ~repo_id ~source ?analyzed_head r] inserts one item ([source] is [scan] or
    [arch-index]; names, paths, signature and doc pass through {!Redact.text}) and returns
    its id. The only writer of [api_items]. *)

val delete_source : Db.t -> repo_id:int -> source:string -> int
(** [delete_source db ~repo_id ~source] deletes every API item of the repository that
    [source] wrote (e.g. ["arch-index"]), with its call edges (cascade), and returns the
    number deleted. Inside the caller's transaction. *)

val module_path : name:string -> string -> string
(** [module_path ~name qualified] is [qualified] without its trailing [name] and separator
    ([.] or [::]): ["Itf"] for ["Itf.parse_file"], ["crate::io"] for ["crate::io::read"];
    [""] when [qualified] is [name] or does not end with it. *)

val write_file :
  Db.t -> repo_id:int -> path:string -> text:string -> blob_sha:string option -> change
(** [write_file db ~repo_id ~path ~text ~blob_sha] extracts the [scan] items of one file
    when its text changed, replacing that file's previous [scan] items, and records the
    file in [api_files]. *)

val remove_except : Db.t -> repo_id:int -> string list -> int
(** [remove_except db ~repo_id keep] deletes the scanned files (and their [scan] items)
    whose path is not in [keep]; returns how many files were deleted. *)

val blobs : Db.t -> repo_id:int -> (string * string) list
(** [blobs db ~repo_id] is the stored [(path, blob_sha)] of the scanned files. *)

val purge_ineligible : Db.t -> int
(** [purge_ineligible db] deletes every API item (both sources) and scanned file of the
    rows that may not have documents (campaign, whitehat, neither tier ["1"] nor
    registered); returns the number of items deleted. *)

val counts : Db.t -> repo_id:int -> (string * int) list
(** [counts db ~repo_id] is the number of items per source, sources in name order. *)
