(** Importing an arch-index comment_db into [api_items] / [api_calls] (spec "Optional
    arch-index analysis"; the output contract is epure's [COMMENT_INDEX_CONTRACT.md] and
    [comment_db.sql], and the flat schema arch-index's LSP runner actually writes).

    Read: [comment_db_meta.schema_version] ([1] or [1.x], else an error), [built_by] or
    [producer] for the tool version; [functions] ([name], [file_path] required;
    [line_start], [signature], [summary] when present), only the rows whose [exported] /
    [exposed] column is [1] when such a column exists; [calls], keyed by
    [caller_id]/[callee_id] (contract) or by [caller_name]/[caller_file]/[callee_name]
    (/[callee_file]) (flat schema). The external database is opened read-only and only
    through prepared statements. *)

type imported = {
  items : int;  (** Items written. *)
  calls : int;  (** Distinct call edges written between written items. *)
  total : int;  (** Rows of [functions], exported or not. *)
  edges : int;  (** Rows of [calls] read (the call graph is empty when [0]). *)
  tool : string;  (** ["<built_by or producer> schema <schema_version>"]. *)
}

val import :
  Ctx.t ->
  repo_id:int ->
  head:string option ->
  language:string ->
  string ->
  (imported, string) result
(** [import ctx ~repo_id ~head ~language path] reads the comment_db at [path]. When
    [functions] has at least one row, one transaction replaces the repository's
    [arch-index] items (kind [function], [analyzed_head = head], text redacted, signature
    and doc capped) and their call edges; with no row nothing is written (the previous
    items stay). An item's qualified path is its name when the name is qualified, else the
    file's module ([Itf] for [lib/itf.ml] in OCaml, the base name otherwise) and the name.
    [Error] for an unreadable or unsupported database. *)
