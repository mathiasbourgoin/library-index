(** JSON views of repository rows for [get] (spec US-3.1, AM-10) and their parts shared
    with {!Explain}. *)

val bool_columns : string list
(** INTEGER columns rendered as JSON booleans ([null] when NULL). *)

val json_columns : string list
(** TEXT columns holding JSON documents, rendered parsed. *)

val column_json : string -> Sqlite3.Data.t -> Yojson.Safe.t
(** [column_json name v] renders one [repos] column value. *)

val effective_status : now:Ptime.t -> (string * Sqlite3.Data.t) list -> string
(** [effective_status ~now row] applies {!Status.effective} to a named row holding
    [status_override], [archived], [last_commit] and [pushed_at]. *)

val named : (string * Sqlite3.Data.t) list -> string -> string option
(** [named row k] is the text of column [k] of a named row, [None] for NULL or absent. *)

val usage_json : Db.t -> int -> Yojson.Safe.t list
(** [usage_json db repo_id] is the usage rows [{consumer, note, at}], oldest first. *)

val provenance_json : Db.t -> int -> (string * Sqlite3.Data.t) list -> Yojson.Safe.t
(** [provenance_json db repo_id row] maps each curated field holding a value to [auto] or
    [curated] (FR-112; no [field_meta] row: [curated]). *)

val get : Ctx.t -> string -> (Yojson.Safe.t, string) result
(** [get ctx ident] resolves [ident] ({!Resolve}) and returns every column (metadata,
    curated fields incl. [rules], README, synopsis, [fetched], [missing_since], …), plus
    [status] (effective), [usage] rows, [uri] ({!Liburi.repo}, [null] for whitehat),
    [documents] ([{path, kind, title, source, fetched_at, uri}], README first; the text is
    read through the document URI) and, while it is set, ["export_dirty": true]. *)
