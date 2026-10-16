(** SQLite access: schema, statement helpers, transactions and the [meta] table.

    Every SQL value is bound as a parameter; no user data is interpolated into SQL. A
    failing statement raises {!Db_error} carrying only a classified message (the SQLite
    result-code name), never SQL text or the SQLite error message. *)

exception Db_error of string
(** Raised by the helpers below. The payload is safe to show to users. *)

type t = Sqlite3.db

val schema_version : int
(** Current schema version, stored as [meta.schema_version] (A1: no migrations). *)

val create : string -> t
(** [create path] opens (creating if needed) the file and configures it (WAL,
    [busy_timeout] 5000 ms, foreign keys). It does not create the schema. A configuration
    failure (read-only directory, held lock) closes the handle and raises {!Db_error}. *)

val open_existing : string -> (t, string) result
(** [open_existing path] opens an initialised DB, or an error telling the user to run
    [library-index init] when the file or schema is missing. A schema-3 file is migrated
    in place under [BEGIN IMMEDIATE], the version read again inside the transaction, so
    concurrent first opens all succeed or report a retryable "busy, retry" error (INV-M,
    FR-130). No error advises [init --force] while the file holds unexported curated data
    ([meta.export_dirty = 1]). *)

val busy_message : string
(** The payload of {!Db_error} for a busy or locked database. *)

val close : t -> unit
(** [close db] closes the handle, ignoring a busy close. *)

val create_schema : t -> unit
(** [create_schema db] creates every table, the FTS5 external-content table and its
    triggers, and seeds [meta.schema_version] and [meta.export_dirty]. *)

val exec : t -> string -> unit
(** [exec db sql] runs parameterless SQL (DDL, pragmas). *)

val run : t -> string -> Sqlite3.Data.t list -> unit
(** [run db sql params] executes one non-query statement with bound parameters. *)

val query : t -> string -> Sqlite3.Data.t list -> (Sqlite3.Data.t array -> 'a) -> 'a list
(** [query db sql params f] maps [f] over the result rows, in order. *)

val query_named :
  t -> string -> Sqlite3.Data.t list -> (string * Sqlite3.Data.t) list list
(** [query_named db sql params] returns each row as [(column name, value)] pairs. *)

val query_one :
  t -> string -> Sqlite3.Data.t list -> (Sqlite3.Data.t array -> 'a) -> 'a option
(** [query_one] is the first row of {!query}, if any. *)

val with_txn : t -> (unit -> ('a, 'e) result) -> ('a, 'e) result
(** [with_txn db f] runs [f] inside [BEGIN IMMEDIATE]. It commits on [Ok], rolls back on
    [Error] or on an exception (which is re-raised), and rolls back when the [COMMIT]
    itself fails (the exception is re-raised): the transaction is never left open. *)

val get_meta : t -> string -> string option
(** [get_meta db k] reads [meta.v] for key [k]. *)

val set_meta : t -> string -> string -> unit
(** [set_meta db k v] upserts [meta(k, v)]. [v] is stored verbatim: callers pass text that
    already went through {!Redact.text} where it is user-controlled. *)

val export_dirty : t -> bool
(** [export_dirty db] is [true] iff [meta.export_dirty = '1'] (AM-10). *)

(** {2 Value helpers} *)

val rtext : string -> Sqlite3.Data.t
(** [rtext s] is [s] redacted ({!Redact.text}) as a TEXT value: use it for every user- or
    network-originated text. *)

val rtext_opt : string option -> Sqlite3.Data.t
(** [rtext_opt] is {!rtext} or NULL. *)

val text : string -> Sqlite3.Data.t
(** [text s] binds [s] unredacted (keys, closed-vocabulary values, timestamps), made valid
    UTF-8 ({!Utf8.sanitize}). *)

val int : int -> Sqlite3.Data.t
(** [int n] is an INTEGER value. *)

val to_string_opt : Sqlite3.Data.t -> string option
(** [to_string_opt d] is the TEXT (or coerced number) of [d], [None] for NULL. *)

val to_int_opt : Sqlite3.Data.t -> int option
(** [to_int_opt d] is the INTEGER of [d], [None] for NULL or non-integers. *)

val to_bool : Sqlite3.Data.t -> bool
(** [to_bool d] is [true] iff [d] is a non-zero integer. *)
